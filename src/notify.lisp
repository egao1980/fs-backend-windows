(in-package #:fs-backend-windows)

;;; R4b-3: the pure half of the :RDCW journal. Nothing in this file calls
;;; Win32, so all of it runs, and is tested, on every OS:
;;;
;;;   octets ──PARSE-FILE-NOTIFY-INFORMATION──▶ ((action . native-name) …)
;;;          ──RDCW-ACCUMULATE──▶ pending relative names, incomplete / lost marks
;;;          ──RDCW-ACCUMULATOR-TAKE──▶ (values names complete-p)
;;;          ──RDCW-RESOLVE-PATHS (lister)──▶ on-disk relative paths for the drain
;;;
;;; plus the RDCW-BARRIER handshake between JOURNAL-DRAIN and the watcher
;;; thread, and RDCW-SAME-FINAL-PATH-P for the root check.

;;; --- FILE_NOTIFY_INFORMATION ---------------------------------------------------------

(declaim (inline %u16 %u32))
(defun %u16 (octets i)
  "The little-endian 16-bit value at OCTETS[I]. AREF, so any vector works."
  (logior (aref octets i) (ash (aref octets (1+ i)) 8)))

(defun %u32 (octets i)
  "The little-endian 32-bit value at OCTETS[I]."
  (logior (%u16 octets i) (ash (%u16 octets (+ i 2)) 16)))

(defun utf16le-octets-to-string (octets &key (start 0) end)
  "Decode the UTF-16LE octets of OCTETS (a vector of (unsigned-byte 8)) in
[START, END) to a string; END NIL means the end of OCTETS. Surrogate pairs
give one character above U+FFFF. Returns NIL when the range is ill-formed:
an odd number of octets, or an unpaired surrogate (NTFS allows those in
names; they cannot become a UTF-8 path)."
  (let ((end (or end (length octets))))
    (when (oddp (- end start))
      (return-from utf16le-octets-to-string nil))
    (let ((out (make-string-output-stream))
          (i start))
      (loop while (< i end)
            do (let ((u (%u16 octets i)))
                 (incf i 2)
                 (cond ((<= #xD800 u #xDBFF)
                        (unless (< i end)
                          (return-from utf16le-octets-to-string nil))
                        (let ((v (%u16 octets i)))
                          (unless (<= #xDC00 v #xDFFF)
                            (return-from utf16le-octets-to-string nil))
                          (incf i 2)
                          (write-char (code-char (+ #x10000
                                                    (ash (- u #xD800) 10)
                                                    (- v #xDC00)))
                                      out)))
                       ((<= #xDC00 u #xDFFF)
                        (return-from utf16le-octets-to-string nil))
                       (t (write-char (code-char u) out)))))
      (get-output-stream-string out))))

(defun parse-file-notify-information (octets &key (start 0) end)
  "Parse the chain of FILE_NOTIFY_INFORMATION entries in OCTETS (a vector of
(unsigned-byte 8), little-endian) from START to END (NIL = the end of
OCTETS; a completion passes the bytes-returned count). → a list, in buffer
order, of (ACTION . NAME): ACTION the raw DWORD (FILE_ACTION_*, unknown values
passed through), NAME the decoded FileName (native, backslash-separated, as
reported), or NIL when it is not well-formed UTF-16 (see
UTF16LE-OCTETS-TO-STRING). An empty range gives NIL.
Each entry is NextEntryOffset (0 for the last), Action, FileNameLength (in
octets), FileName. Signals MALFORMED-NOTIFY-BUFFER when the range is not
such a chain: a header or a name past END, an odd FileNameLength, a
NextEntryOffset that is not a multiple of 4, or that does not move past the
current entry's name."
  (let ((end (or end (length octets)))
        (header +file-notify-information-header-size+)
        (offset start)
        (out '()))
    (when (>= start end)
      (return-from parse-file-notify-information nil))
    (flet ((malformed (reason)
             (error 'malformed-notify-buffer :offset offset :reason reason)))
      (loop
        (when (> (+ offset header) end)
          (malformed "entry header past the end of the buffer"))
        (let* ((next (%u32 octets offset))
               (action (%u32 octets (+ offset 4)))
               (length (%u32 octets (+ offset 8)))
               (name-start (+ offset header))
               (name-end (+ name-start length)))
          (when (oddp length)
            (malformed "odd FileNameLength"))
          (when (> name-end end)
            (malformed "FileName past the end of the buffer"))
          (push (cons action (utf16le-octets-to-string octets :start name-start :end name-end))
                out)
          (when (zerop next)
            (return (nreverse out)))
          (unless (zerop (mod next 4))
            (malformed "NextEntryOffset not a multiple of 4"))
          (when (< next (+ header length))
            (malformed "NextEntryOffset inside the current entry"))
          (when (> (+ offset next header) end)
            (malformed "NextEntryOffset past the end of the buffer"))
          (incf offset next))))))

(defun rdcw-action-keyword (action)
  "The keyword of the FILE_ACTION_* integer ACTION: :ADDED, :REMOVED,
:MODIFIED, :RENAMED-OLD, :RENAMED-NEW; NIL for any other value."
  (case action                          ; the +FILE-ACTION-*+ values
    (1 :added)
    (2 :removed)
    (3 :modified)
    (4 :renamed-old)
    (5 :renamed-new)
    (t nil)))

;;; --- names and paths ---------------------------------------------------------------

(defun %split-components (name)
  "The non-empty components of NAME, split at \"\\\" and \"/\"."
  (let ((components '())
        (start 0)
        (n (length name)))
    (dotimes (i (1+ n) (nreverse components))
      (when (or (= i n) (member (char name i) '(#\\ #\/)))
        (when (> i start)
          (push (subseq name start i) components))
        (setf start (1+ i))))))

(defun %join-components (components)
  (format nil "~{~A~^/~}" components))

(defun %join-relative (directory rest)
  "DIRECTORY (relative POSIX, \"\" for the root) joined with REST."
  (if (string= directory "") rest (concatenate 'string directory "/" rest)))

(defun rdcw-relative-path (native-name)
  "The POSIX relative path of the native relative NATIVE-NAME reported by
ReadDirectoryChangesW: backslashes (and slashes) become \"/\", runs of
separators collapse, leading and trailing separators are dropped. \"\" for
the root itself. The spelling of each component is kept."
  (%join-components (%split-components native-name)))

(defun rdcw-fold-key (string)
  "The case-folded key of STRING for comparing Windows names: each character
through CHAR-UPCASE (simple one-to-one uppercase, as NTFS's $UpCase table;
no length-changing folds, so \"ß\" stays \"ß\"). A fresh string."
  (map 'string #'char-upcase string))

(defun rdcw-short-name-p (component)
  "True when the path component COMPONENT has the form of a generated 8.3
alias: a base of at most 8 characters ending in \"~\" and one or more
digits, with at least one character before the \"~\" and no \"~\" or \".\"
in it, then optionally \".\" and an extension of 1 to 3 characters with no
\".\". Case is ignored. Examples: \"PROGRA~1\", \"LONGFI~1.TXT\",
\"ab~12.c\"; not \"~1\", \"a~b\", \"ABCDEFG~1\", \"x~1.html\"."
  (let* ((dot (position #\. component))
         (base (subseq component 0 (or dot (length component))))
         (extension (and dot (subseq component (1+ dot))))
         (tilde (position #\~ base)))
    (and (<= 1 (length base) 8)
         tilde
         (plusp tilde)
         (< (1+ tilde) (length base))
         ;; Not DIGIT-CHAR-P: it accepts non-ASCII digits on some Lisps.
         (every (lambda (c) (char<= #\0 c #\9)) (subseq base (1+ tilde)))
         (or (null extension)
             (and (<= 1 (length extension) 3)
                  (not (find #\. extension))))
         t)))

(defun %match-component (component entries)
  "The long name in ENTRIES ((LONG . SHORT-OR-NIL) …) that COMPONENT names:
exact, else the unique long-name fold, else the unique short-name fold.
:AMBIGUOUS when several fold to it, NIL when none does."
  (if (find component entries :key #'car :test #'string=)
      component
      (let* ((key (rdcw-fold-key component))
             (by-long (remove-if-not (lambda (e) (string= key (rdcw-fold-key (car e))))
                                     entries)))
        (if by-long
            (if (rest by-long) :ambiguous (car (first by-long)))
            (let ((by-short (remove-if-not (lambda (e)
                                             (and (cdr e)
                                                  (string= key (rdcw-fold-key (cdr e)))))
                                           entries)))
              (cond ((null by-short) nil)
                    ((rest by-short) :ambiguous)
                    (t (car (first by-short)))))))))

(defun %memoized-lister (lister)
  "LISTER called at most once per directory (NIL listings included)."
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (directory)
      (multiple-value-bind (entries found) (gethash directory cache)
        (if found
            entries
            (setf (gethash directory cache) (funcall lister directory)))))))

(defun %resolve-one (name lister)
  "The RESOLVED path of NAME (see RDCW-RESOLVE-PATH); LISTER is memoized."
  (let ((directory ""))
    (loop for (component . rest) on (%split-components name)
          for match = (%match-component component (funcall lister directory))
          do (if (stringp match)
                 (setf directory (%join-relative directory match))
                 (return-from %resolve-one
                   (if (or (eq match :ambiguous) (rdcw-short-name-p component))
                       directory
                       (%join-relative directory (%join-components (cons component rest)))))))
    directory))

(defun %resolved-and-reported (name lister)
  (let ((resolved (%resolve-one name lister)))
    (if (string= resolved name) (list resolved) (list resolved name))))

(defun rdcw-resolve-path (name lister)
  "Map the reported relative POSIX path NAME to what the drain reports, as
a list of one or two relative POSIX paths: first the RESOLVED path (on-disk
spelling), then NAME itself when it is not STRING= to it.
LISTER is a function of a relative directory path (\"\" for the root, in
on-disk spelling) returning that directory's entries as a list of
(LONG-NAME . SHORT-NAME-OR-NIL), or NIL when it is missing or unreadable.
Walk NAME's components from the root; at each, in the current directory's
listing:
  - an entry whose long name is STRING= to the component: take it;
  - else exactly one entry whose long name has the same RDCW-FOLD-KEY: take
    its long name (case correction);
  - else exactly one entry whose short name has the same RDCW-FOLD-KEY: take
    its long name (8.3 expansion);
  - else (missing, or ambiguous: several entries fold to it) stop. RESOLVED
    is then the resolved ancestor directory (\"\" for the root) when the
    component is ambiguous or RDCW-SHORT-NAME-P (a deleted 8.3 alias has no
    knowable long name, so its whole directory is reported), else the
    resolved ancestor joined with the rest of NAME as reported.
Reporting NAME too is always safe (a path that is not on disk only drops
state) and it keeps both names of a case-only rename."
  (%resolved-and-reported name (%memoized-lister lister)))

(defun rdcw-resolve-paths (names lister)
  "RDCW-RESOLVE-PATH over the list NAMES, as one deduplicated list sorted
STRING<. LISTER (as for RDCW-RESOLVE-PATH) is called at most once per
directory per call, so names that fold together (\"README.md\",
\"readme.md\") and siblings share one listing."
  (let ((lister (%memoized-lister lister)))
    (sort (remove-duplicates (loop for name in names
                                   append (%resolved-and-reported name lister))
                             :test #'string=)
          #'string<)))

(defun %final-path-key (path)
  (let ((s (substitute #\\ #\/ path)))
    (cond ((and (>= (length s) 8) (string-equal "\\\\?\\UNC\\" s :end2 8))
           (setf s (concatenate 'string "\\\\" (subseq s 8))))
          ((and (>= (length s) 4) (string= "\\\\?\\" s :end2 4))
           (setf s (subseq s 4))))
    (let ((trimmed (string-right-trim "\\" s)))
      (rdcw-fold-key (if (string= trimmed "") s trimmed)))))

(defun rdcw-same-final-path-p (a b)
  "True when the Win32 paths A and B (strings, as GetFinalPathNameByHandleW
returns them or as the user spelled them) name the same location: a
\"\\\\?\\\" prefix is ignored, \"\\\\?\\UNC\\server\\share\" is
\"\\\\server\\share\", \"/\" is \"\\\", trailing separators are ignored, and
the comparison is by RDCW-FOLD-KEY."
  (string= (%final-path-key a) (%final-path-key b)))

;;; --- the accumulator -----------------------------------------------------------------

(defclass rdcw-accumulator ()
  ((lock :initform (bt2:make-lock :name "rdcw accumulator") :reader %accumulator-lock)
   (names :initform (make-hash-table :test #'equal) :reader %accumulator-names
          :documentation "Pending relative names (keys), exact spellings.")
   (incomplete :initform nil :accessor %accumulator-incomplete)
   (lost :initform nil :accessor %accumulator-lost))
  (:documentation "What a journal has seen since its last drain: the pending
relative names (exact spellings, each once), an INCOMPLETE mark (cleared by
a take) and a LOST mark (sticky: the root is gone). Thread-safe: every
operation takes the accumulator's own lock, so the watcher thread
accumulates while another thread takes. The implementation adds the slots
it needs."))

(defun make-rdcw-accumulator ()
  "A fresh, empty RDCW-ACCUMULATOR: no names, no marks."
  (make-instance 'rdcw-accumulator))

(defun %completion-paths (octets bytes)
  "→ (values paths incomplete-p) of a successful completion's buffer."
  (handler-case
      (let ((incomplete nil)
            (paths '()))
        (loop for (action . name) in (parse-file-notify-information
                                      octets :end (min bytes (length octets)))
              for relative = (and name (rdcw-relative-path name))
              do (if (or (null (rdcw-action-keyword action))
                         (null relative)
                         (string= relative ""))
                     (setf incomplete t)
                     (push relative paths)))
        (values paths incomplete))
    (malformed-notify-buffer ()
      (values nil t))))

(defun rdcw-accumulate (accumulator octets &key (bytes (length octets)) (code 0))
  "Apply one ReadDirectoryChangesW completion to ACCUMULATOR. OCTETS is the
buffer, BYTES the bytes-returned count (only OCTETS[0, BYTES) is read),
CODE the completion's Win32 error code (GetOverlappedResult's GetLastError, 0 on success). Returns NIL. By CODE:
  - ERROR_OPERATION_ABORTED (the journal is closing): no change;
  - ERROR_NOTIFY_ENUM_DIR, or ERROR_SUCCESS with BYTES 0 (the buffer
    overflowed): INCOMPLETE;
  - any other non-zero code (ERROR_ACCESS_DENIED when the root is deleted,
    ERROR_NOTIFY_CLEANUP, ...): LOST;
  - ERROR_SUCCESS: each parsed entry adds (RDCW-RELATIVE-PATH name), for
    every action including both names of a rename (pairs need no matching:
    each name is a touched path). INCOMPLETE instead for a malformed buffer
    (MALFORMED-NOTIFY-BUFFER never escapes), an unknown action, a NIL name,
    or a name whose relative path is \"\" (the root itself)."
  (check-type code integer)
  (check-type bytes (integer 0))
  (unless (= code +error-operation-aborted+)
    ;; Parse outside the lock; mutate inside it.
    (multiple-value-bind (paths incomplete lost)
        (cond ((or (= code +error-notify-enum-dir+)
                   (and (= code +error-success+) (zerop bytes)))
               (values nil t nil))
              ((/= code +error-success+)
               (values nil nil t))
              (t (%completion-paths octets bytes)))
      (let ((names (%accumulator-names accumulator)))
        (bt2:with-lock-held ((%accumulator-lock accumulator))
          (cond (lost
                 (setf (%accumulator-lost accumulator) t)
                 (clrhash names))
                (incomplete
                 (setf (%accumulator-incomplete accumulator) t)
                 (clrhash names))
                ((or (%accumulator-incomplete accumulator) (%accumulator-lost accumulator)))
                (t (dolist (p paths)
                     (setf (gethash p names) t))))))))
  nil)

(defun rdcw-accumulator-take (accumulator)
  "Take what ACCUMULATOR holds: → (values names complete-p). LOST: (values
NIL NIL), and it stays LOST. INCOMPLETE: (values NIL NIL), and the names and
the mark are cleared. Otherwise (values names T) with NAMES the pending
names sorted STRING<, which are cleared."
  (let ((names (%accumulator-names accumulator)))
    (values-list
     (bt2:with-lock-held ((%accumulator-lock accumulator))
       (cond ((%accumulator-lost accumulator)
              (list nil nil))
             ((%accumulator-incomplete accumulator)
              (setf (%accumulator-incomplete accumulator) nil)
              (clrhash names)
              (list nil nil))
             (t (let ((taken (loop for k being the hash-keys of names collect k)))
                  (clrhash names)
                  (list (sort taken #'string<) t))))))))

(defun rdcw-accumulator-mark-lost (accumulator)
  "Mark ACCUMULATOR LOST for good (the root was deleted or moved). → NIL."
  (bt2:with-lock-held ((%accumulator-lock accumulator))
    (setf (%accumulator-lost accumulator) t)
    (clrhash (%accumulator-names accumulator)))
  nil)

(defun rdcw-accumulator-lost-p (accumulator)
  "True once ACCUMULATOR is LOST."
  (bt2:with-lock-held ((%accumulator-lock accumulator))
    (%accumulator-lost accumulator)))

;;; --- the drain barrier ---------------------------------------------------------------
;;;
;;; JOURNAL-DRAIN (any thread) and the watcher thread meet here. The drain
;;; takes a ticket and wakes the watcher; the watcher reads
;;; RDCW-BARRIER-REQUESTED, then handles every completed ReadDirectoryChangesW
;;; and re-issues it until the new request is pending and unsignalled (the
;;; kernel then holds no undelivered event), then acks what it read. A ticket
;;; acked means every event of a write that returned before the ticket was
;;; taken is in the accumulator.

(defclass rdcw-barrier ()
  ((lock :initform (bt2:make-lock :name "rdcw barrier") :reader %barrier-lock)
   (cv :initform (bt2:make-condition-variable :name "rdcw barrier") :reader %barrier-cv)
   (requested :initform 0 :accessor %barrier-requested)
   (acked :initform 0 :accessor %barrier-acked)
   (failed :initform nil :accessor %barrier-failed))
  (:documentation "Tickets requested by drains and acked by the watcher,
with a lock and a condition variable (bordeaux-threads). Thread-safe. The
implementation adds the slots it needs."))

(defun make-rdcw-barrier ()
  "A fresh RDCW-BARRIER: no ticket requested or acked, not failed."
  (make-instance 'rdcw-barrier))

(defun rdcw-barrier-request (barrier)
  "Take a new ticket: → a positive integer, larger than every ticket BARRIER
gave before."
  (bt2:with-lock-held ((%barrier-lock barrier))
    (incf (%barrier-requested barrier))))

(defun rdcw-barrier-requested (barrier)
  "The largest ticket BARRIER has given, 0 before the first."
  (bt2:with-lock-held ((%barrier-lock barrier))
    (%barrier-requested barrier)))

(defun rdcw-barrier-ack (barrier ticket)
  "Answer every ticket up to TICKET (an integer) and wake the waiters. An
ack below an earlier one changes nothing. → NIL."
  (check-type ticket integer)
  (bt2:with-lock-held ((%barrier-lock barrier))
    (setf (%barrier-acked barrier) (max (%barrier-acked barrier) ticket))
    (bt2:condition-broadcast (%barrier-cv barrier)))
  nil)

(defun rdcw-barrier-wait (barrier ticket timeout)
  "Wait at most TIMEOUT seconds (a non-negative real; 0 does not block) for
TICKET to be acked. → T when it is (also when it was before the call), NIL
on timeout or once BARRIER has failed."
  (check-type timeout (real 0))
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second))))
        (lock (%barrier-lock barrier)))
    (bt2:with-lock-held (lock)
      (loop
        (cond ((>= (%barrier-acked barrier) ticket) (return t))
              ((%barrier-failed barrier) (return nil)))
        (let ((left (/ (- deadline (get-internal-real-time))
                       internal-time-units-per-second)))
          (when (<= left 0)
            (return nil))
          ;; Re-checked on every wakeup (spurious ones included).
          (bt2:condition-wait (%barrier-cv barrier) lock :timeout left))))))

(defun rdcw-barrier-fail (barrier)
  "Fail BARRIER for good (the watcher is gone or the journal is closing):
current and later RDCW-BARRIER-WAITs for tickets not yet acked return NIL
at once. → NIL."
  (bt2:with-lock-held ((%barrier-lock barrier))
    (setf (%barrier-failed barrier) t)
    (bt2:condition-broadcast (%barrier-cv barrier)))
  nil)
