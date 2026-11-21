(in-package #:fs-backend-windows)

;;; Contract stubs: every call of %R4B3-PENDING is replaced by the
;;; implementation, and then %R4B3-PENDING itself is deleted.
(defun %r4b3-pending (what)
  (error "R4b-3 not implemented yet: ~A" what))

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

(defun utf16le-octets-to-string (octets &key (start 0) end)
  "Decode the UTF-16LE octets of OCTETS (a vector of (unsigned-byte 8)) in
[START, END) to a string; END NIL means the end of OCTETS. Surrogate pairs
give one character above U+FFFF. Returns NIL when the range is ill-formed:
an odd number of octets, or an unpaired surrogate (NTFS allows those in
names; they cannot become a UTF-8 path)."
  (declare (ignore octets start end))
  (%r4b3-pending 'utf16le-octets-to-string))

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
  (declare (ignore octets start end))
  (%r4b3-pending 'parse-file-notify-information))

(defun rdcw-action-keyword (action)
  "The keyword of the FILE_ACTION_* integer ACTION: :ADDED, :REMOVED,
:MODIFIED, :RENAMED-OLD, :RENAMED-NEW; NIL for any other value."
  (declare (ignore action))
  (%r4b3-pending 'rdcw-action-keyword))

;;; --- names and paths ---------------------------------------------------------------

(defun rdcw-relative-path (native-name)
  "The POSIX relative path of the native relative NATIVE-NAME reported by
ReadDirectoryChangesW: backslashes (and slashes) become \"/\", runs of
separators collapse, leading and trailing separators are dropped. \"\" for
the root itself. The spelling of each component is kept."
  (declare (ignore native-name))
  (%r4b3-pending 'rdcw-relative-path))

(defun rdcw-fold-key (string)
  "The case-folded key of STRING for comparing Windows names: each character
through CHAR-UPCASE (simple one-to-one uppercase, as NTFS's $UpCase table;
no length-changing folds, so \"ß\" stays \"ß\"). A fresh string."
  (declare (ignore string))
  (%r4b3-pending 'rdcw-fold-key))

(defun rdcw-short-name-p (component)
  "True when the path component COMPONENT has the form of a generated 8.3
alias: a base of at most 8 characters ending in \"~\" and one or more
digits, with at least one character before the \"~\" and no \"~\" or \".\"
in it, then optionally \".\" and an extension of 1 to 3 characters with no
\".\". Case is ignored. Examples: \"PROGRA~1\", \"LONGFI~1.TXT\",
\"ab~12.c\"; not \"~1\", \"a~b\", \"ABCDEFG~1\", \"x~1.html\"."
  (declare (ignore component))
  (%r4b3-pending 'rdcw-short-name-p))

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
  (declare (ignore name lister))
  (%r4b3-pending 'rdcw-resolve-path))

(defun rdcw-resolve-paths (names lister)
  "RDCW-RESOLVE-PATH over the list NAMES, as one deduplicated list sorted
STRING<. LISTER (as for RDCW-RESOLVE-PATH) is called at most once per
directory per call, so names that fold together (\"README.md\",
\"readme.md\") and siblings share one listing."
  (declare (ignore names lister))
  (%r4b3-pending 'rdcw-resolve-paths))

(defun rdcw-same-final-path-p (a b)
  "True when the Win32 paths A and B (strings, as GetFinalPathNameByHandleW
returns them or as the user spelled them) name the same location: a
\"\\\\?\\\" prefix is ignored, \"\\\\?\\UNC\\server\\share\" is
\"\\\\server\\share\", \"/\" is \"\\\", trailing separators are ignored, and
the comparison is by RDCW-FOLD-KEY."
  (declare (ignore a b))
  (%r4b3-pending 'rdcw-same-final-path-p))

;;; --- the accumulator -----------------------------------------------------------------

(defclass rdcw-accumulator ()
  ()
  (:documentation "What a journal has seen since its last drain: the pending
relative names (exact spellings, each once), an INCOMPLETE mark (cleared by
a take) and a LOST mark (sticky: the root is gone). Thread-safe: every
operation takes the accumulator's own lock, so the watcher thread
accumulates while another thread takes. The implementation adds the slots
it needs."))

(defun make-rdcw-accumulator ()
  "A fresh, empty RDCW-ACCUMULATOR: no names, no marks."
  (%r4b3-pending 'make-rdcw-accumulator))

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
  (declare (ignorable accumulator octets bytes code))
  (%r4b3-pending 'rdcw-accumulate))

(defun rdcw-accumulator-take (accumulator)
  "Take what ACCUMULATOR holds: → (values names complete-p). LOST: (values
NIL NIL), and it stays LOST. INCOMPLETE: (values NIL NIL), and the names and
the mark are cleared. Otherwise (values names T) with NAMES the pending
names sorted STRING<, which are cleared."
  (declare (ignore accumulator))
  (%r4b3-pending 'rdcw-accumulator-take))

(defun rdcw-accumulator-mark-lost (accumulator)
  "Mark ACCUMULATOR LOST for good (the root was deleted or moved). → NIL."
  (declare (ignore accumulator))
  (%r4b3-pending 'rdcw-accumulator-mark-lost))

(defun rdcw-accumulator-lost-p (accumulator)
  "True once ACCUMULATOR is LOST."
  (declare (ignore accumulator))
  (%r4b3-pending 'rdcw-accumulator-lost-p))

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
  ()
  (:documentation "Tickets requested by drains and acked by the watcher,
with a lock and a condition variable (bordeaux-threads). Thread-safe. The
implementation adds the slots it needs."))

(defun make-rdcw-barrier ()
  "A fresh RDCW-BARRIER: no ticket requested or acked, not failed."
  (%r4b3-pending 'make-rdcw-barrier))

(defun rdcw-barrier-request (barrier)
  "Take a new ticket: → a positive integer, larger than every ticket BARRIER
gave before."
  (declare (ignore barrier))
  (%r4b3-pending 'rdcw-barrier-request))

(defun rdcw-barrier-requested (barrier)
  "The largest ticket BARRIER has given, 0 before the first."
  (declare (ignore barrier))
  (%r4b3-pending 'rdcw-barrier-requested))

(defun rdcw-barrier-ack (barrier ticket)
  "Answer every ticket up to TICKET (an integer) and wake the waiters. An
ack below an earlier one changes nothing. → NIL."
  (declare (ignore barrier ticket))
  (%r4b3-pending 'rdcw-barrier-ack))

(defun rdcw-barrier-wait (barrier ticket timeout)
  "Wait at most TIMEOUT seconds (a non-negative real; 0 does not block) for
TICKET to be acked. → T when it is (also when it was before the call), NIL
on timeout or once BARRIER has failed."
  (declare (ignore barrier ticket timeout))
  (%r4b3-pending 'rdcw-barrier-wait))

(defun rdcw-barrier-fail (barrier)
  "Fail BARRIER for good (the watcher is gone or the journal is closing):
current and later RDCW-BARRIER-WAITs for tickets not yet acked return NIL
at once. → NIL."
  (declare (ignore barrier))
  (%r4b3-pending 'rdcw-barrier-fail))
