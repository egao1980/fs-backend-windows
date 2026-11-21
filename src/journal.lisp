(in-package #:fs-backend-windows)

;;; R4b-3: the :RDCW journal.
;;;
;;; One directory handle per journal, on the root's truename: CreateFileW
;;; with FILE_LIST_DIRECTORY, +RDCW-SHARE-MODE+ (read | write | delete, so
;;; the root and everything below it can still be written, deleted and
;;; renamed), OPEN_EXISTING and +RDCW-CREATE-FLAGS+ (BACKUP_SEMANTICS |
;;; OVERLAPPED). A watcher thread the journal owns (bordeaux-threads) issues
;;; ReadDirectoryChangesW on it, overlapped, recursive (bWatchSubtree TRUE),
;;; with +RDCW-NOTIFY-FILTER+ and a buffer of *RDCW-BUFFER-SIZE* octets, and
;;; waits with WaitForMultipleObjects on the request's event and a wake
;;; event. Each completion goes to RDCW-ACCUMULATE, then the request is
;;; re-issued at once. The thread also answers the RDCW-BARRIER (see
;;; notify.lisp). The first ReadDirectoryChangesW is issued before FS-WATCH
;;; returns, so every change after it is seen.
;;;
;;; JOURNAL-DRAIN: take a barrier ticket, wake the watcher and wait at most
;;; *RDCW-BARRIER-TIMEOUT* for the ack; check the root (below); take the
;;; accumulator; resolve the names against the disk (RDCW-RESOLVE-PATHS with
;;; a FindFirstFileW lister). → (values paths T), or (values NIL NIL) when
;;; the ack did not come, the take was incomplete, or the journal is LOST.
;;;
;;; The root check, on every drain: GetFinalPathNameByHandleW of the handle
;;; must still be RDCW-SAME-FINAL-PATH-P to the one recorded at open (else
;;; the root or an ancestor was renamed), and GetFileAttributesW of the root
;;; must be a directory (else it was deleted). Failing either marks the
;;; accumulator LOST: every later drain is (values NIL NIL) until close.
;;;
;;; JOURNAL-CLOSE (any thread; once, through the protocol's :around): mark
;;; closing, CancelIoEx the request, wake the watcher, which waits for the
;;; cancelled request (GetOverlappedResult, ERROR_OPERATION_ABORTED) and
;;; exits; join it; fail the barrier; close the directory handle and both
;;; events; free the buffer and the OVERLAPPED.

(defvar *rdcw-buffer-size* 65536
  "Size in octets of the ReadDirectoryChangesW buffer of new journals: a
multiple of 4 from 4096 to 1048576 (64 KiB is the most a network
redirector takes; the kernel's own queue for the handle has the size of the
first request). Read by FS-WATCH.")

(defvar *rdcw-barrier-timeout* 1
  "Seconds a JOURNAL-DRAIN waits for the watcher thread to ack its barrier
ticket (a non-negative real). The ack normally comes in well under a
millisecond; a drain that does not get it answers (values NIL NIL). Read at
each drain.")

(defconstant +rdcw-rank+ 100
  "The rank :RDCW registers with.")


(defconstant +rdcw-wide-path-length+ 32768
  "WCHARs of the buffers for GetFinalPathNameByHandleW and GetVolumePathNameW
(the longest \\\\?\\ path, NUL included).")

(defconstant +rdcw-pause-timeout+ 5
  "Seconds RDCW-JOURNAL-PAUSE waits at most for the watcher's acknowledgement.")

(defclass rdcw-journal (sp:journal)
  ((thread :initform nil :accessor %rdcw-journal-thread
           :documentation "The watcher thread; NIL once closed.")
   (handle :initform nil :accessor %rdcw-journal-handle
           :documentation "The root's directory HANDLE (a foreign pointer); NIL once closed.")
   (buffer-size :initform nil :accessor %rdcw-journal-buffer-size
                :documentation "*RDCW-BUFFER-SIZE* when the journal was made.")
   (accumulator :initform (make-rdcw-accumulator) :reader %rdcw-accumulator)
   (barrier :initform (make-rdcw-barrier) :reader %rdcw-barrier)
   (final-path :initform nil :accessor %rdcw-final-path
               :documentation "GetFinalPathNameByHandleW of HANDLE at open (\\\\?\\…).")
   (request-event :initform nil :accessor %rdcw-request-event
                  :documentation "Manual-reset event in the OVERLAPPED.")
   (wake-event :initform nil :accessor %rdcw-wake-event
               :documentation "Auto-reset event: \"look at the control state\". It carries no meaning itself.")
   (buffer :initform nil :accessor %rdcw-buffer
           :documentation "Foreign buffer of BUFFER-SIZE octets.")
   (overlapped :initform nil :accessor %rdcw-overlapped
               :documentation "Foreign OVERLAPPED of the request.")
   (wait-handles :initform nil :accessor %rdcw-wait-handles
                 :documentation "Foreign HANDLE[2] = {request event, wake event}.")
   (control-lock :initform (bt2:make-lock :name "rdcw control") :reader %rdcw-control-lock
                 :documentation "Guards STATE, STARTUP, PAUSE-REQUESTED, CLOSING and the event handles.")
   (control-cv :initform (bt2:make-condition-variable :name "rdcw control")
               :reader %rdcw-control-cv)
   (state :initform :starting :accessor %rdcw-state
          :documentation "The watcher: :STARTING, :RUNNING, :PAUSED, :LOST or :EXITED.")
   (startup :initform nil :accessor %rdcw-startup
            :documentation "NIL until the watcher reports: :OK or (operation . code).")
   (pause-requested :initform nil :accessor %rdcw-pause-requested)
   (closing :initform nil :accessor %rdcw-closing)
   (io-lock :initform (bt2:make-lock :name "rdcw io") :reader %rdcw-io-lock
            :documentation "A drain's use of HANDLE against close releasing it."))
  (:default-initargs :mechanism :rdcw)
  (:documentation "A change journal fed by ReadDirectoryChangesW. Made by
MAKE-RDCW-JOURNAL (FS-WATCH ... :MECHANISM :RDCW). The implementation adds
the slots it needs (accumulator, barrier, events, buffer, OVERLAPPED, the
final path recorded at open, the closing flag, ...)."))

(defun rdcw-journal-thread (journal)
  "The watcher thread of JOURNAL, or NIL once JOURNAL is closed."
  (%rdcw-journal-thread journal))

(defun rdcw-journal-handle (journal)
  "JOURNAL's directory HANDLE, or NIL once JOURNAL is closed."
  (%rdcw-journal-handle journal))

(defun rdcw-journal-buffer-size (journal)
  "The ReadDirectoryChangesW buffer size of JOURNAL, in octets."
  (%rdcw-journal-buffer-size journal))

;;; --- foreign memory helpers ----------------------------------------------------------

(defun %foreign-octets (pointer count &optional (offset 0))
  "A fresh (unsigned-byte 8) vector of the COUNT octets at POINTER + OFFSET."
  (let ((v (make-array count :element-type '(unsigned-byte 8))))
    (dotimes (i count v)
      (setf (aref v i) (cffi:mem-aref pointer :uint8 (+ offset i))))))

(defun %wide-string (pointer nchars &optional (offset 0))
  "The NCHARS UTF-16 code units at POINTER + OFFSET (octets) as a string, or
NIL when they are not well-formed UTF-16."
  (utf16le-octets-to-string (%foreign-octets pointer (* 2 nchars) offset)))

(defun %wide-z (pointer offset max-chars)
  "The NUL-terminated UTF-16 string at POINTER + OFFSET (octets), at most
MAX-CHARS code units; NIL when not well-formed."
  (let ((n (loop for i below max-chars
                 until (zerop (cffi:mem-ref pointer :uint16 (+ offset (* 2 i))))
                 count t)))
    (%wide-string pointer n offset)))

(defmacro %with-foreign-buffer ((var type count) &body body)
  "BODY with VAR a heap foreign array of COUNT TYPE, freed afterwards (big
buffers do not belong on the stack)."
  `(let ((,var (cffi:foreign-alloc ,type :count ,count)))
     (unwind-protect (progn ,@body)
       (cffi:foreign-free ,var))))

(defun %native (pathname-or-string)
  (if (stringp pathname-or-string)
      pathname-or-string
      (uiop:native-namestring pathname-or-string)))

;;; --- handle count --------------------------------------------------------------------

(defvar *rdcw-handles* 0
  "Journal directory handles open in this image.")

(defvar *rdcw-handles-lock* (bt2:make-lock :name "rdcw handle count"))

(defun %count-handles (delta)
  (bt2:with-lock-held (*rdcw-handles-lock*)
    (incf *rdcw-handles* delta)))

(defun rdcw-handle-count ()
  "Number of journal directory handles this image holds open (journals made
and not yet closed)."
  (bt2:with-lock-held (*rdcw-handles-lock*)
    *rdcw-handles*))

;;; --- availability --------------------------------------------------------------------

(defun %rdcw-volume-watchable-p (directory)
  "Windows: true when the volume of the directory pathname DIRECTORY is
local (GetVolumePathNameW, then GetDriveTypeW: DRIVE_FIXED, DRIVE_REMOVABLE
or DRIVE_RAMDISK; not DRIVE_REMOTE, CD-ROM or unknown). Never signals."
  (%with-foreign-buffer (buffer :uint16 +rdcw-wide-path-length+)
    (and (%get-volume-path-name (%native directory) buffer +rdcw-wide-path-length+)
         (let ((volume (%wide-z buffer 0 +rdcw-wide-path-length+)))
           (and volume
                (member (%get-drive-type volume)
                        (list +drive-fixed+ +drive-removable+ +drive-ramdisk+))
                t)))))

(defun rdcw-available-p (fs directory)
  "The AVAILABLE-P probe of :RDCW: true on Windows for a local filesystem
and a directory on a local volume (fixed, removable or RAM disk; network
drives fall back to :POLL)."
  (declare (ignorable fs directory))
  (and (%windows-p)
       (typep fs 'sp:local-filesystem)
       (%rdcw-volume-watchable-p directory)))

;;; --- Win32 pieces of a journal -------------------------------------------------------

(defun %final-path-of (handle)
  "→ (values final-path code): GetFinalPathNameByHandleW of HANDLE, or NIL
and the GetLastError read right after the call."
  (%with-foreign-buffer (buffer :uint16 +rdcw-wide-path-length+)
    (let ((n (%get-final-path-name-by-handle handle buffer +rdcw-wide-path-length+ 0)))
      (if (< 0 n +rdcw-wide-path-length+)
          (values (%wide-string buffer n) nil)
          (values nil (%get-last-error))))))

(defun %rdcw-fail (journal operation code)
  "Signal RDCW-ERROR for the Win32 call OPERATION; CODE was read with
GetLastError right after it (0 means unknown)."
  (error 'rdcw-error :journal journal :operation operation
                     :code (and (integerp code) (plusp code) code)))

(defun %rdcw-open (journal native-root)
  (let ((h (%create-file native-root +file-list-directory+ +rdcw-share-mode+
                         (cffi:null-pointer) +open-existing+ +rdcw-create-flags+
                         (cffi:null-pointer))))
    (when (%invalid-handle-p h)
      (let ((code (%get-last-error)))
        (%rdcw-fail journal "CreateFileW" code)))
    (setf (%rdcw-journal-handle journal) h)
    (%count-handles 1)
    (multiple-value-bind (path code) (%final-path-of h)
      (unless path
        (%rdcw-fail journal "GetFinalPathNameByHandleW" code))
      (setf (%rdcw-final-path journal) path))))

(defun %rdcw-make-event (journal manual-reset)
  (let ((e (%create-event (cffi:null-pointer) manual-reset nil (cffi:null-pointer))))
    (when (%invalid-handle-p e)
      (let ((code (%get-last-error)))
        (%rdcw-fail journal "CreateEventW" code)))
    e))

(defun %rdcw-make-events (journal)
  (setf (%rdcw-request-event journal) (%rdcw-make-event journal t))
  (setf (%rdcw-wake-event journal) (%rdcw-make-event journal nil)))

(defun %rdcw-allocate (journal size)
  (setf (%rdcw-buffer journal) (cffi:foreign-alloc :uint8 :count size))
  (let ((ov (cffi:foreign-alloc '(:struct %overlapped))))
    (setf (%rdcw-overlapped journal) ov)
    (cffi:with-foreign-slots ((internal internal-high offset offset-high event)
                              ov (:struct %overlapped))
      (setf internal 0 internal-high 0 offset 0 offset-high 0
            event (%rdcw-request-event journal))))
  (let ((handles (cffi:foreign-alloc :pointer :count 2)))
    (setf (%rdcw-wait-handles journal) handles)
    (setf (cffi:mem-aref handles :pointer 0) (%rdcw-request-event journal)
          (cffi:mem-aref handles :pointer 1) (%rdcw-wake-event journal))))

(defun %rdcw-release (journal)
  "Release everything JOURNAL holds, in reverse order of acquisition: stop and
join the watcher (which cancels and waits for its own pending request), fail
the barrier, close the events and the directory handle, free the foreign
memory. Each piece is released once and its slot set to NIL, so this serves
both the constructor's unwind and JOURNAL-CLOSE."
  (let ((thread (%rdcw-journal-thread journal))
        (lock (%rdcw-control-lock journal)))
    (bt2:with-lock-held (lock)
      (setf (%rdcw-closing journal) t)
      (bt2:condition-broadcast (%rdcw-control-cv journal))
      (let ((wake (%rdcw-wake-event journal)))
        (when wake (%set-event wake))))
    (when thread
      ;; A hint only: the watcher cancels its own request before it exits.
      (let ((h (%rdcw-journal-handle journal))
            (ov (%rdcw-overlapped journal)))
        (when (and h ov) (%cancel-io-ex h ov)))
      (bt2:join-thread thread)
      (setf (%rdcw-journal-thread journal) nil)))
  (rdcw-barrier-fail (%rdcw-barrier journal))
  (bt2:with-lock-held ((%rdcw-io-lock journal))
    (bt2:with-lock-held ((%rdcw-control-lock journal))
      (let ((request (%rdcw-request-event journal))
            (wake (%rdcw-wake-event journal)))
        (setf (%rdcw-request-event journal) nil
              (%rdcw-wake-event journal) nil)
        (when request (%close-handle request))
        (when wake (%close-handle wake))))
    (let ((h (%rdcw-journal-handle journal)))
      (when h
        (setf (%rdcw-journal-handle journal) nil)
        (%close-handle h)
        (%count-handles -1))))
  (dolist (accessor (list #'%rdcw-buffer #'%rdcw-overlapped #'%rdcw-wait-handles))
    (let ((p (funcall accessor journal)))
      (when p (cffi:foreign-free p))))
  (setf (%rdcw-buffer journal) nil
        (%rdcw-overlapped journal) nil
        (%rdcw-wait-handles journal) nil)
  nil)

;;; --- the watcher ---------------------------------------------------------------------

(defun %rdcw-closing-p (journal)
  (bt2:with-lock-held ((%rdcw-control-lock journal))
    (%rdcw-closing journal)))

(defun %rdcw-apply (journal octets bytes code)
  "One completion into JOURNAL's accumulator: the watcher's code, and
RDCW-JOURNAL-INJECT's."
  (rdcw-accumulate (%rdcw-accumulator journal) octets :bytes bytes :code code))

(defun %rdcw-issue (journal)
  "Issue ReadDirectoryChangesW. → T when it is pending, else the Win32 code
(read right after the call; 0 when the call failed without one)."
  (let ((ov (%rdcw-overlapped journal)))
    (%reset-event (%rdcw-request-event journal))
    (cffi:with-foreign-slots ((internal internal-high offset offset-high)
                              ov (:struct %overlapped))
      (setf internal 0 internal-high 0 offset 0 offset-high 0))
    (if (%read-directory-changes (%rdcw-journal-handle journal) (%rdcw-buffer journal)
                                 (%rdcw-journal-buffer-size journal) t +rdcw-notify-filter+
                                 (cffi:null-pointer) ov (cffi:null-pointer))
        t
        (let ((code (%get-last-error)))
          (if (= code +error-io-pending+) t code)))))

(defun %rdcw-complete (journal)
  "The request's event is signalled: take the result, copy the octets out of
the buffer, re-issue at once (unless the completion lost the root or the
journal is closing), then accumulate the copy. → T when a request is
pending again."
  (multiple-value-bind (code bytes)
      (cffi:with-foreign-object (n :uint32)
        (if (%get-overlapped-result (%rdcw-journal-handle journal) (%rdcw-overlapped journal)
                                    n nil)
            (values +error-success+ (cffi:mem-ref n :uint32))
            (let ((code (%get-last-error)))
              ;; Fail closed: a failure without a usable code loses the root.
              (values (if (member code (list +error-success+ +error-io-incomplete+))
                          +error-io-incomplete+
                          code)
                      0))))
    (let* ((octets (%foreign-octets (%rdcw-buffer journal)
                                    (min bytes (%rdcw-journal-buffer-size journal))))
           (pending (and (member code (list +error-success+ +error-notify-enum-dir+))
                         (not (%rdcw-closing-p journal))
                         (eq t (%rdcw-issue journal)))))
      (%rdcw-apply journal octets (length octets) code)
      pending)))

(defun %rdcw-signalled-p (journal)
  (let ((w (%wait-for-single-object (%rdcw-request-event journal) 0)))
    (cond ((= w +wait-object-0+) t)
          ((= w +wait-timeout+) nil)
          (t (error 'rdcw-error :journal journal :operation "WaitForSingleObject"
                                :code nil)))))

(defun %rdcw-report-startup (journal status)
  (bt2:with-lock-held ((%rdcw-control-lock journal))
    (unless (%rdcw-startup journal)
      (setf (%rdcw-startup journal) status))
    (bt2:condition-broadcast (%rdcw-control-cv journal))))

(defun %rdcw-control (journal)
  "→ :CLOSE or :RUN. While a pause is requested, the watcher acknowledges it
and waits here, on Lisp state only."
  (let ((lock (%rdcw-control-lock journal))
        (cv (%rdcw-control-cv journal)))
    (bt2:with-lock-held (lock)
      (loop
        (cond ((%rdcw-closing journal)
               (return :close))
              ((%rdcw-pause-requested journal)
               (unless (eq (%rdcw-state journal) :paused)
                 (setf (%rdcw-state journal) :paused)
                 (bt2:condition-broadcast cv))
               (bt2:condition-wait cv lock))
              (t
               (setf (%rdcw-state journal) :running)
               (return :run)))))))

(defun %rdcw-lost-idle (journal)
  "The watcher can no longer watch: mark LOST, fail the barrier, and wait on
Lisp state only until close."
  (rdcw-accumulator-mark-lost (%rdcw-accumulator journal))
  (rdcw-barrier-fail (%rdcw-barrier journal))
  (let ((lock (%rdcw-control-lock journal))
        (cv (%rdcw-control-cv journal)))
    (bt2:with-lock-held (lock)
      (setf (%rdcw-state journal) :lost)
      (unless (%rdcw-startup journal)
        (setf (%rdcw-startup journal) (cons "ReadDirectoryChangesW" nil)))
      (bt2:condition-broadcast cv)
      (loop until (%rdcw-closing journal)
            do (bt2:condition-wait cv lock)))))

(defun %rdcw-run (journal)
  "The watcher's loop while a request is pending. → :CLOSE or :LOST."
  (let ((acked 0)
        (barrier (%rdcw-barrier journal))
        (handles (%rdcw-wait-handles journal)))
    (flet ((outcome () (if (%rdcw-closing-p journal) :close :lost)))
      (loop
        (when (eq :close (%rdcw-control journal))
          (return :close))
        (let ((w (%wait-for-multiple-objects 2 handles nil +infinite+)))
          (unless (<= +wait-object-0+ w (1+ +wait-object-0+))
            (return :lost)))
        (when (and (%rdcw-signalled-p journal) (not (%rdcw-complete journal)))
          (return (outcome)))
        ;; Tickets come from Lisp state, not from which handle fired.
        (let ((ticket (rdcw-barrier-requested barrier)))
          (when (> ticket acked)
            ;; Collect until the request is pending and unsignalled.
            (loop while (%rdcw-signalled-p journal)
                  do (unless (%rdcw-complete journal)
                       (return-from %rdcw-run (outcome))))
            (rdcw-barrier-ack barrier ticket)
            (setf acked ticket)))))))

(defun %rdcw-watch (journal)
  "The watcher thread: issue the first request, report it to the
constructor, run until close. It never ends with a request pending."
  (let ((pending nil))
    (unwind-protect
         ;; The thread's top level: a Lisp error here must become LOST and a
         ;; failed barrier (so no drain claims completeness), not an
         ;; unhandled error in a background thread.
         (handler-case
             (let ((r (%rdcw-issue journal)))
               (setf pending (eq r t))
               (%rdcw-report-startup journal (if pending :ok (cons "ReadDirectoryChangesW" r)))
               (when pending
                 (when (eq :lost (%rdcw-run journal))
                   (%rdcw-lost-idle journal))))
           (error ()
             (%rdcw-lost-idle journal)))
      ;; Cancel our own request and wait for it: the buffer and the
      ;; OVERLAPPED are ours again before close frees them.
      (when pending
        (let ((h (%rdcw-journal-handle journal))
              (ov (%rdcw-overlapped journal)))
          (%cancel-io-ex h ov)
          (cffi:with-foreign-object (n :uint32)
            (%get-overlapped-result h ov n t))))
      (bt2:with-lock-held ((%rdcw-control-lock journal))
        (setf (%rdcw-state journal) :exited)
        (unless (%rdcw-startup journal)
          (setf (%rdcw-startup journal) (cons "ReadDirectoryChangesW" nil)))
        (bt2:condition-broadcast (%rdcw-control-cv journal))))))

(defun %rdcw-start-watcher (journal)
  (setf (%rdcw-journal-thread journal)
        (handler-case (bt2:make-thread (lambda () (%rdcw-watch journal))
                                       :name "rdcw watcher")
          (error ()
            (error 'rdcw-error :journal journal :operation "bt2:make-thread" :code nil))))
  (let ((lock (%rdcw-control-lock journal))
        (cv (%rdcw-control-cv journal)))
    (let ((status (bt2:with-lock-held (lock)
                    (loop until (%rdcw-startup journal)
                          do (bt2:condition-wait cv lock))
                    (%rdcw-startup journal))))
      (unless (eq status :ok)
        (%rdcw-fail journal (car status) (cdr status))))))

;;; --- the constructor -----------------------------------------------------------------

(defun %dword-multiple-p (n)
  (and (integerp n) (zerop (mod n 4))))

(deftype %rdcw-buffer-size ()
  '(and (integer 4096 1048576) (satisfies %dword-multiple-p)))

(defun make-rdcw-journal (fs directory matcher)
  "The constructor registered for :RDCW: an open RDCW-JOURNAL over the
directory pathname DIRECTORY on the local filesystem FS with the ignore
MATCHER (or NIL). The handle is on DIRECTORY's truename; the watcher's
first ReadDirectoryChangesW is pending before this returns, so every change
after it is reported. Win32 failures (CreateFileW, CreateEventW, the first
ReadDirectoryChangesW, e.g. ERROR_INVALID_FUNCTION on a filesystem without
change notification) signal RDCW-ERROR with nothing left open or running.
*RDCW-BUFFER-SIZE* outside its range signals a TYPE-ERROR."
  (let ((size *rdcw-buffer-size*))
    (check-type size %rdcw-buffer-size)  ; before anything is opened
    (let ((journal (make-instance 'rdcw-journal :filesystem fs :root directory :ignore matcher))
          (ok nil))
      (setf (%rdcw-journal-buffer-size journal) size)
      (unwind-protect
           (progn
             (%rdcw-open journal (uiop:native-namestring (truename directory)))
             (%rdcw-make-events journal)
             (%rdcw-allocate journal size)
             (%rdcw-start-watcher journal)
             (setf ok t)
             journal)
        (unless ok
          (%rdcw-release journal))))))

;;; --- test hooks ----------------------------------------------------------------------

(defun %rdcw-check-open (journal what)
  (unless (sp:journal-open-p journal)
    (error 'sp:journal-error :journal journal
                             :message (format nil "~A of a closed journal" what))))

(defun rdcw-journal-inject (journal octets &key (bytes (length octets)) (code 0))
  "Test hook: apply a completion to the open JOURNAL through the code the
watcher runs for a real one (RDCW-ACCUMULATE on JOURNAL's accumulator), as
if ReadDirectoryChangesW had returned OCTETS with BYTES and the Win32 error
CODE. Returns NIL. A closed JOURNAL signals CL-STACK-PATHLIB:JOURNAL-ERROR."
  (%rdcw-check-open journal "inject")
  (%rdcw-apply journal octets bytes code)
  nil)

(defun rdcw-journal-pause (journal)
  "Test hook: stop JOURNAL's watcher from handling completions and barrier
tickets until RDCW-JOURNAL-RESUME. Returns NIL once the watcher has
acknowledged the pause. Changes made meanwhile wait in the kernel (and
overflow its queue, which then reports a zero-byte completion on resume).
A drain meanwhile times out and answers (values NIL NIL). JOURNAL-CLOSE of a
paused journal still works. A closed JOURNAL signals
CL-STACK-PATHLIB:JOURNAL-ERROR."
  (%rdcw-check-open journal "pause")
  (let ((lock (%rdcw-control-lock journal))
        (cv (%rdcw-control-cv journal))
        (deadline (+ (get-internal-real-time)
                     (* +rdcw-pause-timeout+ internal-time-units-per-second))))
    (bt2:with-lock-held (lock)
      (setf (%rdcw-pause-requested journal) t)
      (bt2:condition-broadcast cv)
      (let ((wake (%rdcw-wake-event journal)))
        (when wake (%set-event wake)))
      (loop until (or (member (%rdcw-state journal) '(:paused :lost :exited))
                      (%rdcw-closing journal))
            do (let ((left (/ (- deadline (get-internal-real-time))
                              internal-time-units-per-second)))
                 (when (<= left 0) (return))
                 (bt2:condition-wait cv lock :timeout left)))))
  nil)

(defun rdcw-journal-resume (journal)
  "Test hook: undo RDCW-JOURNAL-PAUSE; the watcher then handles what is
queued. Returns NIL; NIL also for a journal that is not paused or closed."
  (when (sp:journal-open-p journal)
    (bt2:with-lock-held ((%rdcw-control-lock journal))
      (setf (%rdcw-pause-requested journal) nil)
      ;; A later pause waits for a fresh acknowledgement.
      (when (eq (%rdcw-state journal) :paused)
        (setf (%rdcw-state journal) :running))
      (bt2:condition-broadcast (%rdcw-control-cv journal))
      (let ((wake (%rdcw-wake-event journal)))
        (when wake (%set-event wake)))))
  nil)

;;; --- Windows helpers -----------------------------------------------------------------

(defun short-path-name (pathname)
  "Windows: the 8.3 form of the existing file or directory PATHNAME (a
pathname or native namestring) as a native namestring
(GetShortPathNameW). It equals the long form where the volume generates no
8.3 aliases. Signals RDCW-ERROR when the call fails and off Windows."
  (let* ((name (%native pathname))
         (size (%get-short-path-name name (cffi:null-pointer) 0)))
    (when (zerop size)
      (let ((code (%get-last-error)))
        (%rdcw-fail nil "GetShortPathNameW" code)))
    (%with-foreign-buffer (buffer :uint16 size)
      (let ((n (%get-short-path-name name buffer size)))
        (when (or (zerop n) (>= n size))
          (let ((code (%get-last-error)))
            (%rdcw-fail nil "GetShortPathNameW" code)))
        (or (%wide-string buffer n)
            (%rdcw-fail nil "GetShortPathNameW" nil))))))

(defun process-handle-count ()
  "Windows: the number of open handles of this process
(GetProcessHandleCount). Signals RDCW-ERROR off Windows."
  (cffi:with-foreign-object (n :uint32)
    (unless (%get-process-handle-count (%get-current-process) n)
      (let ((code (%get-last-error)))
        (%rdcw-fail nil "GetProcessHandleCount" code)))
    (cffi:mem-ref n :uint32)))

;;; --- drain and close -----------------------------------------------------------------

(defun %rdcw-root-intact-p (journal handle)
  "True when HANDLE's final path is still the one recorded at open and that
path is a directory. Any failure counts as not intact."
  (let ((recorded (%rdcw-final-path journal))
        (now (%final-path-of handle)))
    (and recorded now
         (rdcw-same-final-path-p now recorded)
         (let ((attributes (%get-file-attributes recorded)))
           (and (/= attributes +invalid-file-attributes+)
                (logtest attributes +file-attribute-directory+))))))

(defun %rdcw-list-directory (pattern relative directories)
  "Entries ((long . short-or-nil) …) of the FindFirstFileW PATTERN, NIL when
it cannot be listed. Records each subdirectory's relative path in
DIRECTORIES."
  (%with-foreign-buffer (data :uint8 +win32-find-data-size+)
    (let ((h (%find-first-file pattern data)))
      (unless (%invalid-handle-p h)
        (unwind-protect
             (let ((entries '()))
               (loop
                 (let ((long (%wide-z data +find-data-file-name-offset+ 260))
                       (short (%wide-z data +find-data-alternate-name-offset+ 14))
                       (attributes (cffi:mem-ref data :uint32 0)))
                   (when (and long (not (member long '("." "..") :test #'string=)))
                     (push (cons long (and short (plusp (length short)) short)) entries)
                     (when (logtest attributes +file-attribute-directory+)
                       (setf (gethash (%join-relative relative long) directories) t))))
                 (unless (%find-next-file h data)
                   (return (nreverse entries)))))
          (%find-close h))))))

(defun %rdcw-lister (root directories)
  "The RDCW-RESOLVE-PATHS lister below the final path ROOT (\\\\?\\…: on-disk
case, long paths)."
  (let ((base (string-right-trim "\\" root)))
    (lambda (relative)
      (%rdcw-list-directory
       (concatenate 'string base
                    (if (string= relative "")
                        ""
                        (concatenate 'string "\\" (substitute #\\ #\/ relative)))
                    "\\*")
       relative directories))))

(defmethod sp:journal-drain ((journal rdcw-journal) &key settle)
  ;; SETTLE is checked by the protocol's :around and not waited on: NTFS
  ;; queues a notification inside the call that made the change, and the
  ;; barrier (bounded by *RDCW-BARRIER-TIMEOUT*) collects what is queued.
  (declare (ignore settle))
  (let* ((barrier (%rdcw-barrier journal))
         (accumulator (%rdcw-accumulator journal))
         (ticket (rdcw-barrier-request barrier)))
    (bt2:with-lock-held ((%rdcw-control-lock journal))
      (let ((wake (%rdcw-wake-event journal)))
        (when wake (%set-event wake))))
    (let ((acked (rdcw-barrier-wait barrier ticket *rdcw-barrier-timeout*)))
      (bt2:with-lock-held ((%rdcw-io-lock journal))
        (let ((h (%rdcw-journal-handle journal)))
          (unless (and h (%rdcw-root-intact-p journal h))
            (rdcw-accumulator-mark-lost accumulator))
          ;; Taken even after a timeout, so the next drain starts clean.
          (multiple-value-bind (names complete-p) (rdcw-accumulator-take accumulator)
            (if (and acked complete-p)
                (let* ((directories (make-hash-table :test #'equal))
                       (paths (rdcw-resolve-paths
                               names (%rdcw-lister (%rdcw-final-path journal) directories))))
                  ;; A trailing "/" marks a directory for the :around's ignore
                  ;; matcher (a rule "build/" matches "build/" only); the
                  ;; :around strips it.
                  (values (mapcar (lambda (p)
                                    (if (gethash p directories) (concatenate 'string p "/") p))
                                  paths)
                          t))
                (values nil nil))))))))

(defmethod sp:journal-close ((journal rdcw-journal))
  (%rdcw-release journal))
