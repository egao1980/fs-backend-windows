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

(defclass rdcw-journal (sp:journal)
  ((thread :initform nil :accessor %rdcw-journal-thread
           :documentation "The watcher thread; NIL once closed.")
   (handle :initform nil :accessor %rdcw-journal-handle
           :documentation "The root's directory HANDLE (a foreign pointer); NIL once closed.")
   (buffer-size :initform nil :accessor %rdcw-journal-buffer-size
                :documentation "*RDCW-BUFFER-SIZE* when the journal was made."))
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

(defun %rdcw-volume-watchable-p (directory)
  "Windows: true when the volume of the directory pathname DIRECTORY is
local (GetVolumePathNameW, then GetDriveTypeW: DRIVE_FIXED, DRIVE_REMOVABLE
or DRIVE_RAMDISK; not DRIVE_REMOTE, CD-ROM or unknown). Never signals."
  (declare (ignore directory))
  (%r4b3-pending '%rdcw-volume-watchable-p))

(defun rdcw-available-p (fs directory)
  "The AVAILABLE-P probe of :RDCW: true on Windows for a local filesystem
and a directory on a local volume (fixed, removable or RAM disk; network
drives fall back to :POLL)."
  (declare (ignorable fs directory))
  (and (%windows-p)
       (typep fs 'sp:local-filesystem)
       (%rdcw-volume-watchable-p directory)))

(defun make-rdcw-journal (fs directory matcher)
  "The constructor registered for :RDCW: an open RDCW-JOURNAL over the
directory pathname DIRECTORY on the local filesystem FS with the ignore
MATCHER (or NIL). The handle is on DIRECTORY's truename; the watcher's
first ReadDirectoryChangesW is pending before this returns, so every change
after it is reported. Win32 failures (CreateFileW, CreateEventW, the first
ReadDirectoryChangesW, e.g. ERROR_INVALID_FUNCTION on a filesystem without
change notification) signal RDCW-ERROR with nothing left open or running.
*RDCW-BUFFER-SIZE* outside its range signals a TYPE-ERROR."
  (declare (ignore fs directory matcher))
  (%r4b3-pending 'make-rdcw-journal))

(defun rdcw-handle-count ()
  "Number of journal directory handles this image holds open (journals made
and not yet closed)."
  (%r4b3-pending 'rdcw-handle-count))

(defun rdcw-journal-inject (journal octets &key (bytes (length octets)) (code 0))
  "Test hook: apply a completion to the open JOURNAL through the code the
watcher runs for a real one (RDCW-ACCUMULATE on JOURNAL's accumulator), as
if ReadDirectoryChangesW had returned OCTETS with BYTES and the Win32 error
CODE. Returns NIL. A closed JOURNAL signals CL-STACK-PATHLIB:JOURNAL-ERROR."
  (declare (ignorable journal octets bytes code))
  (%r4b3-pending 'rdcw-journal-inject))

(defun rdcw-journal-pause (journal)
  "Test hook: stop JOURNAL's watcher from handling completions and barrier
tickets until RDCW-JOURNAL-RESUME. Returns NIL once the watcher has
acknowledged the pause. Changes made meanwhile wait in the kernel (and
overflow its queue, which then reports a zero-byte completion on resume).
A drain meanwhile times out and answers (values NIL NIL). JOURNAL-CLOSE of a
paused journal still works. A closed JOURNAL signals
CL-STACK-PATHLIB:JOURNAL-ERROR."
  (declare (ignore journal))
  (%r4b3-pending 'rdcw-journal-pause))

(defun rdcw-journal-resume (journal)
  "Test hook: undo RDCW-JOURNAL-PAUSE; the watcher then handles what is
queued. Returns NIL; NIL also for a journal that is not paused or closed."
  (declare (ignore journal))
  (%r4b3-pending 'rdcw-journal-resume))

(defun short-path-name (pathname)
  "Windows: the 8.3 form of the existing file or directory PATHNAME (a
pathname or native namestring) as a native namestring
(GetShortPathNameW). It equals the long form where the volume generates no
8.3 aliases. Signals RDCW-ERROR when the call fails and off Windows."
  (declare (ignore pathname))
  (%r4b3-pending 'short-path-name))

(defun process-handle-count ()
  "Windows: the number of open handles of this process
(GetProcessHandleCount). Signals RDCW-ERROR off Windows."
  (%r4b3-pending 'process-handle-count))

(defmethod sp:journal-drain ((journal rdcw-journal) &key settle)
  ;; SETTLE is checked by the protocol's :around and not waited on: NTFS
  ;; queues a notification inside the call that made the change, and the
  ;; barrier (bounded by *RDCW-BARRIER-TIMEOUT*) collects what is queued.
  (declare (ignore settle))
  (%r4b3-pending 'journal-drain))

(defmethod sp:journal-close ((journal rdcw-journal))
  (%r4b3-pending 'journal-close))
