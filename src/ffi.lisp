(in-package #:fs-backend-windows)

;;; R4b-3: Win32 constants, the conditions, and the kernel32 bindings
;;; (CFFI, stdcall). kernel32.dll is a system DLL (a KnownDLL, mapped into
;;; every process), so there is no native overlay: it is opened at ASDF load
;;; time by absolute path under %SystemRoot%\System32 (AGENTS.md CFFI rule),
;;; on Windows only. Off Windows every binding is a Lisp function of the same
;;; arguments that signals RDCW-ERROR, so the system loads (and registers
;;; :RDCW, unavailable) on every OS with no undefined-alien warnings.
;;;
;;; Given: everything in this file. The implementation may add bindings with
;;; %DEFINE-KERNEL32 (or correct one), but keeps the names below.

;;; --- FILE_ACTION_* (winnt.h) ------------------------------------------------------

(defconstant +file-action-added+ 1)
(defconstant +file-action-removed+ 2)
(defconstant +file-action-modified+ 3)
(defconstant +file-action-renamed-old-name+ 4)
(defconstant +file-action-renamed-new-name+ 5)

;;; --- FILE_NOTIFY_CHANGE_* (winnt.h) -----------------------------------------------

(defconstant +file-notify-change-file-name+ #x001)
(defconstant +file-notify-change-dir-name+ #x002)
(defconstant +file-notify-change-attributes+ #x004)
(defconstant +file-notify-change-size+ #x008)
(defconstant +file-notify-change-last-write+ #x010)
(defconstant +file-notify-change-last-access+ #x020)
(defconstant +file-notify-change-creation+ #x040)
(defconstant +file-notify-change-security+ #x100)

(defconstant +rdcw-notify-filter+
  (logior +file-notify-change-file-name+ +file-notify-change-dir-name+
          +file-notify-change-attributes+ +file-notify-change-size+
          +file-notify-change-last-write+ +file-notify-change-creation+)
  "The dwNotifyFilter of every journal: names (files and directories),
attributes, size, last write and creation. Not LAST_ACCESS (reads are not
changes) and not SECURITY (ACLs are not part of an area's state).")

;;; --- CreateFileW --------------------------------------------------------------------

(defconstant +file-list-directory+ #x0001)
(defconstant +file-share-read+ #x1)
(defconstant +file-share-write+ #x2)
(defconstant +file-share-delete+ #x4)
(defconstant +rdcw-share-mode+
  (logior +file-share-read+ +file-share-write+ +file-share-delete+)
  "The journal's directory handle shares everything, so it never blocks a
write, a delete or a rename of the root or of anything below it.")
(defconstant +open-existing+ 3)
(defconstant +file-flag-backup-semantics+ #x02000000)
(defconstant +file-flag-overlapped+ #x40000000)
(defconstant +rdcw-create-flags+
  (logior +file-flag-backup-semantics+ +file-flag-overlapped+)
  "dwFlagsAndAttributes of the directory handle: BACKUP_SEMANTICS (required
to open a directory) and OVERLAPPED (ReadDirectoryChangesW is issued
asynchronously and cancelled with CancelIoEx).")

;;; --- Win32 error codes (winerror.h) -------------------------------------------------

(defconstant +error-success+ 0)
(defconstant +error-invalid-function+ 1)
(defconstant +error-file-not-found+ 2)
(defconstant +error-path-not-found+ 3)
(defconstant +error-access-denied+ 5)
(defconstant +error-invalid-handle+ 6)
(defconstant +error-notify-cleanup+ 745)
(defconstant +error-operation-aborted+ 995)
(defconstant +error-io-incomplete+ 996)
(defconstant +error-io-pending+ 997)
(defconstant +error-notify-enum-dir+ 1022)

;;; --- other Win32 values -------------------------------------------------------------

(defconstant +wait-object-0+ #x000)
(defconstant +wait-timeout+ #x102)
(defconstant +wait-failed+ #xFFFFFFFF)
(defconstant +infinite+ #xFFFFFFFF)
(defconstant +invalid-file-attributes+ #xFFFFFFFF)
(defconstant +file-attribute-directory+ #x10)
(defconstant +drive-removable+ 2)
(defconstant +drive-fixed+ 3)
(defconstant +drive-remote+ 4)
(defconstant +drive-ramdisk+ 6)

;;; FILE_NOTIFY_INFORMATION { DWORD NextEntryOffset; DWORD Action;
;;;                           DWORD FileNameLength; WCHAR FileName[]; }
;;; little-endian; FileNameLength in octets; entries DWORD-aligned.
(defconstant +file-notify-information-header-size+ 12)

;;; --- conditions -------------------------------------------------------------------

(define-condition rdcw-error (sp:journal-error)
  ((operation :initarg :operation :reader rdcw-error-operation :initform nil
              :documentation "The Win32 call that failed, a string such as \"CreateFileW\".")
   (code :initarg :code :reader rdcw-error-code :initform nil
         :documentation "GetLastError after the failed call, or NIL (no Win32 on this OS)."))
  (:documentation "A Win32 call failed where the journal cannot answer
COMPLETE-P NIL instead: opening the root, creating the events or the
watcher thread, or the first ReadDirectoryChangesW in FS-WATCH; or any
binding called off Windows.")
  (:report (lambda (c s)
             (format s "ReadDirectoryChangesW journal: ~A failed~@[ (error ~D)~]"
                     (or (rdcw-error-operation c) "a Win32 call") (rdcw-error-code c)))))

(define-condition malformed-notify-buffer (error)
  ((offset :initarg :offset :reader malformed-notify-buffer-offset :initform nil
           :documentation "Octet offset of the FILE_NOTIFY_INFORMATION entry at fault.")
   (reason :initarg :reason :reader malformed-notify-buffer-reason :initform nil
           :documentation "A short string saying what is wrong."))
  (:documentation "PARSE-FILE-NOTIFY-INFORMATION was given octets that are not
a well-formed chain of FILE_NOTIFY_INFORMATION entries. The accumulator
turns it into an incomplete drain; it never reaches a JOURNAL-DRAIN caller.")
  (:report (lambda (c s)
             (format s "Malformed FILE_NOTIFY_INFORMATION buffer at offset ~S: ~A"
                     (malformed-notify-buffer-offset c) (malformed-notify-buffer-reason c)))))

;;; --- kernel32 ----------------------------------------------------------------------

(defun %windows-p ()
  #+(or windows win32) t
  #-(or windows win32) nil)

#+(or windows win32)
(progn
  (defun %system32-library (name)
    "Absolute pathname of the system DLL NAME in %SystemRoot%\\System32."
    (let ((root (or (uiop:getenv "SystemRoot") (uiop:getenv "windir") "C:\\Windows")))
      (uiop:parse-native-namestring
       (concatenate 'string (string-right-trim "\\/" root) "\\System32\\" name))))
  (cffi:load-foreign-library (%system32-library "kernel32.dll")))

(defmacro %define-kernel32 ((lisp-name c-name) return-type &rest args)
  "A stdcall CFFI binding of the kernel32 function C-NAME on Windows;
elsewhere a function of the same arguments that signals RDCW-ERROR."
  (declare (ignorable return-type))
  #+(or windows win32)
  `(cffi:defcfun (,c-name ,lisp-name :convention :stdcall) ,return-type ,@args)
  #-(or windows win32)
  `(defun ,lisp-name ,(mapcar #'first args)
     (declare (ignore ,@(mapcar #'first args)))
     (error 'rdcw-error :operation ,c-name :code nil)))

;;; LPCWSTR arguments: Lisp strings as NUL-terminated UTF-16LE.
(cffi:defctype %wstring (:string :encoding :utf-16le))

(%define-kernel32 (%create-file "CreateFileW") :pointer
  (name %wstring) (access :uint32) (share :uint32) (security :pointer)
  (disposition :uint32) (flags :uint32) (template :pointer))
(%define-kernel32 (%read-directory-changes "ReadDirectoryChangesW") :boolean
  (handle :pointer) (buffer :pointer) (length :uint32) (watch-subtree :boolean)
  (filter :uint32) (bytes-returned :pointer) (overlapped :pointer) (completion :pointer))
(%define-kernel32 (%get-overlapped-result "GetOverlappedResult") :boolean
  (handle :pointer) (overlapped :pointer) (bytes :pointer) (wait :boolean))
(%define-kernel32 (%cancel-io-ex "CancelIoEx") :boolean
  (handle :pointer) (overlapped :pointer))
(%define-kernel32 (%create-event "CreateEventW") :pointer
  (security :pointer) (manual-reset :boolean) (initial-state :boolean) (name :pointer))
(%define-kernel32 (%set-event "SetEvent") :boolean (handle :pointer))
(%define-kernel32 (%reset-event "ResetEvent") :boolean (handle :pointer))
(%define-kernel32 (%wait-for-single-object "WaitForSingleObject") :uint32
  (handle :pointer) (milliseconds :uint32))
(%define-kernel32 (%wait-for-multiple-objects "WaitForMultipleObjects") :uint32
  (count :uint32) (handles :pointer) (wait-all :boolean) (milliseconds :uint32))
(%define-kernel32 (%close-handle "CloseHandle") :boolean (handle :pointer))
(%define-kernel32 (%get-last-error "GetLastError") :uint32)
(%define-kernel32 (%get-final-path-name-by-handle "GetFinalPathNameByHandleW") :uint32
  (handle :pointer) (buffer :pointer) (length :uint32) (flags :uint32))
(%define-kernel32 (%get-file-attributes "GetFileAttributesW") :uint32 (name %wstring))
(%define-kernel32 (%find-first-file "FindFirstFileW") :pointer
  (pattern %wstring) (find-data :pointer))
(%define-kernel32 (%find-next-file "FindNextFileW") :boolean
  (handle :pointer) (find-data :pointer))
(%define-kernel32 (%find-close "FindClose") :boolean (handle :pointer))
(%define-kernel32 (%get-short-path-name "GetShortPathNameW") :uint32
  (long-path %wstring) (buffer :pointer) (length :uint32))
(%define-kernel32 (%get-volume-path-name "GetVolumePathNameW") :boolean
  (name %wstring) (buffer :pointer) (length :uint32))
(%define-kernel32 (%get-drive-type "GetDriveTypeW") :uint32 (root %wstring))
(%define-kernel32 (%get-current-process "GetCurrentProcess") :pointer)
(%define-kernel32 (%get-process-handle-count "GetProcessHandleCount") :boolean
  (process :pointer) (count :pointer))

;;; OVERLAPPED: Internal, InternalHigh (ULONG_PTR), Offset, OffsetHigh (DWORD,
;;; in a union with PVOID Pointer), hEvent (HANDLE).
(cffi:defcstruct %overlapped
  (internal :uintptr)
  (internal-high :uintptr)
  (offset :uint32)
  (offset-high :uint32)
  (event :pointer))

;;; WIN32_FIND_DATAW: attributes, three FILETIMEs, size high/low, two
;;; reserved DWORDs, cFileName[MAX_PATH], cAlternateFileName[14] (UTF-16).
(defconstant +win32-find-data-size+ 592)
(defconstant +find-data-file-name-offset+ 44)
(defconstant +find-data-alternate-name-offset+ 564)

(defun %invalid-handle-p (pointer)
  "True for NULL and INVALID_HANDLE_VALUE (-1)."
  (or (cffi:null-pointer-p pointer)
      (= (cffi:pointer-address pointer)
         (ldb (byte (* 8 (cffi:foreign-type-size :pointer)) 0) -1))))

;;; === R4c-3: ReFS / Dev Drive block cloning (:BLOCK-CLONE) ==========================
;;;
;;; Given, like everything above: the constants, BLOCK-CLONE-ERROR and the
;;; kernel32 bindings the clone code needs. The bindings still signal
;;; RDCW-ERROR off Windows (%DEFINE-KERNEL32), so the :BLOCK-CLONE functions
;;; check %WINDOWS-P first and signal BLOCK-CLONE-ERROR themselves
;;; (block-clone.lisp).

;;; --- FSCTL codes (winioctl.h) -------------------------------------------------------

(defconstant +fsctl-duplicate-extents-to-file+ #x00098344
  "CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 209, METHOD_BUFFERED, FILE_WRITE_DATA).
Sent to the TARGET handle with a DUPLICATE_EXTENTS_DATA naming the source
handle: the target's range then shares the source's clusters (ReFS).")
(defconstant +fsctl-set-sparse+ #x000900C4
  "CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 49, METHOD_BUFFERED, FILE_SPECIAL_ACCESS).
No input buffer = set the sparse attribute.")
(defconstant +fsctl-get-integrity-information+ #x0009027C
  "CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 159, METHOD_BUFFERED, FILE_ANY_ACCESS).
Output: FSCTL_GET_INTEGRITY_INFORMATION_BUFFER { WORD ChecksumAlgorithm;
WORD Reserved; DWORD Flags; DWORD ChecksumChunkSizeInBytes; DWORD
ClusterSizeInBytes; } (16 octets).")
(defconstant +fsctl-set-integrity-information+ #x0009C280
  "CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 160, METHOD_BUFFERED, FILE_READ_DATA |
FILE_WRITE_DATA). Input: FSCTL_SET_INTEGRITY_INFORMATION_BUFFER { WORD
ChecksumAlgorithm; WORD Reserved; DWORD Flags; } (8 octets).")
(defconstant +fsctl-get-retrieval-pointers+ #x00090073
  "CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 28, METHOD_NEITHER, FILE_ANY_ACCESS).
Input: STARTING_VCN_INPUT_BUFFER { LONGLONG StartingVcn; }. Output:
RETRIEVAL_POINTERS_BUFFER { DWORD ExtentCount; LONGLONG StartingVcn;
{ LONGLONG NextVcn; LONGLONG Lcn; } Extents[]; } (StartingVcn at offset 8,
the extents from offset 16; an Lcn of -1 is a hole). ERROR_MORE_DATA asks
for another call from the last NextVcn; ERROR_HANDLE_EOF means no clusters
(an empty or MFT-resident file).")

;;; --- volume flags (GetVolumeInformationW lpFileSystemFlags) ----------------------------

(defconstant +file-supports-block-refcounting+ #x08000000
  "FILE_SUPPORTS_BLOCK_REFCOUNTING: the volume can share clusters between
files (FSCTL_DUPLICATE_EXTENTS_TO_FILE). ReFS v2+, Dev Drive; not NTFS.")

;;; --- more Win32 error codes (winerror.h) -----------------------------------------------

(defconstant +error-not-same-device+ 17)
(defconstant +error-handle-eof+ 38)
(defconstant +error-not-supported+ 50)
(defconstant +error-file-exists+ 80)
(defconstant +error-invalid-parameter+ 87)
(defconstant +error-more-data+ 234)
(defconstant +error-block-too-many-references+ 347)

;;; --- CreateFileW / file attributes / FILE_INFO_BY_HANDLE_CLASS -----------------------

(defconstant +generic-read+ #x80000000)
(defconstant +generic-write+ #x40000000)
(defconstant +delete-access+ #x00010000 "DELETE (winnt.h).")
(defconstant +create-new+ 1)
(defconstant +file-attribute-readonly+ #x1)
(defconstant +file-attribute-normal+ #x80)
(defconstant +file-attribute-sparse-file+ #x200)
(defconstant +file-attribute-integrity-stream+ #x8000)
(defconstant +file-end-of-file-info+ 6
  "FileEndOfFileInfo: SetFileInformationByHandle with FILE_END_OF_FILE_INFO
{ LARGE_INTEGER EndOfFile; } (8 octets).")

(defconstant +duplicate-extents-data-size+ 32
  "sizeof(DUPLICATE_EXTENTS_DATA) on x64: { HANDLE FileHandle;
LARGE_INTEGER SourceFileOffset; LARGE_INTEGER TargetFileOffset;
LARGE_INTEGER ByteCount; }, at offsets 0 / 8 / 16 / 24.")

;;; --- the condition ---------------------------------------------------------------------

(define-condition block-clone-error (sp:clone-error)
  ()
  (:default-initargs :mechanism :block-clone)
  (:documentation "A Win32 call of the :BLOCK-CLONE mechanism failed for a
reason a plain copy may still get past (ERROR_ACCESS_DENIED,
ERROR_DISK_FULL, ERROR_SHARING_VIOLATION, ...): CLONE-ERROR-OPERATION is the
call (\"DeviceIoControl\", \"CreateFileW\", ...), CLONE-ERROR-CODE its
GetLastError, PATH-ERROR-PATH the file. Inside FS-CLONE-TREE /
FS-CLONE-FILE the restart USE-COPY is offered. Also signalled by the
Windows helpers (VOLUME-GUID-PATH, FILE-CLUSTER-RUNS, ...) when their call
fails, and by every :BLOCK-CLONE function off Windows (CODE NIL)."))

(defconstant +file-read-attributes+ #x80
  "FILE_READ_ATTRIBUTES (winnt.h): no data access (FILE-CLUSTER-RUNS).")
(defconstant +file-disposition-info+ 4
  "FileDispositionInfo: SetFileInformationByHandle with FILE_DISPOSITION_INFO
{ BOOLEAN DeleteFile; } (1 octet) on a handle opened with DELETE access: the
file is deleted when the handle closes.")

;;; --- kernel32 (block cloning) ----------------------------------------------------------

(%define-kernel32 (%get-volume-information "GetVolumeInformationW") :boolean
  (root %wstring) (volume-name :pointer) (volume-name-size :uint32)
  (serial :pointer) (max-component :pointer) (flags :pointer)
  (fs-name :pointer) (fs-name-size :uint32))
(%define-kernel32 (%get-volume-name-for-volume-mount-point
                   "GetVolumeNameForVolumeMountPointW") :boolean
  (mount-point %wstring) (buffer :pointer) (length :uint32))
(%define-kernel32 (%get-disk-free-space "GetDiskFreeSpaceW") :boolean
  (root %wstring) (sectors-per-cluster :pointer) (bytes-per-sector :pointer)
  (free-clusters :pointer) (total-clusters :pointer))
(%define-kernel32 (%device-io-control "DeviceIoControl") :boolean
  (handle :pointer) (code :uint32) (in :pointer) (in-size :uint32)
  (out :pointer) (out-size :uint32) (returned :pointer) (overlapped :pointer))
(%define-kernel32 (%set-file-information-by-handle "SetFileInformationByHandle") :boolean
  (handle :pointer) (class :int) (info :pointer) (size :uint32))
(%define-kernel32 (%get-file-size-ex "GetFileSizeEx") :boolean
  (handle :pointer) (size :pointer))
(%define-kernel32 (%delete-file "DeleteFileW") :boolean (name %wstring))
