(in-package #:fs-backend-windows)

;;; R4c-3: the :BLOCK-CLONE clone mechanism (ReFS / Dev Drive block cloning)
;;; for cl-stack-pathlib 0.10.0's clone protocol. pathlib walks the tree and
;;; calls BLOCK-CLONE-FILE for each regular file; this file is the Win32
;;; glue under the pure plan in block-clone-plan.lisp.
;;;
;;; Availability (BLOCK-CLONE-AVAILABLE-P, given below): Windows, the local
;;; filesystem, a source volume with FILE_SUPPORTS_BLOCK_REFCOUNTING, and
;;; both directories on that one volume (equal volume GUID paths).
;;;
;;; Per file (BLOCK-CLONE-FILE), in this order:
;;;   1. CreateFileW SOURCE: GENERIC_READ, share READ, OPEN_EXISTING.
;;;   2. CreateFileW DESTINATION: GENERIC_READ | GENERIC_WRITE | DELETE,
;;;      share 0, CREATE_NEW.
;;;   3. SOURCE has FILE_ATTRIBUTE_SPARSE_FILE → FSCTL_SET_SPARSE on
;;;      DESTINATION (ReFS requires the sparse states to match).
;;;   4. FSCTL_GET_INTEGRITY_INFORMATION on SOURCE → FSCTL_SET_INTEGRITY_
;;;      INFORMATION on DESTINATION with its ChecksumAlgorithm and Flags (the
;;;      integrity states must match too).
;;;   5. SetFileInformationByHandle FileEndOfFileInfo: DESTINATION's end of
;;;      file = SOURCE's size (GetFileSizeEx).
;;;   6. FSCTL_DUPLICATE_EXTENTS_TO_FILE on DESTINATION, one per chunk of
;;;      (BLOCK-CLONE-CHUNKS size (VOLUME-CLUSTER-SIZE source-dir)), each
;;;      with (ENCODE-DUPLICATE-EXTENTS-DATA source-handle offset offset count).
;;;   7. CloseHandle both, always (UNWIND-PROTECT).
;;; A 0-octet SOURCE takes no ioctl (steps 3-6 are skipped). Any failure
;;; deletes DESTINATION (DeleteFileW, or the DELETE access on the handle) and
;;; maps GetLastError with BLOCK-CLONE-ERROR-REASON: a reason → (values NIL
;;; reason), and pathlib copies the file; NIL → BLOCK-CLONE-ERROR (pathlib
;;; offers USE-COPY).
;;;
;;; Names go to Win32 as UTF-16LE (%WSTRING), so a non-BMP name clones,
;;; which SBCL's own OPEN cannot do. Paths are native namestrings of the
;;; absolute pathnames pathlib passes.
;;;
;;; Platform facts (windows-latest, Windows Server 2025, ReFS 3.14, checked
;;; while writing this contract): diskpart formats ReFS on a VHDX, and
;;; Format-Volume -DevDrive works; ReFS and Dev Drive report
;;; FILE_SUPPORTS_BLOCK_REFCOUNTING, NTFS does not; the ioctl on an NTFS
;;; target answers ERROR_INVALID_FUNCTION (1), also when the source is on
;;; ReFS (the ioctl goes to the target's file system), so only the probe's
;;; GUID comparison says :CROSS-VOLUME for ReFS → NTFS; a 1-octet file
;;; clones with a count rounded up to one cluster; FSCTL_GET_RETRIEVAL_
;;; POINTERS works on ReFS and shows the clone on the source's LCNs; a write into
;;; a shared cluster gets a new LCN only once it is written back
;;; (FlushFileBuffers), so tests flush before they look; free space is no
;;; proof of sharing (ReFS allocates on the end-of-file extension of a
;;; non-sparse target and releases it lazily: a 64 MiB clone showed ~64 MiB
;;; less free space right after the ioctl); a sparse source and a
;;; non-sparse target fail with ERROR_NOT_SUPPORTED (50), hence step 3;
;;; ReFS → another ReFS volume (a Dev Drive) answers ERROR_INVALID_PARAMETER
;;; (87), not ERROR_NOT_SAME_DEVICE, so again only the probe says
;;; :CROSS-VOLUME; the tail cluster of a 1-octet clone was not seen on the
;;; source's LCN, so the tests prove sharing on whole clusters only.

(defconstant +block-clone-rank+ 100
  "The rank :BLOCK-CLONE registers with (above :COPY, which is not ranked).")

(defun %block-clone-off-windows (operation path)
  "Signal BLOCK-CLONE-ERROR for OPERATION off Windows (no Win32 here)."
  (unless (%windows-p)
    (error 'block-clone-error :operation operation :path path :code nil
                              :message "block cloning needs Windows (ReFS / Dev Drive)")))

;;; --- internals ----------------------------------------------------------------------

(defun %bc-fail (operation code path)
  "Signal BLOCK-CLONE-ERROR: the Win32 call OPERATION failed with
GetLastError CODE (or NIL) for PATH."
  (error 'block-clone-error :operation operation :code code :path path))

(defmacro %bc-check ((operation path &key (test 'identity)) form)
  "FORM's value when TEST accepts it, else BLOCK-CLONE-ERROR with the
GetLastError read as the next foreign call after FORM."
  (let ((value (gensym "VALUE")) (code (gensym "CODE")))
    `(let* ((,value ,form)
            (,code (%get-last-error)))
       (if (funcall #',test ,value)
           ,value
           (%bc-fail ,operation ,code ,path)))))

(defun %bc-valid-handle-p (handle)
  (not (%invalid-handle-p handle)))

(defun %bc-valid-attributes-p (attributes)
  (/= attributes +invalid-file-attributes+))

(defun %bc-mark-delete (handle)
  "FileDispositionInfo DeleteFile = TRUE on HANDLE (opened with DELETE
access): the file goes away when HANDLE is closed, with no second lookup of
its name. True on success; never signals (a cleanup step)."
  (cffi:with-foreign-object (flag :uint8)
    (setf (cffi:mem-ref flag :uint8) 1)
    (%set-file-information-by-handle handle +file-disposition-info+ flag 1)))

(defun %bc-mount-point (directory)
  "GetVolumePathNameW: the mount point (e.g. \"C:\\\\\") of DIRECTORY, a
pathname or native namestring of an existing file or directory."
  (%with-foreign-buffer (buffer :uint16 +rdcw-wide-path-length+)
    (%bc-check ("GetVolumePathNameW" directory)
      (%get-volume-path-name (%native directory) buffer +rdcw-wide-path-length+))
    (or (%wide-z buffer 0 +rdcw-wide-path-length+)
        (%bc-fail "GetVolumePathNameW" nil directory))))

(defconstant +volume-guid-path-length+ 64
  "WCHARs for \"\\\\?\\Volume{GUID}\\\" (49) and the NUL, rounded up.")

(defconstant +retrieval-buffer-octets+ 4096
  "The RETRIEVAL_POINTERS_BUFFER of one FSCTL_GET_RETRIEVAL_POINTERS call:
the 16-octet header and up to 255 extents.")

;;; --- Windows helpers ----------------------------------------------------------------

(defun volume-supports-block-clone-p (directory)
  "Windows: true when the volume of the existing DIRECTORY (a pathname or
native namestring) has FILE_SUPPORTS_BLOCK_REFCOUNTING: GetVolumePathNameW
→ GetVolumeInformationW → VOLUME-FLAGS-BLOCK-CLONE-P. Signals
BLOCK-CLONE-ERROR when a call fails, and off Windows."
  (%block-clone-off-windows "GetVolumeInformationW" directory)
  (let ((root (%bc-mount-point directory)))
    (cffi:with-foreign-object (flags :uint32)
      (%bc-check ("GetVolumeInformationW" directory)
        (%get-volume-information root (cffi:null-pointer) 0 (cffi:null-pointer)
                                 (cffi:null-pointer) flags (cffi:null-pointer) 0))
      (volume-flags-block-clone-p (cffi:mem-ref flags :uint32)))))

(defun volume-guid-path (directory)
  "Windows: the volume GUID path (\"\\\\?\\Volume{…}\\\") of the volume
holding the existing DIRECTORY: GetVolumePathNameW (the mount point, e.g.
\"C:\\\") → GetVolumeNameForVolumeMountPointW. Two directories are on one
volume exactly when their GUID paths are STRING-EQUAL, whatever the
spelling (drive letter case, a subdirectory, an 8.3 name, a mount point).
Signals BLOCK-CLONE-ERROR when a call fails, and off Windows."
  (%block-clone-off-windows "GetVolumeNameForVolumeMountPointW" directory)
  (let ((root (%bc-mount-point directory)))
    (%with-foreign-buffer (buffer :uint16 +volume-guid-path-length+)
      (%bc-check ("GetVolumeNameForVolumeMountPointW" directory)
        (%get-volume-name-for-volume-mount-point root buffer +volume-guid-path-length+))
      (or (%wide-z buffer 0 +volume-guid-path-length+)
          (%bc-fail "GetVolumeNameForVolumeMountPointW" nil directory)))))

(defun volume-cluster-size (directory)
  "Windows: the cluster size in octets of the volume holding DIRECTORY
(GetDiskFreeSpaceW: sectors per cluster × bytes per sector; 4096 or 65536 on
ReFS). Signals BLOCK-CLONE-ERROR when the call fails, and off Windows."
  (%block-clone-off-windows "GetDiskFreeSpaceW" directory)
  (let ((root (%bc-mount-point directory)))
    (cffi:with-foreign-objects ((sectors :uint32) (octets :uint32)
                                (free :uint32) (total :uint32))
      (%bc-check ("GetDiskFreeSpaceW" directory)
        (%get-disk-free-space root sectors octets free total))
      (* (cffi:mem-ref sectors :uint32) (cffi:mem-ref octets :uint32)))))

(defun file-cluster-runs (pathname)
  "Windows, for tests: the cluster runs of the file PATHNAME (a pathname or
native namestring) through FSCTL_GET_RETRIEVAL_POINTERS, repeated while it
answers ERROR_MORE_DATA: a list, in VCN order, of (VCN LCN COUNT), LCN -1 for
a hole (sparse). NIL for a file with no clusters (ERROR_HANDLE_EOF: empty,
or resident in the MFT on NTFS). The file is opened with no data access
(FILE_READ_ATTRIBUTES), share read / write / delete. Signals
BLOCK-CLONE-ERROR when a call fails, and off Windows. A block clone's runs
map to the same LCNs as its source's; a copy's do not."
  (%block-clone-off-windows "DeviceIoControl" pathname)
  (let ((handle (%bc-check ("CreateFileW" pathname :test %bc-valid-handle-p)
                  (%create-file (%native pathname) +file-read-attributes+ +rdcw-share-mode+
                                (cffi:null-pointer) +open-existing+ 0 (cffi:null-pointer)))))
    (unwind-protect
         (cffi:with-foreign-objects ((start :int64) (returned :uint32))
           (%with-foreign-buffer (out :uint8 +retrieval-buffer-octets+)
             (let ((runs '()) (next-start 0))
               (loop
                 (setf (cffi:mem-ref start :int64) next-start)
                 (let* ((ok (%device-io-control handle +fsctl-get-retrieval-pointers+
                                                start 8 out +retrieval-buffer-octets+
                                                returned (cffi:null-pointer)))
                        (code (if ok +error-success+ (%get-last-error))))
                   (cond ((or ok (eql code +error-more-data+))
                          (let ((extents (cffi:mem-ref out :uint32 0))
                                (vcn (cffi:mem-ref out :int64 8)))
                            (when (and (not ok) (zerop extents)) ; no progress: never spin
                              (%bc-fail "DeviceIoControl" code pathname))
                            (dotimes (k extents)
                              (let ((next (cffi:mem-ref out :int64 (+ 16 (* 16 k))))
                                    (lcn (cffi:mem-ref out :int64 (+ 24 (* 16 k))))) ; hole: -1
                                (push (list vcn lcn (- next vcn)) runs)
                                (setf vcn next)))
                            (when ok (return (nreverse runs)))
                            (setf next-start vcn)))   ; ERROR_MORE_DATA: from the last NextVcn
                         ((eql code +error-handle-eof+) (return (nreverse runs)))
                         (t (%bc-fail "DeviceIoControl" code pathname))))))))
      (%close-handle handle))))

;;; --- the mechanism ------------------------------------------------------------------

(defun block-clone-available-p (fs source-dir destination-dir)
  "The AVAILABLE-P probe of :BLOCK-CLONE, checked in this order: off
Windows (NIL :UNAVAILABLE); FS not a LOCAL-FILESYSTEM (NIL
:UNSUPPORTED-FILESYSTEM); SOURCE-DIR's volume without block refcounting
(NIL :UNSUPPORTED-VOLUME); SOURCE-DIR and DESTINATION-DIR on different
volumes (NIL :CROSS-VOLUME); else T. Both directories exist (pathlib passes
the nearest existing ancestor of a destination)."
  (cond ((not (%windows-p)) (values nil :unavailable))
        ((not (typep fs 'sp:local-filesystem)) (values nil :unsupported-filesystem))
        ((not (volume-supports-block-clone-p source-dir)) (values nil :unsupported-volume))
        ((not (string-equal (volume-guid-path source-dir) (volume-guid-path destination-dir)))
         (values nil :cross-volume))
        (t t)))

(defun block-clone-file (fs source destination)
  "The per-file CLONE-FILE function of :BLOCK-CLONE (steps above): SOURCE an
absolute pathname of a regular file, DESTINATION absent with its directory
existing. → T when DESTINATION holds SOURCE's content and shares its
clusters; (values NIL reason) when it made nothing and pathlib should copy
(BLOCK-CLONE-ERROR-REASON); signals BLOCK-CLONE-ERROR (operation, code,
path = SOURCE) for any other failure, DESTINATION deleted first. A
DESTINATION that already exists is not this call's: CREATE_NEW fails with
ERROR_FILE_EXISTS (80), BLOCK-CLONE-ERROR is signalled and the file is left
as it was. Signals BLOCK-CLONE-ERROR off Windows."
  (declare (ignorable fs))
  (%block-clone-off-windows "CreateFileW" source)
  ;; Invariant: on every exit DESTINATION exists iff T is returned or it
  ;; existed before the call; both handles are closed and every foreign
  ;; buffer freed. A Win32 failure records (operation code), GetLastError
  ;; read as the next foreign call, and leaves BLOCK CLONE; the cleanup
  ;; deletes a destination this call created (CREATED) unless SUCCESS, so an
  ;; unwind for any other reason removes it too, and the code is mapped only
  ;; after both handles are closed.
  (let ((src-name (%native source)) (dst-name (%native destination))
        (no-pointer (cffi:null-pointer))
        (src nil) (dst nil) (created nil) (success nil) (failure nil))
    (block clone
      (macrolet ((try ((operation &key (test 'identity)) form)
                   (let ((value (gensym "VALUE")) (code (gensym "CODE")))
                     `(let* ((,value ,form)
                             (,code (%get-last-error)))
                        (if (funcall #',test ,value)
                            ,value
                            (progn (setf failure (list ,operation ,code))
                                   (return-from clone)))))))
        (cffi:with-foreign-objects ((returned :uint32) (size :int64)
                                    (integrity :uint8 16) (set-integrity :uint8 8)
                                    (extents :uint8 +duplicate-extents-data-size+))
          (unwind-protect
               (progn
                 ;; 1. SOURCE
                 (setf src (try ("CreateFileW" :test %bc-valid-handle-p)
                             (%create-file src-name +generic-read+ +file-share-read+ no-pointer
                                           +open-existing+ +file-attribute-normal+ no-pointer)))
                 ;; 2. DESTINATION, CREATE_NEW: from here on it is this call's
                 (setf dst (try ("CreateFileW" :test %bc-valid-handle-p)
                             (%create-file dst-name
                                           (logior +generic-read+ +generic-write+ +delete-access+)
                                           0 no-pointer +create-new+ +file-attribute-normal+
                                           no-pointer))
                       created t)
                 (try ("GetFileSizeEx") (%get-file-size-ex src size))
                 (let ((octets (cffi:mem-ref size :int64)))
                   (when (plusp octets)    ; 0 octets: steps 3-6 skipped, no ioctl
                     ;; 3. sparse states match
                     (let ((attributes (try ("GetFileAttributesW"
                                             :test %bc-valid-attributes-p)
                                         (%get-file-attributes src-name))))
                       (when (logtest attributes +file-attribute-sparse-file+)
                         (try ("DeviceIoControl")
                           (%device-io-control dst +fsctl-set-sparse+ no-pointer 0
                                               no-pointer 0 returned no-pointer))))
                     ;; 4. integrity states match: ChecksumAlgorithm (WORD @0), Flags (DWORD @4)
                     (try ("DeviceIoControl")
                       (%device-io-control src +fsctl-get-integrity-information+ no-pointer 0
                                           integrity 16 returned no-pointer))
                     (setf (cffi:mem-ref set-integrity :uint16 0)
                           (cffi:mem-ref integrity :uint16 0)
                           (cffi:mem-ref set-integrity :uint16 2) 0
                           (cffi:mem-ref set-integrity :uint32 4)
                           (cffi:mem-ref integrity :uint32 4))
                     (try ("DeviceIoControl")
                       (%device-io-control dst +fsctl-set-integrity-information+
                                           set-integrity 8 no-pointer 0 returned no-pointer))
                     ;; 5. end of file = SOURCE's size (SIZE holds the int64)
                     (try ("SetFileInformationByHandle")
                       (%set-file-information-by-handle dst +file-end-of-file-info+ size 8))
                     ;; 6. one ioctl per chunk; the count of the last is rounded up
                     (let ((handle-value (cffi:pointer-address src)))
                       (loop for (offset . byte-count)
                               in (block-clone-chunks octets (volume-cluster-size source))
                             do (let ((data (encode-duplicate-extents-data
                                             handle-value offset offset byte-count)))
                                  (dotimes (i +duplicate-extents-data-size+)
                                    (setf (cffi:mem-aref extents :uint8 i) (aref data i))))
                                (try ("DeviceIoControl")
                                  (%device-io-control dst +fsctl-duplicate-extents-to-file+
                                                      extents +duplicate-extents-data-size+
                                                      no-pointer 0 returned no-pointer))))))
                 (setf success t))
            ;; 7. always: mark ours for deletion unless it is done, close both,
            ;; and fall back to DeleteFileW (after the close: share 0).
            (let ((marked nil))
              (when dst
                (when (and created (not success))
                  (setf marked (%bc-mark-delete dst)))
                (%close-handle dst))
              (when src (%close-handle src))
              (when (and created (not success) (not marked))
                (%delete-file dst-name)))))))
    (if success
        t
        ;; Only a normal exit from BLOCK CLONE gets here without SUCCESS, and
        ;; that is a recorded failure; any other unwind has passed through.
        (destructuring-bind (operation code) failure
          (let ((reason (block-clone-error-reason code)))
            (if reason
                (values nil reason)
                (%bc-fail operation code source)))))))
