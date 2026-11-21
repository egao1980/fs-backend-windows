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

;;; --- Windows helpers ----------------------------------------------------------------

(defun volume-supports-block-clone-p (directory)
  "Windows: true when the volume of the existing DIRECTORY (a pathname or
native namestring) has FILE_SUPPORTS_BLOCK_REFCOUNTING: GetVolumePathNameW
→ GetVolumeInformationW → VOLUME-FLAGS-BLOCK-CLONE-P. Signals
BLOCK-CLONE-ERROR when a call fails, and off Windows."
  (%block-clone-off-windows "GetVolumeInformationW" directory)
  (%r4c3-pending 'volume-supports-block-clone-p))

(defun volume-guid-path (directory)
  "Windows: the volume GUID path (\"\\\\?\\Volume{…}\\\") of the volume
holding the existing DIRECTORY: GetVolumePathNameW (the mount point, e.g.
\"C:\\\") → GetVolumeNameForVolumeMountPointW. Two directories are on one
volume exactly when their GUID paths are STRING-EQUAL, whatever the
spelling (drive letter case, a subdirectory, an 8.3 name, a mount point).
Signals BLOCK-CLONE-ERROR when a call fails, and off Windows."
  (%block-clone-off-windows "GetVolumeNameForVolumeMountPointW" directory)
  (%r4c3-pending 'volume-guid-path))

(defun volume-cluster-size (directory)
  "Windows: the cluster size in octets of the volume holding DIRECTORY
(GetDiskFreeSpaceW: sectors per cluster × bytes per sector; 4096 or 65536 on
ReFS). Signals BLOCK-CLONE-ERROR when the call fails, and off Windows."
  (%block-clone-off-windows "GetDiskFreeSpaceW" directory)
  (%r4c3-pending 'volume-cluster-size))

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
  (%r4c3-pending 'file-cluster-runs))

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
  (declare (ignorable fs destination))
  (%block-clone-off-windows "CreateFileW" source)
  (%r4c3-pending 'block-clone-file))
