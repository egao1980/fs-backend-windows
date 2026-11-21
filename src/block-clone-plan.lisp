(in-package #:fs-backend-windows)

;;; Contract stubs: every call of %R4C3-PENDING is replaced by the
;;; implementation, and then %R4C3-PENDING itself is deleted.
(defun %r4c3-pending (what)
  (error "R4c-3 not implemented yet: ~A" what))

;;; R4c-3: the pure half of :BLOCK-CLONE. Nothing in this file calls Win32,
;;; so all of it runs, and is tested, on every OS:
;;;
;;;   file size, cluster size ──BLOCK-CLONE-CHUNKS──▶ ((offset . count) …)
;;;   each chunk ──ENCODE-DUPLICATE-EXTENTS-DATA──▶ the ioctl's 32 octets
;;;   GetVolumeInformationW flags ──VOLUME-FLAGS-BLOCK-CLONE-P──▶ boolean
;;;   GetLastError ──BLOCK-CLONE-ERROR-REASON──▶ a fallback reason, or NIL
;;;                                             (NIL: signal BLOCK-CLONE-ERROR)

(defun block-clone-chunks (size cluster-size &key (max-chunk (expt 2 30)))
  "The FSCTL_DUPLICATE_EXTENTS_TO_FILE calls that clone a file of SIZE
octets on a volume with clusters of CLUSTER-SIZE octets: a list, in file
order, of (OFFSET . BYTE-COUNT), the same offset in source and target.
Offsets are multiples of CLUSTER-SIZE, every BYTE-COUNT is a multiple of
CLUSTER-SIZE and at most MAX-CHUNK, and the chunks cover [0, SIZE) with no
gap or overlap. The last chunk is rounded UP to a cluster boundary (the
ioctl requires cluster-aligned counts and accepts a range past the end of
file, which BLOCK-CLONE-FILE has already set to SIZE). SIZE 0 gives NIL.
SIZE a non-negative integer, CLUSTER-SIZE and MAX-CHUNK positive integers,
MAX-CHUNK a multiple of CLUSTER-SIZE; otherwise TYPE-ERROR, checked even
when SIZE is 0."
  (declare (ignore size cluster-size max-chunk))
  (%r4c3-pending 'block-clone-chunks))

(defun block-clone-error-reason (code)
  "The fallback reason for the Win32 error CODE (GetLastError) of a failed
clone step, or NIL when the failure is not a fallback and BLOCK-CLONE-FILE
signals BLOCK-CLONE-ERROR instead:
  ERROR_INVALID_FUNCTION (1), ERROR_NOT_SUPPORTED (50) → :UNSUPPORTED-VOLUME
  ERROR_NOT_SAME_DEVICE (17)                         → :CROSS-VOLUME
  ERROR_INVALID_PARAMETER (87), ERROR_BLOCK_TOO_MANY_REFERENCES (347)
                                                     → :CLONE-FAILED
  anything else (0, ERROR_ACCESS_DENIED, ERROR_DISK_FULL, …, a non-integer)
                                                     → NIL"
  (declare (ignore code))
  (%r4c3-pending 'block-clone-error-reason))

(defun volume-flags-block-clone-p (flags)
  "True (T) when the lpFileSystemFlags FLAGS of GetVolumeInformationW have
FILE_SUPPORTS_BLOCK_REFCOUNTING (#x08000000), else NIL. FLAGS is a DWORD
(an integer in [0, 2^32)); otherwise TYPE-ERROR."
  (declare (ignore flags))
  (%r4c3-pending 'volume-flags-block-clone-p))

(defun encode-duplicate-extents-data (handle-value source-offset target-offset byte-count)
  "The DUPLICATE_EXTENTS_DATA input buffer of FSCTL_DUPLICATE_EXTENTS_TO_FILE
on x64: a fresh (simple-array (unsigned-byte 8) (32)), little-endian,
FileHandle (the SOURCE handle's value, 8 octets) at 0, SourceFileOffset at 8,
TargetFileOffset at 16, ByteCount at 24 (LARGE_INTEGERs). HANDLE-VALUE is an
integer in [0, 2^64), the offsets and BYTE-COUNT integers in [0, 2^63);
otherwise TYPE-ERROR."
  (declare (ignore handle-value source-offset target-offset byte-count))
  (%r4c3-pending 'encode-duplicate-extents-data))
