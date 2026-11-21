(defpackage #:fs-backend-windows
  (:use #:cl)
  (:local-nicknames (#:sp #:cl-stack-pathlib))
  (:export
   ;; the mechanism
   #:+rdcw-rank+
   #:rdcw-available-p
   #:make-rdcw-journal
   #:rdcw-journal
   #:rdcw-journal-thread
   #:rdcw-journal-handle
   #:rdcw-journal-buffer-size
   #:rdcw-handle-count
   #:*rdcw-buffer-size*
   #:*rdcw-barrier-timeout*
   ;; test hooks
   #:rdcw-journal-inject
   #:rdcw-journal-pause
   #:rdcw-journal-resume
   ;; Windows helpers (signal RDCW-ERROR off Windows)
   #:short-path-name
   #:process-handle-count
   ;; pure, every OS: FILE_NOTIFY_INFORMATION buffers
   #:utf16le-octets-to-string
   #:parse-file-notify-information
   #:rdcw-action-keyword
   ;; pure, every OS: names and paths
   #:rdcw-relative-path
   #:rdcw-fold-key
   #:rdcw-short-name-p
   #:rdcw-resolve-path
   #:rdcw-resolve-paths
   #:rdcw-same-final-path-p
   ;; pure, every OS: the accumulator (pending paths + incomplete / lost marks)
   #:rdcw-accumulator
   #:make-rdcw-accumulator
   #:rdcw-accumulate
   #:rdcw-accumulator-take
   #:rdcw-accumulator-mark-lost
   #:rdcw-accumulator-lost-p
   ;; pure, every OS: the drain barrier
   #:rdcw-barrier
   #:make-rdcw-barrier
   #:rdcw-barrier-request
   #:rdcw-barrier-requested
   #:rdcw-barrier-ack
   #:rdcw-barrier-wait
   #:rdcw-barrier-fail
   ;; conditions
   #:rdcw-error
   #:rdcw-error-operation
   #:rdcw-error-code
   #:malformed-notify-buffer
   #:malformed-notify-buffer-offset
   #:malformed-notify-buffer-reason
   ;; FILE_ACTION_* (winnt.h)
   #:+file-action-added+
   #:+file-action-removed+
   #:+file-action-modified+
   #:+file-action-renamed-old-name+
   #:+file-action-renamed-new-name+
   ;; FILE_NOTIFY_CHANGE_* (winnt.h)
   #:+file-notify-change-file-name+
   #:+file-notify-change-dir-name+
   #:+file-notify-change-attributes+
   #:+file-notify-change-size+
   #:+file-notify-change-last-write+
   #:+file-notify-change-last-access+
   #:+file-notify-change-creation+
   #:+file-notify-change-security+
   #:+rdcw-notify-filter+
   ;; CreateFileW
   #:+file-list-directory+
   #:+file-share-read+
   #:+file-share-write+
   #:+file-share-delete+
   #:+rdcw-share-mode+
   #:+open-existing+
   #:+file-flag-backup-semantics+
   #:+file-flag-overlapped+
   #:+rdcw-create-flags+
   ;; Win32 error codes
   #:+error-success+
   #:+error-file-not-found+
   #:+error-path-not-found+
   #:+error-access-denied+
   #:+error-invalid-handle+
   #:+error-invalid-function+
   #:+error-operation-aborted+
   #:+error-io-incomplete+
   #:+error-io-pending+
   #:+error-notify-enum-dir+
   #:+error-notify-cleanup+
   ;; R4c-3: the clone mechanism :block-clone
   #:+block-clone-rank+
   #:block-clone-available-p
   #:block-clone-file
   #:block-clone-error
   ;; pure, every OS: the chunk plan, error mapping, volume flags, ioctl input
   #:block-clone-chunks
   #:block-clone-error-reason
   #:volume-flags-block-clone-p
   #:encode-duplicate-extents-data
   ;; Windows helpers (signal BLOCK-CLONE-ERROR off Windows)
   #:volume-supports-block-clone-p
   #:volume-guid-path
   #:volume-cluster-size
   #:file-cluster-runs
   ;; FSCTL_* (winioctl.h), volume flags
   #:+fsctl-duplicate-extents-to-file+
   #:+fsctl-set-sparse+
   #:+fsctl-get-integrity-information+
   #:+fsctl-set-integrity-information+
   #:+fsctl-get-retrieval-pointers+
   #:+file-supports-block-refcounting+
   ;; Win32 error codes of the clone
   #:+error-not-same-device+
   #:+error-not-supported+
   #:+error-invalid-parameter+
   #:+error-block-too-many-references+))
