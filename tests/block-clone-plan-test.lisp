(in-package #:fs-backend-windows/tests)

;;; R4c-3, pure (every OS): the chunk plan, the error table, the volume
;;; flags and the DUPLICATE_EXTENTS_DATA encoding.

(defconstant +mib+ (* 1024 1024))
(defconstant +gib+ (* 1024 1024 1024))

(deftest-parametrize block-clone-chunks-cases
    ((size cluster expected)
     :ids ("empty" "one-octet" "one-cluster" "cluster-plus-one" "two-clusters"
           "64k-one-octet" "64k-cluster-plus-one" "64k-exact")
     (0 4096 nil)
     (1 4096 (quote ((0 . 4096))))
     (4096 4096 (quote ((0 . 4096))))
     (4097 4096 (quote ((0 . 8192))))
     (8192 4096 (quote ((0 . 8192))))
     (1 65536 (quote ((0 . 65536))))
     (65537 65536 (quote ((0 . 131072))))
     (65536 65536 (quote ((0 . 65536)))))
  (ok (equal expected (block-clone-chunks size cluster))))

(deftest block-clone-chunks-max-chunk
  (let ((three (block-clone-chunks (* 3 +gib+) 4096 :max-chunk +gib+)))
    (ok (equal `((0 . ,+gib+) (,+gib+ . ,+gib+) (,(* 2 +gib+) . ,+gib+)) three)
        "3 GiB in 1 GiB chunks")
    (ok (equal three (block-clone-chunks (* 3 +gib+) 4096)) "the default MAX-CHUNK is 1 GiB"))
  (ok (equal `((0 . ,+gib+) (,+gib+ . ,+gib+) (,(* 2 +gib+) . ,+gib+) (,(* 3 +gib+) . 4096))
             (block-clone-chunks (1+ (* 3 +gib+)) 4096 :max-chunk +gib+))
      "a tail octet past 3 GiB: a fourth chunk of one cluster")
  (ok (equal `((0 . ,+gib+) (,+gib+ . 131072))
             (block-clone-chunks (+ +gib+ 65536 5) 65536 :max-chunk +gib+))
      "64 KiB clusters: the tail rounded up to two clusters")
  (ok (equal '((0 . 8192) (8192 . 8192) (16384 . 4096))
             (block-clone-chunks 20000 4096 :max-chunk 8192))
      "a small MAX-CHUNK"))

(deftest block-clone-chunks-laws
  ;; For many sizes: aligned, contiguous from 0, each ≤ MAX-CHUNK, covering
  ;; exactly the size rounded up to a cluster.
  (let ((x 7))
    (dolist (cluster '(4096 65536))
      (dolist (max-chunk (list cluster (* 4 cluster) (* 16 +mib+)))
        (dotimes (i 200)
          (let* ((size (mod (setf x (logand #xFFFFFFFF (+ (* x 1664525) 1013904223))) (* 40 +mib+)))
                 (chunks (block-clone-chunks size cluster :max-chunk max-chunk))
                 (rounded (* cluster (ceiling size cluster))))
            (unless (and (loop for expect = 0 then (+ off n)
                               for (off . n) in chunks
                               always (and (= off expect) (zerop (mod off cluster))
                                           (zerop (mod n cluster)) (< 0 n) (<= n max-chunk)))
                         (= rounded (loop for (nil . n) in chunks sum n))
                         (eq (null chunks) (zerop size)))
              (fail (format nil "bad plan for size ~D cluster ~D max ~D: ~S"
                            size cluster max-chunk chunks))
              (return)))))))
  (pass "chunk laws checked"))

(deftest block-clone-chunks-type-errors
  (ok (signals (block-clone-chunks 10 4096 :max-chunk 5000) 'type-error)
      "MAX-CHUNK not a multiple of the cluster size")
  (ok (signals (block-clone-chunks 0 4096 :max-chunk 5000) 'type-error)
      "checked even for an empty file")
  (ok (signals (block-clone-chunks 10 4096 :max-chunk 0) 'type-error))
  (ok (signals (block-clone-chunks 10 0) 'type-error))
  (ok (signals (block-clone-chunks -1 4096) 'type-error))
  (ok (signals (block-clone-chunks 1.5 4096) 'type-error)))

(defparameter +block-clone-error-table+
  '((1 . :unsupported-volume) (50 . :unsupported-volume)
    (17 . :cross-volume)
    (87 . :clone-failed) (347 . :clone-failed)))

(deftest block-clone-error-reasons
  (loop for (code . reason) in +block-clone-error-table+
        do (ok (eq reason (block-clone-error-reason code)) (format nil "~D → ~S" code reason)))
  ;; the other way: exactly these codes give a reason
  (loop for reason in '(:unsupported-volume :cross-volume :clone-failed)
        do (ok (equal (loop for (code . r) in +block-clone-error-table+
                            when (eq r reason) collect code)
                      (loop for code from 0 below 2000
                            when (eq reason (block-clone-error-reason code)) collect code))
               (format nil "only the locked codes give ~S" reason)))
  (dolist (code '(0 2 3 5 32 80 112 1450))
    (ok (null (block-clone-error-reason code))
        (format nil "~D (access denied, disk full, sharing, …) → NIL: signal" code)))
  (ok (null (block-clone-error-reason nil)))
  (ok (null (block-clone-error-reason :access-denied))))

(deftest block-clone-volume-flags
  (ok (eq t (volume-flags-block-clone-p #x08000000)))
  (ok (eq t (volume-flags-block-clone-p (logior #x08000000 #x03E706FF))) "ReFS-like")
  (ok (eq t (volume-flags-block-clone-p #xFFFFFFFF)))
  (ok (null (volume-flags-block-clone-p 0)))
  (ok (null (volume-flags-block-clone-p #x03E706FF)) "NTFS-like (no block refcounting)")
  (ok (null (volume-flags-block-clone-p #xF7FFFFFF)) "every flag but that one")
  (ok (signals (volume-flags-block-clone-p -1) 'type-error))
  (ok (signals (volume-flags-block-clone-p (expt 2 32)) 'type-error))
  (ok (signals (volume-flags-block-clone-p nil) 'type-error)))

(defun %le (n count)
  (loop for i below count collect (ldb (byte 8 (* 8 i)) n)))

(deftest block-clone-duplicate-extents-data
  (let ((v (encode-duplicate-extents-data #x1122334455667788 #x0102 #x0A0B0C0D #x7000000000000001)))
    (ok (typep v '(simple-array (unsigned-byte 8) (32))) "32 octets")
    (ok (equal (%le #x1122334455667788 8) (coerce (subseq v 0 8) 'list)) "FileHandle at 0")
    (ok (equal (%le #x0102 8) (coerce (subseq v 8 16) 'list)) "SourceFileOffset at 8")
    (ok (equal (%le #x0A0B0C0D 8) (coerce (subseq v 16 24) 'list)) "TargetFileOffset at 16")
    (ok (equal (%le #x7000000000000001 8) (coerce (subseq v 24 32) 'list)) "ByteCount at 24"))
  (ok (equalp (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
              (encode-duplicate-extents-data 0 0 0 0)))
  (ok (not (eq (encode-duplicate-extents-data 1 0 0 4096) (encode-duplicate-extents-data 1 0 0 4096)))
      "a fresh vector each call")
  (ok (equalp (encode-duplicate-extents-data 1 4096 4096 65536)
              (encode-duplicate-extents-data 1 4096 4096 65536)))
  (ok (signals (encode-duplicate-extents-data -1 0 0 0) 'type-error))
  (ok (signals (encode-duplicate-extents-data (expt 2 64) 0 0 0) 'type-error))
  (ok (signals (encode-duplicate-extents-data 1 -4096 0 4096) 'type-error))
  (ok (signals (encode-duplicate-extents-data 1 0 (expt 2 63) 4096) 'type-error))
  (ok (signals (encode-duplicate-extents-data 1 0 0 (expt 2 63)) 'type-error)))
