(in-package #:fs-backend-windows/tests)

;;; The R4c law (cl-stack-pathlib/clone-suite) with :block-clone on a real
;;; ReFS volume: equivalence with :copy, independence, metadata, and the
;;; cross-volume fallback against the NTFS temp directory.

(deftest-refs block-clone-suite-cross-volume
  (let ((other (uiop:temporary-directory)))
    (multiple-value-bind (failures steps)
        (run-clone-suite :block-clone :directory (refs-root) :other-volume other)
      (ok (null failures) (format nil ":block-clone keeps the law: ~S" failures))
      (ok (equal (clone-suite-steps :mechanism :block-clone :other-volume other) steps)
          "every applicable step ran")
      (ok (member :cross-volume steps))
      (ok (member :sparse steps))
      (ok (member :equivalence steps)))))

(deftest-refs block-clone-suite
  (multiple-value-bind (failures steps) (run-clone-suite :block-clone :directory (refs-root))
    (ok (null failures) (format nil ":block-clone keeps the law: ~S" failures))
    (ok (equal (clone-suite-steps :mechanism :block-clone) steps))))

;;; Dev Drive (ReFS with the same flag), when the `refs` job could make one:
;;; the suite there, and ReFS ↔ Dev Drive as two cloning volumes.

(deftest-devdrive block-clone-suite-devdrive
  (with-devdrive-dir (dir)
    (ok (volume-supports-block-clone-p dir) "a Dev Drive has block refcounting")
    (ok (equal '(t) (multiple-value-list (block-clone-available-p *fs* dir dir)))))
  (multiple-value-bind (failures steps)
      (run-clone-suite :block-clone :directory (devdrive-root) :other-volume (refs-root))
    (ok (null failures) (format nil "Dev Drive, other volume ReFS: ~S" failures))
    (ok (member :cross-volume steps))))

(deftest-devdrive block-clone-refs-to-devdrive
  (with-refs-dir (refs)
    (with-devdrive-dir (dev)
      (ok (equal '(nil :cross-volume)
                 (multiple-value-list (block-clone-available-p *fs* refs dev)))
          "two cloning volumes: the GUID paths differ")
      (let ((src (write-octets (sub refs "a.bin") (pseudo-random-octets 100000)))
            (dst (sub dev "a.bin")))
        (ok (equal '(nil :clone-failed) (clone-direct src dst))
            "ReFS → Dev Drive: the ioctl answers ERROR_INVALID_PARAMETER")
        (ok (absent-p dst))))))
