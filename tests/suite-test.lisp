(in-package #:fs-backend-windows/tests)

;;; The R4b law (cl-stack-pathlib/journal-suite): an area with an :rdcw
;;; journal gives exactly the full rescan's changes after every corpus step.

(deftest-parametrize rdcw-journal-suite
    ((settle) :ids ("default-settle" "settle-0") (0.05) (0))
  (if (not (windows-p))
      (skip +skip-note+)
      (multiple-value-bind (failures steps) (run-journal-suite :rdcw :settle settle)
        (ok (null failures) (format nil ":rdcw keeps the law: ~S" failures))
        (ok (equal (journal-suite-steps :backend :local) steps) "every applicable step ran")
        (ok (member :case-only-rename steps))
        (ok (member :burst steps))
        (ok (member :child-process steps)))))

(deftest-windows rdcw-journal-suite-auto
  ;; :auto picks :rdcw (the best available mechanism on Windows).
  (ok (null (run-journal-suite :auto))))
