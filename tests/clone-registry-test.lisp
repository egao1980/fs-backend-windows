(in-package #:fs-backend-windows/tests)

;;; R4c-3 on every OS: the registration, the constants, the condition, the
;;; portable branches of the probe, and the off-Windows behaviour.

(deftest block-clone-registered
  (let ((names (sp:clone-mechanisms)))
    (ok (member :block-clone names) "loading fs-backend-windows registers :block-clone")
    (ok (eq :copy (first (last names))) ":copy is still last")
    (ok (< (position :block-clone names) (position :copy names)) ":block-clone ranks above :copy")
    (ok (= 100 +block-clone-rank+)))
  (ok (member :rdcw (sp:watch-mechanisms)) ":rdcw is still registered beside it"))

(deftest block-clone-constants
  (ok (= #x00098344 +fsctl-duplicate-extents-to-file+))
  (ok (= #x000900C4 +fsctl-set-sparse+))
  (ok (= #x0009027C +fsctl-get-integrity-information+))
  (ok (= #x0009C280 +fsctl-set-integrity-information+))
  (ok (= #x00090073 +fsctl-get-retrieval-pointers+))
  (ok (= #x08000000 +file-supports-block-refcounting+))
  (ok (equal '(17 50 87 347)
             (list +error-not-same-device+ +error-not-supported+
                   +error-invalid-parameter+ +error-block-too-many-references+))))

(deftest block-clone-error-condition
  (let ((c (make-condition 'block-clone-error :operation "DeviceIoControl" :code 5
                                              :path #p"C:/x/a.txt")))
    (ok (typep c 'sp:clone-error) "a cl-stack-pathlib clone-error, so USE-COPY applies")
    (ok (typep c 'sp:path-error))
    (ok (eq :block-clone (sp:clone-error-mechanism c)) "mechanism defaults to :block-clone")
    (ok (equal "DeviceIoControl" (sp:clone-error-operation c)))
    (ok (eql 5 (sp:clone-error-code c)))
    (ok (search "DeviceIoControl" (princ-to-string c)))))

(deftest block-clone-memory-filesystem
  ;; The probe's order is locked: off Windows comes first, so a memory
  ;; filesystem is :UNAVAILABLE on macOS / Linux and :UNSUPPORTED-FILESYSTEM
  ;; on Windows.
  (let ((memory (sp:make-memory-filesystem)))
    (sp:with-filesystem (memory)
      (sp:mkdir "/w" :parents t))
    (ok (equal (if (windows-p) '(nil :unsupported-filesystem) '(nil :unavailable))
               (multiple-value-list (block-clone-available-p memory #p"/w/" #p"/w/"))))
    (ok (not (member :block-clone (sp:clone-mechanisms memory #p"/w/" #p"/w/"))))
    (ok (signals (sp:fs-clone-tree memory #p"/w/" #p"/c/" :mechanism :block-clone)
                 'sp:unsupported-operation))))

(deftest block-clone-off-windows
  (if (windows-p)
      (pass "Windows: see clone-detect-test and clone-refs-test")
      (with-work-dir (dir)
        (ok (equal '(nil :unavailable)
                   (multiple-value-list (block-clone-available-p *fs* dir dir))))
        (ok (not (member :block-clone (sp:clone-mechanisms *fs* dir dir))))
        (multiple-value-bind (mechanism reason) (sp:fs-clone-supported-p *fs* dir)
          (ok (null mechanism))
          (ok (eq :unavailable reason) "the best-ranked mechanism's reason"))
        (let ((src (put dir "a.txt" "hello")))
          (ok (signals (sp:fs-clone-file *fs* src (sub dir "b.txt") :mechanism :block-clone)
                       'sp:unsupported-operation)
              "by name: unsupported-operation")
          (ok (not (probe-file (sub dir "b.txt"))) "nothing created")
          (let ((report (sp:fs-clone-file *fs* src (sub dir "c.txt"))))
            (ok (eq :copy (sp:clone-report-mechanism report)) ":auto copies")
            (ok (eq :unavailable (sp:clone-report-reason report))))))))

(deftest block-clone-helpers-off-windows
  (if (windows-p)
      (pass "Windows: the real helpers")
      (flet ((signals-bce (thunk)
               (handler-case (progn (funcall thunk) nil)
                 (block-clone-error (c)
                   (and (eq :block-clone (sp:clone-error-mechanism c))
                        (null (sp:clone-error-code c)))))))
        (ok (signals-bce (lambda () (volume-supports-block-clone-p "C:\\"))))
        (ok (signals-bce (lambda () (volume-guid-path "C:\\"))))
        (ok (signals-bce (lambda () (volume-cluster-size "C:\\"))))
        (ok (signals-bce (lambda () (file-cluster-runs "C:\\x.bin"))))
        (ok (signals-bce (lambda () (block-clone-file *fs* #p"/tmp/a" #p"/tmp/b")))))))
