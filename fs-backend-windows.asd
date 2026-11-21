(defsystem "fs-backend-windows"
  :version "0.1.0"
  :description "Windows ReadDirectoryChangesW change journals (:rdcw) for cl-stack-pathlib fs-watch"
  :author "egao1980"
  :license "MIT"
  ;; kernel32 only (CreateFileW, ReadDirectoryChangesW, ...): a system DLL,
  ;; opened in src/ffi.lisp at load time on Windows. No overlay.
  :depends-on ((:version "cl-stack-pathlib" "0.9.0") "cffi" "bordeaux-threads" "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "ffi")            ; constants, conditions, kernel32 (Windows)
               (:file "notify")         ; pure: buffer parser, paths, accumulator, barrier
               (:file "journal")
               (:file "register")
               (:file "zz-diag"))      ; register-watch-mechanism :rdcw at load
  :in-order-to ((test-op (test-op "fs-backend-windows/tests"))))

(defsystem "fs-backend-windows/tests"
  :depends-on ("fs-backend-windows" "cl-stack-pathlib/journal-suite" "bordeaux-threads" "rove" "uiop")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "support")
               (:file "registry-test")
               (:file "notify-test")      ; every OS
               (:file "paths-test")       ; every OS
               (:file "accumulator-test") ; every OS
               (:file "barrier-test")     ; every OS
               (:file "journal-test")     ; Windows
               (:file "suite-test"))      ; Windows
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
