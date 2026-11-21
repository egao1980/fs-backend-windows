(defsystem "fs-backend-windows"
  :version "0.2.0"
  :description "Windows ReadDirectoryChangesW change journals (:rdcw) and ReFS / Dev Drive block cloning (:block-clone) for cl-stack-pathlib"
  :author "egao1980"
  :license "MIT"
  ;; kernel32 only (CreateFileW, ReadDirectoryChangesW, DeviceIoControl, ...):
  ;; a system DLL, opened in src/ffi.lisp at load time on Windows. No overlay.
  :depends-on ((:version "cl-stack-pathlib" "0.10.0") "cffi" "bordeaux-threads" "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "ffi")              ; constants, conditions, kernel32 (Windows)
               (:file "notify")           ; pure: buffer parser, paths, accumulator, barrier
               (:file "journal")
               (:file "block-clone-plan") ; pure: chunk plan, error mapping, ioctl input
               (:file "block-clone")      ; :block-clone (Win32 glue)
               (:file "register"))        ; :rdcw and :block-clone at load
  :in-order-to ((test-op (test-op "fs-backend-windows/tests"))))

(defsystem "fs-backend-windows/tests"
  :depends-on ("fs-backend-windows" "cl-stack-pathlib/journal-suite"
               "cl-stack-pathlib/clone-suite" "bordeaux-threads" "cffi" "rove" "uiop")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "support")
               (:file "registry-test")
               (:file "notify-test")          ; every OS
               (:file "paths-test")           ; every OS
               (:file "accumulator-test")     ; every OS
               (:file "barrier-test")         ; every OS
               (:file "journal-test")         ; Windows
               (:file "suite-test")           ; Windows
               (:file "clone-support")
               (:file "clone-registry-test")  ; every OS
               (:file "block-clone-plan-test"); every OS
               (:file "clone-detect-test")    ; Windows (NTFS)
               (:file "clone-refs-test")      ; Windows + FS_BACKEND_WINDOWS_REFS_DIR
               (:file "clone-suite-test"))    ; Windows + FS_BACKEND_WINDOWS_REFS_DIR
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
