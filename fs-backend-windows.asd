(defsystem "fs-backend-windows"
  :version "0.1.0"
  :description "Windows ReadDirectoryChangesW change journals (:rdcw) for cl-stack-pathlib fs-watch"
  :author "egao1980"
  :license "MIT"
  :depends-on ("uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")))
