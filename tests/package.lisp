(defpackage #:fs-backend-windows/tests
  (:use #:cl #:rove #:fs-backend-windows)
  (:local-nicknames (#:sp #:cl-stack-pathlib))
  (:import-from #:cl-stack-pathlib/journal-suite #:run-journal-suite #:journal-suite-steps))
