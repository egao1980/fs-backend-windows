(defpackage #:fs-backend-windows/tests
  (:use #:cl #:rove #:fs-backend-windows)
  (:local-nicknames (#:sp #:cl-stack-pathlib))
  (:import-from #:cl-stack-pathlib/journal-suite #:run-journal-suite #:journal-suite-steps)
  (:import-from #:cl-stack-pathlib/clone-suite #:run-clone-suite #:clone-suite-steps))
