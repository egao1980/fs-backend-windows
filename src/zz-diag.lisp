(in-package #:cl-user)
;;; THROWAWAY DIAGNOSTIC (never merged).
#+win32
(progn
(defun diag-hc ()
  (sb-alien:with-alien ((n sb-alien:unsigned-int))
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "GetProcessHandleCount"
                            (function sb-alien:int sb-alien:unsigned-long (* sb-alien:unsigned-int)))
     (ldb (byte 64 0) -1) (sb-alien:addr n))
    n))
(defun diag-wstr (s)
  (let* ((n (length s)) (p (sb-alien:make-alien (sb-alien:unsigned 16) (1+ n))))
    (dotimes (i n) (setf (sb-alien:deref p i) (char-code (char s i))))
    (setf (sb-alien:deref p n) 0) p))
(defun diag-run ()
  (format t "~&DIAG SBCL ~A~%" (lisp-implementation-version))
  (dotimes (i 3) (sb-thread:join-thread (sb-thread:make-thread (lambda () 1))))
  (let ((before (diag-hc)))
    (dotimes (i 50) (sb-thread:join-thread (sb-thread:make-thread (lambda () 1))))
    (format t "~&DIAG THREADS: handles before ~D after 50 make/join ~D~%" before (diag-hc)))
  (let* ((tmp (string-right-trim "\\/" (uiop:getenv "TEMP")))
         (p (format nil "~A\\diagp" tmp))
         (w (format nil "~A\\diagp\\w" tmp))
         (move (sb-alien:extern-alien "MoveFileExW"
                                      (function sb-alien:int (* (sb-alien:unsigned 16)) (* (sb-alien:unsigned 16)) sb-alien:unsigned-int)))
         (gle (sb-alien:extern-alien "GetLastError" (function sb-alien:unsigned-int))))
    (ensure-directories-exist (uiop:ensure-directory-pathname (uiop:parse-native-namestring w)))
    (let ((h (sb-alien:alien-funcall
              (sb-alien:extern-alien "CreateFileW"
                                     (function sb-alien:unsigned-long (* (sb-alien:unsigned 16)) sb-alien:unsigned-int sb-alien:unsigned-int
                                               sb-alien:unsigned-long sb-alien:unsigned-int sb-alien:unsigned-int sb-alien:unsigned-long))
              (diag-wstr w) 1 7 0 3 (logior #x02000000 #x40000000) 0)))
      (format t "~&DIAG RENAME: handle ~X~%" h)
      (let ((r (sb-alien:alien-funcall move (diag-wstr p) (diag-wstr (format nil "~A\\diagq" tmp)) 0)))
        (format t "~&DIAG RENAME: parent rename with child handle open -> ~D (error ~D)~%" r (sb-alien:alien-funcall gle)))
      (sb-alien:alien-funcall (sb-alien:extern-alien "CloseHandle" (function sb-alien:int sb-alien:unsigned-long)) h)
      (let ((r (sb-alien:alien-funcall move (diag-wstr p) (diag-wstr (format nil "~A\\diagq" tmp)) 0)))
        (format t "~&DIAG RENAME: parent rename after close -> ~D (error ~D)~%" r (sb-alien:alien-funcall gle)))))
  (handler-case (with-open-file (s (merge-pathnames (format nil "~C.txt" (code-char 128512)) (uiop:temporary-directory))
                                   :direction :output :if-exists :supersede)
                  (write-string "y" s) (format t "~&DIAG EMOJI: open ok~%"))
    (error (e) (format t "~&DIAG EMOJI: ~A~%" e)))
  (finish-output))
(handler-case (diag-run) (error (e) (format t "~&DIAG FAILED: ~A~%" e) (finish-output))))
