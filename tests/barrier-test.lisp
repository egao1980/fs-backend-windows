(in-package #:fs-backend-windows/tests)

;;; The drain barrier between JOURNAL-DRAIN and the watcher thread. Pure
;;; Lisp with bordeaux-threads: every OS.

(deftest barrier-tickets
  (let ((b (make-rdcw-barrier)))
    (ok (typep b 'rdcw-barrier))
    (ok (= 0 (rdcw-barrier-requested b)))
    (let* ((t1 (rdcw-barrier-request b))
           (t2 (rdcw-barrier-request b)))
      (ok (and (integerp t1) (plusp t1)))
      (ok (> t2 t1) "tickets increase")
      (ok (= t2 (rdcw-barrier-requested b)))
      (ok (not (rdcw-barrier-wait b t1 0)) "not acked, timeout 0: NIL at once")
      (ok (null (rdcw-barrier-ack b t1)))
      (ok (rdcw-barrier-wait b t1 0) "acked")
      (ok (not (rdcw-barrier-wait b t2 0)) "an ack answers tickets up to it, not later ones")
      (rdcw-barrier-ack b t2)
      (ok (rdcw-barrier-wait b t2 0))
      (rdcw-barrier-ack b t1)
      (ok (rdcw-barrier-wait b t2 0) "an older ack changes nothing"))))

(deftest barrier-wait-for-responder
  (let* ((b (make-rdcw-barrier))
         (ticket (rdcw-barrier-request b))
         (responder (bt2:make-thread
                     (lambda ()
                       (sleep 0.1)
                       ;; The watcher acks what it read from REQUESTED.
                       (rdcw-barrier-ack b (rdcw-barrier-requested b)))
                     :name "barrier-responder")))
    (multiple-value-bind (seconds result) (elapsed (lambda () (rdcw-barrier-wait b ticket 5)))
      (ok (eq t result) "the ack arrives")
      (ok (>= seconds 0.05) (format nil "it waited for it (~,3F s)" seconds))
      (ok (< seconds 2) (format nil "and returned as soon as it came (~,3F s)" seconds)))
    (bt2:join-thread responder)))

(deftest barrier-timeout
  (let* ((b (make-rdcw-barrier))
         (ticket (rdcw-barrier-request b)))
    (multiple-value-bind (seconds result) (elapsed (lambda () (rdcw-barrier-wait b ticket 0.2)))
      (ok (null result) "no ack: NIL")
      (ok (>= seconds 0.15) (format nil "after the timeout (~,3F s)" seconds))
      (ok (< seconds 1.5) (format nil "not much later (~,3F s)" seconds)))))

(deftest barrier-many-waiters
  (let* ((b (make-rdcw-barrier))
         (results (make-array 4 :initial-element :unset))
         (waiters (loop for k below 4
                        collect (let ((k k)
                                      (ticket (rdcw-barrier-request b)))
                                  (bt2:make-thread
                                   (lambda () (setf (aref results k) (rdcw-barrier-wait b ticket 5)))
                                   :name "barrier-waiter")))))
    (sleep 0.05)
    (rdcw-barrier-ack b (rdcw-barrier-requested b))
    (multiple-value-bind (seconds) (elapsed (lambda () (mapc #'bt2:join-thread waiters)))
      (ok (< seconds 2) "one ack wakes every waiter"))
    (ok (every (lambda (r) (eq r t)) results) (format nil "all acked: ~S" results))))

(deftest barrier-fail
  (let* ((b (make-rdcw-barrier))
         (ticket (rdcw-barrier-request b))
         (result :unset)
         (waiter (bt2:make-thread (lambda () (setf result (rdcw-barrier-wait b ticket 10)))
                                  :name "barrier-waiter")))
    (sleep 0.05)
    (ok (null (rdcw-barrier-fail b)))
    (multiple-value-bind (seconds) (elapsed (lambda () (bt2:join-thread waiter)))
      (ok (< seconds 2) "a failed barrier wakes its waiters at once"))
    (ok (null result) "and they get NIL")
    (let ((later (rdcw-barrier-request b)))
      (multiple-value-bind (seconds result2) (elapsed (lambda () (rdcw-barrier-wait b later 5)))
        (ok (null result2) "later waits: NIL")
        (ok (< seconds 1) "without waiting")))))
