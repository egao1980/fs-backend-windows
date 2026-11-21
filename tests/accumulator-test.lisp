(in-package #:fs-backend-windows/tests)

;;; The accumulator: completions (hand-built buffers + error codes) in,
;;; (values names complete-p) out. Pure, every OS.

(defun take (acc) (multiple-value-list (rdcw-accumulator-take acc)))

(deftest accumulator-fresh
  (let ((acc (make-rdcw-accumulator)))
    (ok (typep acc 'rdcw-accumulator))
    (ok (equal '(nil t) (take acc)) "nothing seen: complete and empty")
    (ok (not (rdcw-accumulator-lost-p acc)))))

(deftest accumulator-paths
  (let ((acc (make-rdcw-accumulator)))
    (ok (null (rdcw-accumulate acc (fni '(1 "new.txt") '(3 "src\\deep\\a.lisp") '(2 "gone.txt")))))
    (ok (equal '(("gone.txt" "new.txt" "src/deep/a.lisp") t) (take acc))
        "relative POSIX paths, sorted STRING<")
    (ok (equal '(nil t) (take acc)) "a take consumes")
    (rdcw-accumulate acc (fni '(3 "a.txt") '(3 "a.txt")))
    (rdcw-accumulate acc (fni '(3 "a.txt")))
    (ok (equal '(("a.txt") t) (take acc)) "exact duplicates once, across completions")))

(deftest accumulator-keeps-spellings
  ;; A case-only rename: both spellings survive (the resolver needs both).
  (let ((acc (make-rdcw-accumulator)))
    (rdcw-accumulate acc (fni '(4 "README.md") '(5 "readme.md")))
    (ok (equal '(("README.md" "readme.md") t) (take acc)))))

(deftest accumulator-renames
  (let ((acc (make-rdcw-accumulator)))
    (rdcw-accumulate acc (fni '(4 "src\\c.lisp") '(5 "src\\d.lisp")))
    (rdcw-accumulate acc (fni '(4 "docs") '(5 "manual")))
    (ok (equal '(("docs" "manual" "src/c.lisp" "src/d.lisp") t) (take acc))
        "both names of each rename, files and directories")
    ;; An old name whose new name came in the next completion, and a lone new name.
    (rdcw-accumulate acc (fni '(4 "x.txt")))
    (rdcw-accumulate acc (fni '(5 "y.txt")))
    (rdcw-accumulate acc (fni '(5 "z.txt")))
    (ok (equal '(("x.txt" "y.txt" "z.txt") t) (take acc)) "unpaired names are still touched paths")))

(deftest accumulator-unicode
  (let ((acc (make-rdcw-accumulator)))
    (rdcw-accumulate acc (fni '(1 "é\\日本.txt") '(1 "😀.txt")))
    (ok (equal (list (sort (list "é/日本.txt" "😀.txt") #'string<) t) (take acc)))))

(deftest accumulator-bytes
  (let ((acc (make-rdcw-accumulator))
        (buf (fni '(1 "a.txt"))))
    (rdcw-accumulate acc (pad buf 100) :bytes (length buf))
    (ok (equal '(("a.txt") t) (take acc)) "only OCTETS[0, BYTES) is read")))

(deftest-parametrize accumulator-overflow
    ((octets bytes code)
     :ids ("zero-bytes" "zero-bytes-stale-buffer" "notify-enum-dir")
     ((octets) 0 0)
     ((fni '(1 "stale.txt")) 0 0)
     ((octets) 0 1022))
  (let ((acc (make-rdcw-accumulator)))
    (rdcw-accumulate acc (fni '(1 "before.txt")))
    (rdcw-accumulate acc octets :bytes bytes :code code)
    (rdcw-accumulate acc (fni '(1 "after.txt")))
    (ok (equal '(nil nil) (take acc)) "an overflow makes the take incomplete")
    (ok (not (rdcw-accumulator-lost-p acc)) "an overflow is recoverable")
    (ok (equal '(nil t) (take acc)) "the mark and the names were cleared")
    (rdcw-accumulate acc (fni '(3 "later.txt")))
    (ok (equal '(("later.txt") t) (take acc)) "complete again")))

(deftest-parametrize accumulator-lost
    ((code)
     :ids ("access-denied" "notify-cleanup" "invalid-handle" "other")
     (5) (745) (6) (87))
  (let ((acc (make-rdcw-accumulator)))
    (rdcw-accumulate acc (fni '(1 "before.txt")))
    (rdcw-accumulate acc (octets) :bytes 0 :code code)
    (ok (rdcw-accumulator-lost-p acc) (format nil "error ~D: the root is lost" code))
    (dotimes (k 3)
      (rdcw-accumulate acc (fni '(1 "after.txt")))
      (ok (equal '(nil nil) (take acc)) "lost is sticky"))))

(deftest accumulator-aborted
  (let ((acc (make-rdcw-accumulator)))
    (rdcw-accumulate acc (fni '(1 "a.txt")))
    (rdcw-accumulate acc (octets) :bytes 0 :code 995)
    (ok (equal '(("a.txt") t) (take acc)) "ERROR_OPERATION_ABORTED (close) changes nothing")
    (ok (not (rdcw-accumulator-lost-p acc)))))

(deftest-parametrize accumulator-incomplete-entries
    ((octets)
     :ids ("unknown-action" "bad-utf16" "root-itself" "root-separator" "malformed")
     ((fni '(1 "a.txt") '(9 "b.txt")))
     ((fni '(1 "a.txt") (list 1 '(#xD800))))
     ((fni '(1 "a.txt") '(3 "")))
     ((fni '(1 "a.txt") '(3 "\\")))
     ((apply #'octets (append (u32le 0) (u32le 1) (u32le 99) (u16le "a")))))
  (let ((acc (make-rdcw-accumulator)))
    (ok (null (rdcw-accumulate acc octets)) "no condition escapes")
    (ok (equal '(nil nil) (take acc)))
    (ok (not (rdcw-accumulator-lost-p acc)))
    (rdcw-accumulate acc (fni '(1 "c.txt")))
    (ok (equal '(("c.txt") t) (take acc)) "recovers on the next take")))

(deftest accumulator-mark-lost
  (let ((acc (make-rdcw-accumulator)))
    (rdcw-accumulate acc (fni '(1 "a.txt")))
    (ok (null (rdcw-accumulator-mark-lost acc)))
    (ok (rdcw-accumulator-lost-p acc))
    (ok (equal '(nil nil) (take acc)))
    (rdcw-accumulate acc (fni '(1 "b.txt")))
    (ok (equal '(nil nil) (take acc)) "sticky")))

(deftest accumulator-threads
  ;; The watcher accumulates while another thread takes: nothing is lost or doubled.
  (let* ((acc (make-rdcw-accumulator))
         (seen (make-hash-table :test #'equal))
         (writers (loop for w below 2
                        collect (let ((w w))
                                  (bt2:make-thread
                                   (lambda ()
                                     (dotimes (k 500)
                                       (rdcw-accumulate
                                        acc (fni (list 1 (format nil "w~D\\f~3,'0D.txt" w k))))))
                                   :name "accumulator-writer")))))
    (loop until (notany #'bt2:thread-alive-p writers)
          do (destructuring-bind (names complete-p) (take acc)
               (ok complete-p)
               (dolist (n names) (incf (gethash n seen 0))))
             (sleep 0.001))
    (mapc #'bt2:join-thread writers)
    (destructuring-bind (names complete-p) (take acc)
      (ok complete-p)
      (dolist (n names) (incf (gethash n seen 0))))
    (ok (= 1000 (hash-table-count seen)) (format nil "~D distinct names" (hash-table-count seen)))
    (ok (loop for v being the hash-values of seen always (= v 1)) "each name taken once")))
