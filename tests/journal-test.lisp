(in-package #:fs-backend-windows/tests)

;;; The :rdcw journal on Windows: what drains report, names (UTF-16, case,
;;; 8.3), overflow, the root going away, the barrier, close, leaks.
;;; Elsewhere each test is one skip with a note.

(defparameter +tree+ '("sub/a.txt" "sub/deep/c.txt" "other/b.txt" "top.txt" "empty/"))

;;; --- basics ------------------------------------------------------------------------

(deftest-windows rdcw-journal-basics
  (with-work-dir (dir)
    (make-tree dir +tree+)
    (let ((handles (rdcw-handle-count))
          (j (watch dir)))
      (unwind-protect
           (progn
             (ok (typep j 'rdcw-journal))
             (ok (eq :rdcw (sp:journal-mechanism j)))
             (ok (sp:journal-open-p j))
             (ok (eq *fs* (sp:journal-filesystem j)))
             (ok (rdcw-journal-handle j) "a directory handle while open")
             (ok (= *rdcw-buffer-size* (rdcw-journal-buffer-size j)))
             (ok (and (rdcw-journal-thread j) (bt2:thread-alive-p (rdcw-journal-thread j)))
                 "its watcher thread is alive")
             (ok (= (1+ handles) (rdcw-handle-count)))
             (ok (equal '(nil t) (drain j)) "nothing touched: complete and empty")
             (put dir "sub/a.txt" "edited")
             (destructuring-bind (paths complete-p) (drain j)
               (ok complete-p "a plain edit needs no rescan")
               (ok (covers-p paths "sub/a.txt") (format nil "sub/a.txt reported: ~S" paths))
               (ok (not (covers-p paths "other/b.txt")) "an untouched sibling directory is not covered")
               (ok (not (covers-p paths "top.txt"))))
             (ok (equal '(nil t) (drain j)) "a drain consumes what it reported")
             (put dir "new.txt" "n")
             (delete-file (sub dir "top.txt"))
             (ensure-directories-exist (sub dir "made/"))
             (rename-file (sub dir "sub/deep/c.txt") (sub dir "other/c2.txt"))
             (destructuring-bind (paths complete-p) (drain j)
               (ok complete-p)
               (dolist (rel '("new.txt" "top.txt" "made" "sub/deep/c.txt" "other/c2.txt"))
                 (ok (covers-p paths rel) (format nil "~A covered by ~S" rel paths)))
               (ok (not (covers-p paths "sub/a.txt")) "an entry untouched since the last drain"))
             (put dir "empty/x.txt" "x")
             (destructuring-bind (paths complete-p) (drain j)
               (ok complete-p)
               (ok (every (lambda (p) (and (plusp (length p))
                                           (char/= #\/ (char p 0))
                                           (not (find #\\ p))))
                          paths)
                   (format nil "relative POSIX paths, no backslashes: ~S" paths))
               (ok (covers-p paths "empty/x.txt"))))
        (sp:journal-close j)))))

(deftest-windows rdcw-journal-ignore
  (with-work-dir (dir)
    (put dir ".gitignore" (format nil "*.log~%build/~%"))
    (make-tree dir '("a.txt" "build/o"))
    (with-journal (j dir :ignore :auto)
      (put dir "x.log" "l")
      (put dir "build/new.o" "o")
      (put dir "a.txt" "edited")
      (ok (equal '(("a.txt") t) (drain j)) "ignored paths never show up"))))

(deftest-windows rdcw-journal-after-fs-watch-only
  (with-work-dir (dir)
    (put dir "before.txt" "b")
    (with-journal (j dir)
      (drain j)
      (put dir "after.txt" "a")
      (destructuring-bind (paths complete-p) (drain j)
        (ok complete-p)
        (ok (covers-p paths "after.txt"))
        (ok (not (covers-p paths "before.txt")))))))

(deftest-windows rdcw-journal-child-process
  (with-work-dir (dir)
    (make-tree dir '("keep.txt" "victim.txt"))
    (with-journal (j dir)
      (drain j)
      (cmd dir "echo x> made.txt && del victim.txt && mkdir newdir && echo y> newdir\\in.txt && ren keep.txt kept.txt")
      (destructuring-bind (paths complete-p) (drain j)
        (ok complete-p)
        (dolist (rel '("made.txt" "victim.txt" "newdir/in.txt" "keep.txt" "kept.txt"))
          (ok (covers-p paths rel) (format nil "~A covered by ~S" rel paths)))))))

;;; --- names -------------------------------------------------------------------------

(deftest-windows rdcw-journal-unicode-names
  (with-work-dir (dir)
    (with-journal (j dir)
      (put dir "é/日本.txt" "x")
      (put dir "😀.txt" "y")
      (destructuring-bind (paths complete-p) (drain j)
        (ok complete-p)
        (ok (covers-p paths "é/日本.txt") (format nil "BMP names: ~S" paths))
        (ok (covers-p paths "😀.txt") (format nil "a surrogate pair: ~S" paths))))))

(deftest-windows rdcw-journal-case
  (with-work-dir (dir)
    (make-tree dir '("CaseRoot/README.md" "CaseRoot/Src/x.lisp"))
    (let ((root (sub dir "CaseRoot/")))
      ;; Watched as spelled in lower case: paths come back as on disk.
      (with-journal (j (sub dir "caseroot/"))
        (drain j)
        (put root "Src/x.lisp" "edited")
        (destructuring-bind (paths complete-p) (drain j)
          (ok complete-p)
          (ok (covers-p paths "Src/x.lisp") (format nil "on-disk spelling: ~S" paths)))
        ;; Written through a differently cased name: the on-disk name is covered.
        (put-native (concatenate 'string (native root) "src\\X.LISP") "again")
        (destructuring-bind (paths complete-p) (drain j)
          (ok complete-p)
          (ok (covers-p paths "Src/x.lisp") (format nil "written as src\\X.LISP: ~S" paths)))
        ;; A case-only rename: both names.
        (rename-file (sub root "README.md") (sub root "readme.md"))
        (destructuring-bind (paths complete-p) (drain j)
          (ok complete-p "a case-only rename needs no rescan")
          (ok (covers-p paths "README.md") (format nil "old name in ~S" paths))
          (ok (covers-p paths "readme.md") (format nil "new name in ~S" paths)))
        ;; Deleted through a differently cased name: the on-disk name is covered.
        (delete-file (uiop:parse-native-namestring
                      (concatenate 'string (native root) "README.MD")))
        (destructuring-bind (paths complete-p) (drain j)
          (if complete-p
              (ok (covers-p paths "readme.md") (format nil "deleted as README.MD: ~S" paths))
              (pass "incomplete: rescan everything")))))))

(deftest-windows rdcw-journal-short-names
  (with-work-dir (dir)
    (let* ((long "sub/Long File Name Here.txt")
           (file (put dir long "x"))
           (short (short-path-name file)))
      (if (string-equal short (native file))
          (skip "this volume generates no 8.3 aliases (fsutil 8dot3name): nothing to test")
          (with-journal (j dir)
            (drain j)
            (ok (search "~" short) (format nil "the 8.3 alias ~A" short))
            ;; Written through its 8.3 alias: the long name is covered.
            (put-native short "edited")
            (destructuring-bind (paths complete-p) (drain j)
              (ok complete-p)
              (ok (covers-p paths long) (format nil "written as ~A: ~S" short paths)))
            ;; Deleted through its 8.3 alias: covered (by itself or by sub).
            (delete-file (uiop:parse-native-namestring short))
            (destructuring-bind (paths complete-p) (drain j)
              (ok complete-p "a deletion below the root needs no full rescan")
              (ok (covers-p paths long) (format nil "deleted as ~A: ~S" short paths))))))))

;;; --- overflow ------------------------------------------------------------------------

(deftest-parametrize rdcw-injected-overflow
    ((bytes code) :ids ("zero-bytes" "notify-enum-dir") (0 0) (0 1022))
  (if (not (windows-p))
      (skip +skip-note+)
      (with-work-dir (dir)
        (make-tree dir +tree+)
        (with-journal (j dir)
          (drain j)
          (put dir "sub/a.txt" "edited")
          (rdcw-journal-inject j (octets) :bytes bytes :code code)
          (ok (equal '(nil nil) (drain j)) "an overflow: complete-p NIL")
          (put dir "other/b.txt" "edited")
          (destructuring-bind (paths complete-p) (drain j)
            (ok complete-p "the drain after an overflow is complete again")
            (ok (covers-p paths "other/b.txt")))))))

(deftest-windows rdcw-injected-events
  (with-work-dir (dir)
    (with-journal (j dir)
      (drain j)
      (rdcw-journal-inject j (fni '(1 "x\\y.txt") '(4 "old.txt") '(5 "new.txt")))
      (ok (equal '(("new.txt" "old.txt" "x/y.txt") t) (drain j))
          "injected names go through the watcher's code (missing files keep their spelling)")
      (rdcw-journal-inject j (fni '(3 "")))
      (ok (equal '(nil nil) (drain j)) "an event on the root itself: rescan everything")
      (ok (equal '(nil t) (drain j))))))

(deftest-windows rdcw-real-overflow
  ;; A small buffer and a paused watcher: 2000 creates overflow the kernel's
  ;; queue for the handle (it has the size of the first request).
  (with-work-dir (dir)
    (let ((*rdcw-buffer-size* 4096)
          (*rdcw-barrier-timeout* 0.2))
      (with-journal (j dir)
        (ok (= 4096 (rdcw-journal-buffer-size j)))
        (drain j)
        (rdcw-journal-pause j)
        (dotimes (k 2000) (put dir (format nil "burst/f~4,'0D.txt" k) "n"))
        (rdcw-journal-resume j)
        (ok (equal '(nil nil) (drain j)) "overflowed: complete-p NIL")
        (put dir "after.txt" "a")
        (destructuring-bind (paths complete-p) (drain j)
          (ok complete-p "recovered on the next drain")
          (ok (covers-p paths "after.txt")))))))

(deftest-windows rdcw-buffer-size-checked
  (with-work-dir (dir)
    (dolist (size '(100 4095 4097 2097152))
      (let ((*rdcw-buffer-size* size)
            (before (rdcw-handle-count)))
        (ok (signals (watch dir) 'type-error) (format nil "buffer size ~D refused" size))
        (ok (= before (rdcw-handle-count)) "nothing left open")))))

(deftest-windows rdcw-burst
  (with-work-dir (dir)
    (with-journal (j dir)
      (dotimes (k 2000) (put dir (format nil "burst/f~4,'0D.txt" k) "n"))
      (destructuring-bind (paths complete-p) (drain j)
        (if complete-p
            (ok (every (lambda (k) (covers-p paths (format nil "burst/f~4,'0D.txt" k)))
                       (loop for k below 2000 collect k))
                "complete: every burst file is covered")
            (ok (null paths) "incomplete (overflow): no paths"))))))

;;; --- the root ------------------------------------------------------------------------

(deftest-windows rdcw-root-deleted
  (with-work-dir (dir)
    (make-tree dir '("w/a.txt" "w/s/b.txt"))
    (with-journal (j (sub dir "w/"))
      (drain j)
      (uiop:delete-directory-tree (sub dir "w/") :validate t)
      (ok (not (probe-file (sub dir "w/a.txt"))) "the watched root can be deleted (FILE_SHARE_DELETE)")
      (ok (equal '(nil nil) (drain j)) "the root deleted: complete-p NIL")
      (make-tree dir '("w/again.txt"))
      (ok (equal '(nil nil) (drain j)) "for good, even when a directory of that name is back")
      (ok (equal '(nil nil) (drain j))))))

(deftest-windows rdcw-root-renamed
  (with-work-dir (dir)
    (make-tree dir '("w/a.txt"))
    (with-journal (j (sub dir "w/"))
      (drain j)
      (cmd dir "ren w moved")
      (ok (probe-file (sub dir "moved/a.txt")) "the watched root can be renamed")
      (ok (equal '(nil nil) (drain j)) "the root moved: complete-p NIL")
      (put dir "moved/a.txt" "edited")
      (ok (equal '(nil nil) (drain j)) "for good"))
    ;; An ancestor renamed counts too.
    (make-tree dir '("p/w/a.txt"))
    (with-journal (j (sub dir "p/w/"))
      (drain j)
      (cmd dir "ren p q")
      (ok (equal '(nil nil) (drain j)) "an ancestor moved: complete-p NIL"))))

(deftest-windows rdcw-injected-root-error
  (with-work-dir (dir)
    (with-journal (j dir)
      (drain j)
      (rdcw-journal-inject j (octets) :bytes 0 :code 5)
      (ok (equal '(nil nil) (drain j)) "ERROR_ACCESS_DENIED: lost")
      (put dir "a.txt" "a")
      (ok (equal '(nil nil) (drain j)) "sticky"))))

;;; --- the barrier (settle) ------------------------------------------------------------

(deftest-windows rdcw-drain-collects-at-once
  ;; A drain reports a write that returned just before it, complete, with
  ;; settle 0 as with the default: the barrier collects what the kernel queued.
  (with-work-dir (dir)
    (with-journal (j dir)
      (drain j)
      (dolist (settle '(0 0.05))
        (dotimes (round 20)
          (let ((name (format nil "f-~A-~D.txt" settle round)))
            (put dir name "x")
            (multiple-value-bind (seconds result) (elapsed (lambda () (drain j :settle settle)))
              (ok (second result) (format nil "settle ~A: complete" settle))
              (ok (covers-p (first result) name) (format nil "settle ~A: ~A reported" settle name))
              (ok (< seconds 0.5) (format nil "settle ~A: drained in ~,3F s" settle seconds)))))))))

(deftest-windows rdcw-drain-barrier-timeout
  (with-work-dir (dir)
    (let ((*rdcw-barrier-timeout* 0.2))
      (with-journal (j dir)
        (drain j)
        (rdcw-journal-pause j)
        (put dir "a.txt" "a")
        (multiple-value-bind (seconds result) (elapsed (lambda () (drain j :settle 0)))
          (ok (equal '(nil nil) result) "the watcher did not answer: complete-p NIL")
          (ok (< seconds 1.5) (format nil "after the barrier timeout (~,3F s)" seconds)))
        (rdcw-journal-resume j)
        (put dir "b.txt" "b")
        (destructuring-bind (paths complete-p) (drain j)
          (ok complete-p "the watcher answers again")
          (ok (covers-p paths "b.txt")))))))

;;; --- close -----------------------------------------------------------------------------

(deftest-windows rdcw-close
  (with-work-dir (dir)
    (make-tree dir +tree+)
    (let* ((handles (rdcw-handle-count))
           (j (watch dir))
           (thread (rdcw-journal-thread j)))
      (ok (bt2:thread-alive-p thread))
      (ok (null (sp:journal-close j)))
      (ok (not (sp:journal-open-p j)))
      (ok (not (bt2:thread-alive-p thread)) "close joins the watcher thread")
      (ok (null (rdcw-journal-thread j)))
      (ok (null (rdcw-journal-handle j)) "the handle is closed")
      (ok (= handles (rdcw-handle-count)))
      (ok (null (sp:journal-close j)) "idempotent")
      (ok (signals (sp:journal-drain j) 'sp:journal-error))
      (ok (signals (rdcw-journal-inject j (fni '(1 "a"))) 'sp:journal-error))
      (ok (signals (rdcw-journal-pause j) 'sp:journal-error))
      (ok (null (rdcw-journal-resume j)))
      ;; The root is free again: it can be deleted without a sharing violation.
      (uiop:delete-directory-tree (sub dir "empty/") :validate t)
      (ok (not (probe-file (sub dir "empty/")))))
    ;; Close with the root already gone, from another thread, and while paused.
    (let ((j (watch (sub dir "other/"))))
      (uiop:delete-directory-tree (sub dir "other/") :validate t)
      (ok (null (sp:journal-close j)) "close after the root was deleted"))
    (let* ((j (watch (sub dir "sub/")))
           (thread (rdcw-journal-thread j))
           (closer (bt2:make-thread (lambda () (sp:journal-close j)) :name "closer")))
      (bt2:join-thread closer)
      (ok (not (sp:journal-open-p j)))
      (ok (not (bt2:thread-alive-p thread)) "closed from another thread"))
    (let* ((j (watch dir))
           (thread (rdcw-journal-thread j)))
      (rdcw-journal-pause j)
      (multiple-value-bind (seconds) (elapsed (lambda () (sp:journal-close j)))
        (ok (< seconds 3) "close of a paused journal returns"))
      (ok (not (bt2:thread-alive-p thread)) "and joins its watcher"))))

(deftest-windows rdcw-area-auto
  (with-work-dir (dir)
    (make-tree dir '("base/a.txt"))
    (let ((area (sp:open-area (sp:ensure-path (sub dir "base/") :directory t)
                              :directory (sp:ensure-path (sub dir "area/") :directory t))))
      (unwind-protect
           (let ((j (sp:area-journal area)))
             (ok (and j (eq :rdcw (sp:journal-mechanism j))) ":journal :auto attaches :rdcw")
             (sp:close-area area)
             (ok (not (sp:journal-open-p j)) "closing the area closes its journal"))
        (when (sp:area-open-p area) (sp:close-area area))))))

;;; --- leaks -----------------------------------------------------------------------------

(deftest-windows rdcw-no-leak-after-50-cycles
  (with-work-dir (dir)
    (make-tree dir +tree+)
    ;; One cycle first, so lazily made threads or handles are counted in BEFORE.
    (let ((j (watch dir))) (drain j) (sp:journal-close j))
    (let ((threads (thread-count))
          (journals (rdcw-handle-count))
          (handles (process-handle-count)))
      (dotimes (k 50)
        (let ((j (watch dir)))
          (put dir "sub/a.txt" (princ-to-string k))
          (drain j)
          (when (zerop (mod k 10))
            (rdcw-journal-inject j (octets) :bytes 0))
          (drain j)
          (sp:journal-close j)))
      (ok (= journals (rdcw-handle-count)) "every journal handle closed")
      (ok (wait-until (lambda () (<= (thread-count) threads)) :timeout 3)
          (format nil "no thread leak: ~D threads before, ~D after 50 cycles" threads (thread-count)))
      (ok (<= (process-handle-count) (+ handles 5))
          (format nil "no handle leak: ~D process handles before, ~D after 50 cycles (3 per journal)"
                  handles (process-handle-count))))))
