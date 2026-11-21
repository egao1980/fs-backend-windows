(in-package #:fs-backend-windows/tests)

;;; Names and paths: pure string work and a fake lister, every OS.

(deftest-parametrize rdcw-relative-path-table
    ((native expected)
     :ids ("file" "nested" "root" "leading" "trailing" "doubled" "slashes" "unicode" "spaces")
     ("a.txt" "a.txt")
     ("src\\deep\\e.lisp" "src/deep/e.lisp")
     ("" "")
     ("\\a.txt" "a.txt")
     ("dir\\" "dir")
     ("a\\\\b" "a/b")
     ("a/b\\c" "a/b/c")
     ("é\\日本\\😀.txt" "é/日本/😀.txt")
     ("Program Files\\My App\\x y.txt" "Program Files/My App/x y.txt"))
  (ok (equal expected (rdcw-relative-path native)) (format nil "~S → ~S" native expected)))

(deftest rdcw-fold-keys
  (ok (equal "ABC.TXT" (rdcw-fold-key "aBc.txt")))
  (ok (string= (rdcw-fold-key "README.md") (rdcw-fold-key "readme.MD")))
  (ok (string= (rdcw-fold-key "Été") (rdcw-fold-key "éTÉ")) "Latin-1 letters fold")
  (ok (string= (rdcw-fold-key "Straße") (rdcw-fold-key "STRAßE")) "ß is kept, one-to-one")
  (ok (string/= (rdcw-fold-key "a.txt") (rdcw-fold-key "b.txt")))
  (ok (= 6 (length (rdcw-fold-key "Straße"))) "no length-changing folds")
  (let ((s "keep"))
    (rdcw-fold-key s)
    (ok (equal "keep" s) "the argument is not modified")))

(deftest-parametrize rdcw-short-name-table
    ((component expected)
     :ids ("progra" "longfi-ext" "lower" "two-digits" "one-char-base" "eight-base"
           "no-ext-dot" "plain" "tilde-only" "no-digits" "nine-base" "long-ext"
           "two-dots" "tilde-first" "two-tildes" "digits-then-letters" "empty-ext")
     ("PROGRA~1" t) ("LONGFI~1.TXT" t) ("longfi~1.txt" t) ("AB~12.C" t) ("A~1" t)
     ("ABCDEF~9.X" t) ("ABCD~1" t)
     ("file.txt" nil) ("~1" nil) ("a~b" nil) ("ABCDEFG~1" nil) ("x~1.html" nil)
     ("A~1.B.C" nil) ("~ABC~1" nil) ("A~B~1" nil) ("AB~1X" nil) ("AB~1." nil))
  (ok (eq expected (and (rdcw-short-name-p component) t))
      (format nil "~S → ~S" component expected)))

(deftest-parametrize rdcw-resolve-path-table
    ((name expected)
     :ids ("exact" "case-corrected" "nested-case" "short-file" "short-file-lower"
           "short-dir" "short-dir-nested" "missing-leaf" "missing-leaf-case-dir"
           "missing-short-leaf" "missing-short-at-root" "missing-dir-chain"
           "exact-in-case-sensitive" "ambiguous" "unique-in-case-sensitive"
           "unicode-case" "deep-exact")
     ("README.md" '("README.md"))
     ("readme.md" '("README.md" "readme.md"))
     ("SRC/main.LISP" '("src/Main.lisp" "SRC/main.LISP"))
     ("LONGFI~1.TXT" '("LongFileName.txt" "LONGFI~1.TXT"))
     ("longfi~1.txt" '("LongFileName.txt" "longfi~1.txt"))
     ("PROGRA~1/App/Run.exe" '("Program Files/App/Run.exe" "PROGRA~1/App/Run.exe"))
     ("deep/ALONGD~1/f.txt" '("deep/A Long Directory/f.txt" "deep/ALONGD~1/f.txt"))
     ("src/gone.txt" '("src/gone.txt"))
     ("SRC/gone.txt" '("src/gone.txt" "SRC/gone.txt"))
     ("sub/GONE~1.TXT" '("sub" "sub/GONE~1.TXT"))
     ("GONE~1.TXT" '("" "GONE~1.TXT"))
     ("gone-dir/x/y.txt" '("gone-dir/x/y.txt"))
     ("case/a.TXT" '("case/a.TXT"))
     ("case/a.txt" '("case" "case/a.txt"))
     ("case/B.TXT" '("case/b.txt" "case/B.TXT"))
     ("été.TXT" '("Été.txt" "été.TXT"))
     ("src/deep/x.lisp" '("src/deep/x.lisp")))
  (ok (equal expected (rdcw-resolve-path name (make-fake-lister)))
      (format nil "~S → ~S" name expected)))

(deftest rdcw-resolve-path-root-lister-arguments
  (multiple-value-bind (lister calls) (make-fake-lister)
    (rdcw-resolve-path "PROGRA~1/App/Run.exe" lister)
    (ok (equal '("" "Program Files" "Program Files/App") (funcall calls))
        "directories are listed by their resolved (on-disk) spelling, root first")))

(deftest rdcw-resolve-paths-union
  (multiple-value-bind (lister calls) (make-fake-lister)
    (let ((out (rdcw-resolve-paths '("readme.md" "README.md" "src/Main.lisp" "src/main.lisp"
                                     "LONGFI~1.TXT" "src/gone.txt" "README.md")
                                   lister)))
      (ok (equal '("LONGFI~1.TXT" "LongFileName.txt" "README.md" "readme.md"
                   "src/Main.lisp" "src/gone.txt" "src/main.lisp")
                 out)
          (format nil "deduplicated, sorted STRING<: ~S" out))
      (ok (equal (sort (copy-list (funcall calls)) #'string<)
                 (remove-duplicates (sort (copy-list (funcall calls)) #'string<) :test #'string=))
          (format nil "each directory listed at most once: ~S" (funcall calls)))
      (ok (equal '("" "src") (sort (remove-duplicates (funcall calls) :test #'string=) #'string<)))))
  (ok (null (rdcw-resolve-paths '() (make-fake-lister))) "no names → NIL"))

(deftest rdcw-resolve-paths-missing-root-listing
  ;; A lister that knows nothing (the root is gone): names stay as reported.
  (let ((lister (lambda (dir) (declare (ignore dir)) nil)))
    (ok (equal '("a.txt" "b/c.txt") (rdcw-resolve-paths '("b/c.txt" "a.txt") lister)))
    (ok (equal '("" "X~1") (rdcw-resolve-paths '("X~1") lister))
        "an unresolvable 8.3 alias at the root → \"\" (rescan)")))

(deftest-parametrize rdcw-same-final-path-table
    ((a b expected)
     :ids ("identical" "prefix" "case" "trailing" "unc" "slashes" "different" "prefix-name"
           "different-drive")
     ("C:\\Users\\x\\w" "C:\\Users\\x\\w" t)
     ("\\\\?\\C:\\Users\\x\\w" "C:\\Users\\x\\w" t)
     ("\\\\?\\C:\\USERS\\X\\W" "c:\\users\\x\\w" t)
     ("C:\\Users\\x\\w\\" "C:\\Users\\x\\w" t)
     ("\\\\?\\UNC\\srv\\share\\d" "\\\\srv\\share\\d" t)
     ("C:/Users/x/w/" "C:\\Users\\x\\w" t)
     ("C:\\Users\\x\\w" "C:\\Users\\x\\moved" nil)
     ("C:\\a" "C:\\ab" nil)
     ("C:\\a" "D:\\a" nil))
  (ok (eq expected (and (rdcw-same-final-path-p a b) t)) (format nil "~S vs ~S" a b)))
