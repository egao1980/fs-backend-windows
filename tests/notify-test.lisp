(in-package #:fs-backend-windows/tests)

;;; FILE_NOTIFY_INFORMATION parsing and UTF-16LE: pure, every OS, on
;;; hand-built octet buffers (FNI in support.lisp).

;;; --- UTF-16LE ------------------------------------------------------------------------

(deftest-parametrize utf16le-decode
    ((units expected)
     :ids ("empty" "ascii" "latin-1" "bmp-cjk" "surrogate-pair" "pair-in-middle"
           "lone-high" "lone-low" "high-at-end" "reversed-pair")
     (() "")
     ('(#x61 #x2E #x74) "a.t")
     ('(#xE9 #xFC) "éü")
     ('(#x65E5 #x672C) "日本")
     ('(#xD83D #xDE00) "😀")
     ('(#x61 #xD83D #xDE00 #x62) "a😀b")
     ('(#xD800 #x61) nil)
     ('(#x61 #xDC00) nil)
     ('(#x61 #xD83D) nil)
     ('(#xDE00 #xD83D) nil))
  (ok (equal expected (utf16le-octets-to-string (apply #'octets (u16le units))))
      (format nil "~S → ~S" units expected)))

(deftest utf16le-decode-ranges
  (let ((v (apply #'octets (append '(0 0) (u16le "abc") '(7)))))
    (ok (equal "abc" (utf16le-octets-to-string v :start 2 :end 8)))
    (ok (equal "b" (utf16le-octets-to-string v :start 4 :end 6)))
    (ok (equal "" (utf16le-octets-to-string v :start 4 :end 4)))
    (ok (null (utf16le-octets-to-string v :start 2 :end 7)) "an odd number of octets → NIL"))
  (let ((adjustable (make-array 4 :element-type '(unsigned-byte 8) :adjustable t
                                  :initial-contents (u16le "hi"))))
    (ok (equal "hi" (utf16le-octets-to-string adjustable)) "a non-simple vector")))

;;; --- the parser ------------------------------------------------------------------------

(deftest parse-fni-single
  (ok (equal '((1 . "a.txt")) (parse-file-notify-information (fni '(1 "a.txt")))))
  (ok (equal '((3 . "src\\deep\\e.lisp"))
             (parse-file-notify-information (fni '(3 "src\\deep\\e.lisp"))))
      "names stay native (backslashes), as reported")
  (ok (equal '((3 . "")) (parse-file-notify-information (fni '(3 ""))))
      "a zero-length name parses (the accumulator decides what it means)"))

(deftest parse-fni-empty
  (ok (null (parse-file-notify-information (octets))) "no octets → no entries")
  (ok (null (parse-file-notify-information (pad (octets) 64) :end 0))
      "bytes-returned 0 → no entries"))

(deftest parse-fni-chain
  (let ((buf (fni '(4 "src\\c.lisp") '(5 "src\\d.lisp") '(1 "x") '(2 "日本\\😀.txt") '(3 "abcd"))))
    (ok (zerop (mod (length buf) 2)))
    (ok (equal '((4 . "src\\c.lisp") (5 . "src\\d.lisp") (1 . "x") (2 . "日本\\😀.txt") (3 . "abcd"))
               (parse-file-notify-information buf))
        "every entry in buffer order, padding skipped")))

(deftest parse-fni-padding-is-dword
  ;; "x" is 12 + 2 octets, padded to 16: the next entry starts at 16.
  (let ((buf (fni '(1 "x") '(2 "y"))))
    (ok (= 16 (aref buf 0)) "NextEntryOffset 16")
    (ok (equal '((1 . "x") (2 . "y")) (parse-file-notify-information buf)))))

(deftest parse-fni-unknown-action-passes-through
  (ok (equal '((9 . "a") (1 . "b")) (parse-file-notify-information (fni '(9 "a") '(1 "b"))))))

(deftest parse-fni-bad-utf16-name
  (ok (equal '((1 . nil) (1 . "ok"))
             (parse-file-notify-information (fni (list 1 '(#xD800 #x61)) '(1 "ok"))))
      "an unpaired surrogate → NIL name, the rest still parsed"))

(deftest parse-fni-range
  (let* ((buf (fni '(1 "a") '(3 "b")))
         (n (length buf)))
    (ok (equal '((1 . "a") (3 . "b")) (parse-file-notify-information (pad buf 40) :end n))
        "trailing octets past END (bytes-returned) are not read")
    (ok (equal '((1 . "a") (3 . "b"))
               (parse-file-notify-information (concatenate '(simple-array (unsigned-byte 8) (*))
                                                           (make-list 8 :initial-element 0) buf)
                                              :start 8))
        "START offsets the chain")))

(deftest parse-fni-non-simple-vector
  (let* ((buf (fni '(1 "a") '(2 "b")))
         (v (make-array (length buf) :element-type '(unsigned-byte 8) :adjustable t
                                     :fill-pointer (length buf) :initial-contents buf)))
    (ok (equal '((1 . "a") (2 . "b")) (parse-file-notify-information v)))))

(deftest-parametrize parse-fni-malformed
    ((bytes)
     :ids ("short-header" "name-past-end" "odd-name-length" "next-past-end"
           "next-not-dword" "next-inside-entry" "next-zero-length-loop")
     ((list 0 0 0 0 1 0 0 0 4 0))
     ((append (u32le 0) (u32le 1) (u32le 10) (u16le "ab")))
     ((append (u32le 0) (u32le 1) (u32le 3) (u16le "ab")))
     ((append (u32le 64) (u32le 1) (u32le 2) (u16le "a") '(0 0)))
     ((append (u32le 18) (u32le 1) (u32le 2) (u16le "a") '(0 0 0 0) (fni-entry 1 "b")))
     ((append (u32le 12) (u32le 1) (u32le 4) (u16le "ab") (fni-entry 1 "b")))
     ((append (u32le 4) (u32le 1) (u32le 0) (fni-entry 1 "b"))))
  (ok (signals (parse-file-notify-information (apply #'octets bytes)) 'malformed-notify-buffer)
      "a broken chain signals MALFORMED-NOTIFY-BUFFER")
  (handler-case (parse-file-notify-information (apply #'octets bytes))
    (malformed-notify-buffer (c)
      (ok (integerp (malformed-notify-buffer-offset c)) "the offset of the entry at fault")
      (ok (stringp (malformed-notify-buffer-reason c))))
    (:no-error (&rest values) (declare (ignore values)) (fail "no condition"))))

;;; --- actions -------------------------------------------------------------------------

(deftest-parametrize rdcw-action-keywords
    ((action keyword)
     :ids ("added" "removed" "modified" "renamed-old" "renamed-new" "zero" "six" "huge")
     (1 :added) (2 :removed) (3 :modified) (4 :renamed-old) (5 :renamed-new)
     (0 nil) (6 nil) (#xFFFFFFFF nil))
  (ok (eq keyword (rdcw-action-keyword action))))
