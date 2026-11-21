(in-package #:fs-backend-windows/tests)

;;; Helpers.
;;;
;;; Every OS: hand-built FILE_NOTIFY_INFORMATION buffers (FNI), a UTF-16LE
;;; encoder of the tests' own, and a fake directory lister.
;;;
;;; Windows: work directories (a fresh subdirectory of
;;; UIOP:TEMPORARY-DIRECTORY, by truename, deleted afterwards), edits through
;;; CL:OPEN / CL:RENAME-FILE / UIOP and cmd.exe (not the pathlib API), so the
;;; journal sees plain file system calls.

(defun windows-p () (uiop:os-windows-p))

(defparameter +skip-note+
  "ReadDirectoryChangesW is Windows only: skipped on this OS (the :rdcw mechanism is registered but unavailable here; CI runs it on windows-latest)")

(defmacro deftest-windows (name &body body)
  "A DEFTEST whose BODY runs on Windows; elsewhere it records one skip with a note."
  `(deftest ,name
     (if (windows-p)
         (progn ,@body)
         (skip +skip-note+))))

;;; --- octets ------------------------------------------------------------------------

(defun octets (&rest bytes)
  (coerce bytes '(simple-array (unsigned-byte 8) (*))))

(defun u32le (n)
  (list (ldb (byte 8 0) n) (ldb (byte 8 8) n) (ldb (byte 8 16) n) (ldb (byte 8 24) n)))

(defun u16le-units (string)
  "UTF-16 code units of STRING (surrogate pairs above U+FFFF)."
  (loop for ch across string
        for c = (char-code ch)
        if (< c #x10000) collect c
        else append (let ((v (- c #x10000)))
                      (list (+ #xD800 (ash v -10)) (+ #xDC00 (logand v #x3FF))))))

(defun u16le (string-or-units)
  "UTF-16LE octets (a list) of a string, or of a list of raw code units (for
unpaired surrogates)."
  (loop for u in (if (stringp string-or-units) (u16le-units string-or-units) string-or-units)
        append (list (ldb (byte 8 0) u) (ldb (byte 8 8) u))))

(defun fni-entry (action name &key next)
  "Octets (a list) of one FILE_NOTIFY_INFORMATION entry. NAME is a string or
a list of raw UTF-16 code units. NEXT: the NextEntryOffset (default 0)."
  (let ((name-octets (u16le name)))
    (append (u32le (or next 0)) (u32le action) (u32le (length name-octets)) name-octets)))

(defun fni (&rest entries)
  "A well-formed buffer of ENTRIES, each (ACTION NAME): DWORD-aligned, each
NextEntryOffset the padded entry size, the last 0."
  (let ((out '()))
    (loop for (entry . more) on entries
          for (action name) = entry
          for raw = (fni-entry action name)
          for padded = (* 4 (ceiling (length raw) 4))
          do (setf out (append out
                               (if more
                                   (append (fni-entry action name :next padded)
                                           (make-list (- padded (length raw)) :initial-element 0))
                                   raw))))
    (apply #'octets out)))

(defun pad (vector n &optional (byte #xEE))
  "VECTOR followed by N octets BYTE (trailing garbage past bytes-returned)."
  (concatenate '(simple-array (unsigned-byte 8) (*)) vector (make-list n :initial-element byte)))

;;; --- a fake lister for RDCW-RESOLVE-PATH -------------------------------------------

(defparameter +fake-tree+
  '(("" ("README.md") ("src") ("sub") ("LongFileName.txt" . "LONGFI~1.TXT")
        ("Program Files" . "PROGRA~1") ("case") ("Été.txt") ("deep"))
    ("src" ("Main.lisp") ("deep"))
    ("src/deep" ("x.lisp"))
    ("sub" ("kept.txt"))
    ("Program Files" ("App" . nil))
    ("Program Files/App" ("Run.exe"))
    ;; a case-sensitive directory (per-directory case sensitivity, WSL)
    ("case" ("A.txt") ("a.TXT") ("b.txt"))
    ("deep" ("A Long Directory" . "ALONGD~1"))
    ("deep/A Long Directory" ("f.txt" . nil)))
  "Directories (relative, on-disk spelling) → entries (LONG . SHORT).")

(defun make-fake-lister (&optional (tree +fake-tree+))
  "→ (values lister calls): LISTER answers from TREE; CALLS is a function
returning the list of directories asked for, in order."
  (let ((calls '()))
    (values (lambda (dir)
              (push dir calls)
              (rest (assoc dir tree :test #'string=)))
            (lambda () (reverse calls)))))

;;; --- Windows: work directories and edits --------------------------------------------

(defvar *fs* (sp:make-local-filesystem))

(defun %fresh-name (prefix)
  (format nil "~A-~36R" prefix (random (expt 36 8) (make-random-state t))))

(defun call-with-work-dir (fn &key (prefix "fs-backend-windows"))
  "Call FN with the truename of a new empty directory under the temp directory."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (%fresh-name prefix) (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (unwind-protect (funcall fn (truename dir))
      (ignore-errors (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(defmacro with-work-dir ((var &rest keys) &body body)
  `(call-with-work-dir (lambda (,var) ,@body) ,@keys))

(defun sub (dir rel)
  "The pathname REL (POSIX, \"/\"-terminated for a directory) below DIR."
  (merge-pathnames (uiop:parse-unix-namestring rel) (uiop:ensure-directory-pathname dir)))

(defun put (dir rel text)
  (let ((p (sub dir rel)))
    (ensure-directories-exist p)
    (with-open-file (s p :direction :output :if-exists :supersede :if-does-not-exist :create
                         :external-format :utf-8)
      (write-string text s))
    p))

(defun put-native (native-namestring text)
  "Write TEXT to the file named by the native NATIVE-NAMESTRING (e.g. an 8.3 or
differently cased spelling), keeping that spelling."
  (with-open-file (s (uiop:parse-native-namestring native-namestring)
                     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string text s)))

(defun make-tree (dir rels)
  (dolist (r rels dir)
    (if (char= #\/ (char r (1- (length r))))
        (ensure-directories-exist (sub dir r))
        (put dir r r))))

(defun native (pathname)
  (uiop:native-namestring pathname))

(defun cmd (dir command)
  "Run COMMAND with cmd.exe /c in DIR; → stdout."
  (uiop:run-program (format nil "cmd.exe /c ~A" command)
                    :directory dir :output :string :error-output :string))

;;; --- Windows: edits through the UTF-16 Win32 API --------------------------------------
;;;
;;; SBCL's own file calls (OPEN, PROBE-FILE, DELETE-FILE, RENAME-FILE) pass
;;; names to Win32 as UCS-2 (sb-win32 SYSTEM-STRING), so they cannot name a
;;; file outside the BMP (a surrogate pair, e.g. U+1F600). These helpers go
;;; through CreateFileW / WriteFile / DeleteFileW / MoveFileExW with UTF-16LE
;;; names: the system's CreateFileW binding and %WSTRING type, plus three
;;; bindings of the tests' own (made with the system's %DEFINE-KERNEL32, so
;;; they load, and signal RDCW-ERROR, off Windows).

(fs-backend-windows::%define-kernel32 (%test-write-file "WriteFile") :boolean
  (handle :pointer) (buffer :pointer) (length :uint32) (written :pointer) (overlapped :pointer))
(fs-backend-windows::%define-kernel32 (%test-delete-file "DeleteFileW") :boolean
  (name fs-backend-windows::%wstring))
(fs-backend-windows::%define-kernel32 (%test-move-file "MoveFileExW") :boolean
  (from fs-backend-windows::%wstring) (to fs-backend-windows::%wstring) (flags :uint32))

(defconstant +generic-write+ #x40000000)
(defconstant +create-always+ 2)
(defconstant +file-attribute-normal+ #x80)

(defun wide-name (dir rel)
  "The native name (a string, may hold non-BMP characters) of REL below DIR."
  (concatenate 'string (native (uiop:ensure-directory-pathname dir))
               (substitute #\\ #\/ rel)))

(defun put-wide (dir rel text)
  "Create or overwrite the file REL below DIR (its directory must exist) with
TEXT (UTF-8) through CreateFileW/WriteFile; → its native name."
  (let* ((name (wide-name dir rel))
         (h (fs-backend-windows::%create-file
             name +generic-write+ 0 (cffi:null-pointer) +create-always+
             +file-attribute-normal+ (cffi:null-pointer))))
    (when (fs-backend-windows::%invalid-handle-p h)
      (error "CreateFileW ~S failed (error ~D)" name (fs-backend-windows::%get-last-error)))
    (unwind-protect
         (cffi:with-foreign-string ((buffer size) text :encoding :utf-8 :null-terminated-p nil)
           (cffi:with-foreign-object (written :uint32)
             (unless (%test-write-file h buffer size written (cffi:null-pointer))
               (error "WriteFile ~S failed (error ~D)" name (fs-backend-windows::%get-last-error)))))
      (fs-backend-windows::%close-handle h))
    name))

(defun delete-wide (dir rel)
  "DeleteFileW of REL below DIR; → true on success."
  (%test-delete-file (wide-name dir rel)))

(defun move-wide (dir from to)
  "MoveFileExW (no flags) of FROM to TO, both below DIR; → (values ok-p code),
CODE the GetLastError of a failure."
  (if (%test-move-file (wide-name dir from) (wide-name dir to) 0)
      (values t 0)
      (values nil (fs-backend-windows::%get-last-error))))

(defun watch (dir &rest keys)
  (apply #'sp:fs-watch *fs* (uiop:ensure-directory-pathname dir) :mechanism :rdcw keys))

(defmacro with-journal ((var dir &rest keys) &body body)
  `(let ((,var (watch ,dir ,@keys)))
     (unwind-protect (progn ,@body)
       (sp:journal-close ,var))))

(defun drain (journal &rest keys)
  "(paths complete-p) of one drain."
  (multiple-value-list (apply #'sp:journal-drain journal keys)))

(defun covers-p (paths rel)
  "True when REL is one of PATHS or below one of them."
  (some (lambda (p)
          (or (string= p rel)
              (and (< (length p) (length rel))
                   (string= p rel :end2 (length p))
                   (char= #\/ (char rel (length p))))))
        paths))

(defun thread-count () (length (bt2:all-threads)))

(defun wait-until (predicate &key (timeout 3) (step 0.05))
  "Poll PREDICATE until true or TIMEOUT seconds; → its last value."
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))
        for v = (funcall predicate)
        when (or v (> (get-internal-real-time) deadline)) return v
        do (sleep step)))

(defun elapsed (fn)
  "→ (values seconds result) of calling FN."
  (let* ((t0 (get-internal-real-time))
         (r (funcall fn)))
    (values (/ (- (get-internal-real-time) t0) internal-time-units-per-second) r)))
