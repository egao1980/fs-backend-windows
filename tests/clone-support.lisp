(in-package #:fs-backend-windows/tests)

;;; Helpers for the R4c-3 (:block-clone) tests.
;;;
;;; Where they run:
;;;   every OS               clone-registry-test, block-clone-plan-test
;;;   Windows (NTFS temp)    clone-detect-test: UIOP:TEMPORARY-DIRECTORY is on
;;;                          C: (NTFS) on windows-latest
;;;   Windows + a ReFS dir   clone-refs-test, clone-suite-test: the directory
;;;                          named by FS_BACKEND_WINDOWS_REFS_DIR (CI job
;;;                          `refs` formats a ReFS VHDX and sets R:\)
;;;   + a Dev Drive dir      FS_BACKEND_WINDOWS_DEVDRIVE_DIR (optional, set by
;;;                          the `refs` job when Format-Volume -DevDrive worked)
;;;
;;; Without FS_BACKEND_WINDOWS_REFS_DIR each ReFS test is one skip with a
;;; note. With FS_BACKEND_WINDOWS_REQUIRE_REFS=1 a missing directory FAILS
;;; instead, and REFS-VOLUME-READY fails when the volume cannot clone.

(defparameter +clone-skip-note+
  "block cloning is Windows only: skipped on this OS (:block-clone is registered but unavailable here; CI runs it on windows-latest, job `refs`)")

(defparameter +refs-skip-note+
  "block cloning needs ReFS / Dev Drive: set FS_BACKEND_WINDOWS_REFS_DIR (CI job `refs`)")

(defparameter +devdrive-skip-note+
  "no Dev Drive: set FS_BACKEND_WINDOWS_DEVDRIVE_DIR (CI job `refs`, when Format-Volume -DevDrive works)")

(defun %env-directory (name)
  (let ((v (uiop:getenv name)))
    (and v (plusp (length v))
         (uiop:ensure-directory-pathname (uiop:parse-native-namestring v)))))

(defun refs-root () (%env-directory "FS_BACKEND_WINDOWS_REFS_DIR"))
(defun devdrive-root () (%env-directory "FS_BACKEND_WINDOWS_DEVDRIVE_DIR"))
(defun refs-required-p () (equal "1" (uiop:getenv "FS_BACKEND_WINDOWS_REQUIRE_REFS")))

(defmacro deftest-ntfs (name &body body)
  "A DEFTEST whose BODY runs on Windows (on the NTFS temp directory);
elsewhere one skip."
  `(deftest ,name
     (if (windows-p)
         (progn ,@body)
         (skip +clone-skip-note+))))

(defmacro deftest-refs (name &body body)
  "A DEFTEST whose BODY runs on Windows with FS_BACKEND_WINDOWS_REFS_DIR set.
Off Windows: one skip. On Windows without the directory: one skip, or a
failure when FS_BACKEND_WINDOWS_REQUIRE_REFS=1."
  `(deftest ,name
     (cond ((not (windows-p)) (skip +clone-skip-note+))
           ((refs-root) (progn ,@body))
           ((refs-required-p)
            (fail "FS_BACKEND_WINDOWS_REQUIRE_REFS=1 but FS_BACKEND_WINDOWS_REFS_DIR is not set"))
           (t (skip +refs-skip-note+)))))

(defmacro deftest-devdrive (name &body body)
  "A DEFTEST whose BODY runs on Windows with FS_BACKEND_WINDOWS_DEVDRIVE_DIR
(and FS_BACKEND_WINDOWS_REFS_DIR) set; else one skip."
  `(deftest ,name
     (if (and (windows-p) (devdrive-root) (refs-root))
         (progn ,@body)
         (skip +devdrive-skip-note+))))

(defun call-with-dir-under (root fn &key (prefix "r4c3"))
  "Call FN with the truename of a new empty directory below ROOT; delete it
afterwards (ACLs reset first, see DENY-WRITES)."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (%fresh-name prefix) (uiop:ensure-directory-pathname root)))))
    (ensure-directories-exist dir)
    (unwind-protect (funcall fn (truename dir))
      (ignore-errors (allow-writes dir))
      (ignore-errors (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(defmacro with-refs-dir ((var) &body body)
  `(call-with-dir-under (refs-root) (lambda (,var) ,@body)))

(defmacro with-devdrive-dir ((var) &body body)
  `(call-with-dir-under (devdrive-root) (lambda (,var) ,@body)))

;;; --- octets on disk ----------------------------------------------------------------

(defun pseudo-random-octets (n &optional (seed 4242))
  "N reproducible octets (a 32-bit LCG); no two clusters alike."
  (let ((v (make-array n :element-type '(unsigned-byte 8)))
        (x seed))
    (dotimes (i n v)
      (setf x (logand #xFFFFFFFF (+ (* x 1664525) 1013904223))
            (aref v i) (ldb (byte 8 24) x)))))

(defun write-octets (pathname octets)
  (ensure-directories-exist pathname)
  (with-open-file (s pathname :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :supersede :if-does-not-exist :create)
    (write-sequence octets s))
  pathname)

(defun read-octets (pathname)
  (with-open-file (s pathname :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun file-attributes (pathname)
  "GetFileAttributesW of PATHNAME (#xFFFFFFFF when it does not exist)."
  (fs-backend-windows::%get-file-attributes (native pathname)))

(defun sparse-p (pathname)
  (logtest #x200 (file-attributes pathname)))     ; FILE_ATTRIBUTE_SPARSE_FILE

(defun absent-p (pathname)
  (= #xFFFFFFFF (file-attributes pathname)))

(defun make-sparse-file (pathname size &key (head 4096) (tail 4096))
  "A sparse file of SIZE octets (SIZE ≥ HEAD + TAIL + 1 MiB): HEAD octets of
data at the start, TAIL at the end, a hole between (fsutil sparse setflag,
fsutil file seteof). → (values pathname content), CONTENT the expected octets."
  (let* ((h (pseudo-random-octets head 1))
         (tl (pseudo-random-octets tail 2))
         (content (make-array size :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace content h)
    (replace content tl :start1 (- size tail))
    (write-octets pathname h)
    (uiop:run-program (list "fsutil" "sparse" "setflag" (native pathname)) :output :string)
    (uiop:run-program (list "fsutil" "file" "seteof" (native pathname) (princ-to-string (- size tail)))
                      :output :string)
    (with-open-file (s pathname :direction :output :element-type '(unsigned-byte 8)
                                :if-exists :append)
      (write-sequence tl s))
    (values pathname content)))

;;; --- writes that reach the disk ----------------------------------------------------
;;;
;;; ReFS allocates a new cluster for a write into a shared one when the data
;;; is written back, not when WriteFile returns: FSCTL_GET_RETRIEVAL_POINTERS
;;; right after a cached write (CL:OPEN, CLOSE) still shows the shared LCN.
;;; POKE-FLUSHED writes through CreateFileW / WriteFile and FlushFileBuffers,
;;; after which the clone's first run has a new LCN and the source's has not
;;; (windows-latest, ReFS 3.14).

(fs-backend-windows::%define-kernel32 (%test-flush-file-buffers "FlushFileBuffers") :boolean
  (handle :pointer))
(fs-backend-windows::%define-kernel32 (%test-read-file "ReadFile") :boolean
  (handle :pointer) (buffer :pointer) (length :uint32) (read :pointer) (overlapped :pointer))

(defconstant +generic-read+ #x80000000)
(defconstant +open-existing-disposition+ 3)

(defun poke-flushed (pathname octets)
  "Overwrite the first (LENGTH OCTETS) octets of the existing file PATHNAME
in place and flush them to disk (FlushFileBuffers)."
  (let ((h (fs-backend-windows::%create-file
            (native pathname) +generic-write+ 0 (cffi:null-pointer)
            +open-existing-disposition+ +file-attribute-normal+ (cffi:null-pointer))))
    (when (fs-backend-windows::%invalid-handle-p h)
      (error "CreateFileW ~S failed (error ~D)" pathname (fs-backend-windows::%get-last-error)))
    (unwind-protect
         (cffi:with-pointer-to-vector-data (p octets)
           (cffi:with-foreign-object (written :uint32)
             (unless (%test-write-file h p (length octets) written (cffi:null-pointer))
               (error "WriteFile failed (error ~D)" (fs-backend-windows::%get-last-error)))
             (unless (%test-flush-file-buffers h)
               (error "FlushFileBuffers failed (error ~D)" (fs-backend-windows::%get-last-error)))))
      (fs-backend-windows::%close-handle h))
    pathname))

(defun read-wide (native-name)
  "The octets of the file NATIVE-NAME (may hold non-BMP characters) through
CreateFileW / ReadFile."
  (let ((h (fs-backend-windows::%create-file
            native-name +generic-read+ 1 (cffi:null-pointer)
            +open-existing-disposition+ 0 (cffi:null-pointer))))
    (when (fs-backend-windows::%invalid-handle-p h)
      (error "CreateFileW ~S failed (error ~D)" native-name (fs-backend-windows::%get-last-error)))
    (unwind-protect
         (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
           (cffi:with-foreign-objects ((buffer :uint8 65536) (n :uint32))
             (loop
               (unless (%test-read-file h buffer 65536 n (cffi:null-pointer))
                 (error "ReadFile failed (error ~D)" (fs-backend-windows::%get-last-error)))
               (let ((k (cffi:mem-ref n :uint32)))
                 (when (zerop k) (return))
                 (dotimes (i k) (vector-push-extend (cffi:mem-aref buffer :uint8 i) out)))))
           (coerce out '(simple-array (unsigned-byte 8) (*))))
      (fs-backend-windows::%close-handle h))))

;;; --- ACLs ----------------------------------------------------------------------------

(defun deny-writes (dir)
  "Deny Everyone (S-1-1-0) write access to DIR and below (icacls): creating a
file in DIR then fails with ERROR_ACCESS_DENIED, administrators included."
  (uiop:run-program (list "icacls" (native dir) "/deny" "*S-1-1-0:(OI)(CI)(W)")
                    :output :string :error-output :string))

(defun allow-writes (dir)
  "Remove the deny entry of DENY-WRITES."
  (when (windows-p)
    (uiop:run-program (list "icacls" (native dir) "/remove:d" "*S-1-1-0")
                      :output :string :error-output :string :ignore-error-status t)))

;;; --- cluster runs ------------------------------------------------------------------

(defun runs-clusters (runs)
  (loop for (nil nil count) in runs sum count))

(defun shares-clusters-p (a b)
  "True when the files A and B have the same, non-empty cluster runs."
  (let ((ra (file-cluster-runs a)) (rb (file-cluster-runs b)))
    (and ra (equal ra rb))))

(defun disjoint-clusters-p (a b)
  "True when no data cluster of A is a data cluster of B (by run start LCN
ranges)."
  (flet ((ranges (runs)
           (loop for (nil lcn count) in runs unless (= lcn -1) collect (cons lcn (+ lcn count)))))
    (let ((ra (ranges (file-cluster-runs a))) (rb (ranges (file-cluster-runs b))))
      (notany (lambda (x) (some (lambda (y) (and (< (car x) (cdr y)) (< (car y) (cdr x)))) rb))
              ra))))

(defun clone-direct (source destination)
  "(BLOCK-CLONE-FILE *fs* SOURCE DESTINATION) as a list of its values."
  (multiple-value-list (block-clone-file *fs* source destination)))

(defun lcn-at (runs vcn)
  "The LCN of cluster VCN in RUNS ((vcn lcn count) …): -1 in a hole, NIL past
the runs."
  (loop for (start lcn count) in runs
        when (and (<= start vcn) (< vcn (+ start count)))
          return (if (= lcn -1) -1 (+ lcn (- vcn start)))))
