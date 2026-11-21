(in-package #:fs-backend-windows/tests)

;;; R4c-3 on a real ReFS volume (FS_BACKEND_WINDOWS_REFS_DIR; CI job `refs`).
;;; Content, sparse files, a non-BMP name, the proof that clusters are really
;;; shared (FSCTL_GET_RETRIEVAL_POINTERS), cross-volume, an ACL failure, and
;;; areas.

(deftest-refs refs-volume-ready
  (with-refs-dir (dir)
    (ok (volume-supports-block-clone-p dir)
        "the ReFS volume has FILE_SUPPORTS_BLOCK_REFCOUNTING")
    (let ((cluster (volume-cluster-size dir)))
      (ok (member cluster '(4096 65536)) (format nil "a ReFS cluster size: ~S" cluster)))
    (ok (equal '(t) (multiple-value-list (block-clone-available-p *fs* dir dir))))
    (ok (eq :block-clone (first (sp:clone-mechanisms *fs* dir dir))) ":auto picks :block-clone")
    (ok (equal '(:block-clone nil) (multiple-value-list (sp:fs-clone-supported-p *fs* dir))))
    (ok (not (string-equal (volume-guid-path dir) (volume-guid-path (uiop:temporary-directory))))
        "the ReFS volume is not the temp directory's")))

(deftest-refs refs-clone-content
  (with-refs-dir (dir)
    (let ((cluster (volume-cluster-size dir)))
      (loop for (name size) in `(("empty" 0) ("one" 1) ("cluster" ,cluster)
                                 ("cluster+1" ,(1+ cluster)) ("8mib" ,(* 8 1024 1024))
                                 ("8mib+3" ,(+ 3 (* 8 1024 1024))))
            for content = (pseudo-random-octets size (+ 11 size))
            for src = (write-octets (sub dir (format nil "src/~A.bin" name)) content)
            for dst = (sub dir (format nil "dst/~A.bin" name))
            do (ensure-directories-exist dst)
               (ok (equal '(t) (clone-direct src dst)) (format nil "~A: cloned" name))
               (ok (equalp content (read-octets dst)) (format nil "~A: same content" name))
               (ok (= size (with-open-file (s dst :element-type '(unsigned-byte 8)) (file-length s)))
                   (format nil "~A: same size (the rounded tail is not part of the file)" name))
               (ok (equalp content (read-octets src)) (format nil "~A: source unchanged" name))))))

(deftest-refs refs-clone-file-report
  (with-refs-dir (dir)
    (let* ((content (pseudo-random-octets (* 3 1024 1024)))
           (src (write-octets (sub dir "a.bin") content))
           (report (sp:fs-clone-file *fs* src (sub dir "out/b.bin"))))
      (ok (eq :block-clone (sp:clone-report-mechanism report)) ":auto clones on ReFS")
      (ok (null (sp:clone-report-reason report)))
      (ok (= 1 (sp:clone-report-cloned report)))
      (ok (null (sp:clone-report-fallbacks report)))
      (ok (equalp content (read-octets (sub dir "out/b.bin")))))))

(deftest-refs refs-clone-non-bmp-name
  ;; SBCL's OPEN cannot name U+1F600; the source is made and the clone read
  ;; through CreateFileW (tests/support.lisp, tests/clone-support.lisp).
  (with-refs-dir (dir)
    (let* ((text "grinning: 😀, a cluster's worth follows")
           (name (put-wide dir "clone-😀.txt" text))
           (src (sub dir "clone-😀.txt"))
           (dst (sub dir "clone-😀-copy.txt")))
      (ok (stringp name))
      (ok (equal '(t) (clone-direct src dst)) "a non-BMP name clones")
      (ok (equalp (read-wide name) (read-wide (wide-name dir "clone-😀-copy.txt")))
          "same content, read through CreateFileW")
      (ok (delete-wide dir "clone-😀-copy.txt"))
      (ok (delete-wide dir "clone-😀.txt")))))

(deftest-refs refs-clone-sparse
  (with-refs-dir (dir)
    (multiple-value-bind (src content) (make-sparse-file (sub dir "sparse.bin") (* 16 1024 1024))
      (ok (sparse-p src) "the source is sparse")
      (ok (member -1 (mapcar #'second (file-cluster-runs src))) "the source has a hole")
      (let ((dst (sub dir "sparse-clone.bin")))
        (ok (equal '(t) (clone-direct src dst)) "a sparse source clones (FSCTL_SET_SPARSE first)")
        (ok (sparse-p dst) "the clone is sparse too")
        (ok (equalp content (read-octets dst)) "same size and content")))))

(deftest-refs refs-clusters-shared
  ;; The proof that the ioctl shared clusters (not just that the bytes match):
  ;; a block clone's runs are its source's LCNs, a copy's are not; a flushed
  ;; write into the clone's first cluster moves only the clone's first run.
  (with-refs-dir (dir)
    (let* ((size (* 64 1024 1024))
           (content (pseudo-random-octets size 64))
           (src (write-octets (sub dir "big.bin") content))
           (clone (sub dir "big-clone.bin"))
           (copy (sub dir "big-copy.bin")))
      (ok (equal '(t) (clone-direct src clone)))
      (let ((src-runs (file-cluster-runs src))
            (clone-runs (file-cluster-runs clone)))
        (ok (consp src-runs))
        (ok (equal src-runs clone-runs) "the clone maps to exactly the source's clusters")
        (ok (= (runs-clusters src-runs) (/ size (volume-cluster-size dir))) "every cluster of 64 MiB")
        (sp:fs-clone-file *fs* src copy :mechanism :copy)
        (ok (disjoint-clusters-p src copy) "a :copy has clusters of its own")
        (poke-flushed clone (octets 1 2 3 4))
        (let ((after (file-cluster-runs clone)))
          (ok (/= (lcn-at src-runs 0) (lcn-at after 0))
              "the written cluster (VCN 0) of the clone has a new LCN")
          (ok (equal src-runs (file-cluster-runs src)) "the source's runs are unchanged")
          (let ((last (1- (runs-clusters src-runs))))
            (ok (every (lambda (vcn) (eql (lcn-at src-runs vcn) (lcn-at after vcn)))
                       (list 1 2 (floor last 2) last))
                "the clone's other clusters are still shared")))
        (ok (equalp content (read-octets src)) "the source's content is unchanged")
        (let ((c (read-octets clone)))
          (ok (equalp (octets 1 2 3 4) (subseq c 0 4)))
          (ok (equalp (subseq content 4) (subseq c 4)) "the rest of the clone is the source's"))))))

(deftest-refs refs-cross-volume
  (with-refs-dir (dir)
    (with-work-dir (ntfs)
      (ok (equal '(nil :cross-volume)
                 (multiple-value-list (block-clone-available-p *fs* dir ntfs)))
          "ReFS → NTFS temp: the probe says :cross-volume")
      (ok (equal '(nil :unsupported-volume)
                 (multiple-value-list (block-clone-available-p *fs* ntfs dir)))
          "NTFS → ReFS: the source volume decides first")
      (let* ((src (write-octets (sub dir "a.bin") (pseudo-random-octets 10000)))
             (dst (sub ntfs "a.bin")))
        (ok (equal '(nil :unsupported-volume) (clone-direct src dst))
            "the ioctl goes to the NTFS target: ERROR_INVALID_FUNCTION")
        (ok (absent-p dst) "DESTINATION deleted")
        (let ((report (sp:fs-clone-file *fs* src (sub ntfs "b.bin"))))
          (ok (eq :copy (sp:clone-report-mechanism report)))
          (ok (eq :cross-volume (sp:clone-report-reason report))))
        (ok (signals (sp:fs-clone-file *fs* src (sub ntfs "c.bin") :mechanism :block-clone)
                     'sp:unsupported-operation))
        (ok (absent-p (sub ntfs "c.bin")))))))

(deftest-refs refs-access-denied
  (with-refs-dir (dir)
    (let ((src (write-octets (sub dir "a.bin") (pseudo-random-octets 10000)))
          (locked (ensure-directories-exist (sub dir "locked/"))))
      (deny-writes locked)
      (handler-case (progn (clone-direct src (sub locked "a.bin"))
                           (fail "a denied destination directory must signal"))
        (block-clone-error (c)
          (ok (eql 5 (sp:clone-error-code c)) "ERROR_ACCESS_DENIED")
          (ok (equal "CreateFileW" (sp:clone-error-operation c)))
          (ok (eq :block-clone (sp:clone-error-mechanism c)))))
      (ok (absent-p (sub locked "a.bin")))
      ;; Through pathlib: BLOCK-CLONE-ERROR is a CLONE-ERROR, USE-COPY is offered.
      (let ((offered nil))
        (block try
          (handler-bind ((block-clone-error
                           (lambda (c)
                             (setf offered (and (find-restart 'sp:use-copy c) t))
                             (return-from try))))
            (sp:fs-clone-file *fs* src (sub locked "b.bin") :mechanism :block-clone)))
        (ok offered "use-copy is offered around the clone"))
      (ok (absent-p (sub locked "b.bin")))
      (allow-writes locked))))

(deftest-refs refs-areas
  (with-refs-dir (dir)
    (let ((base (sub dir "base/")))
      (make-tree base '("a.txt" "src/b.lisp" "src/c.lisp" "empty/"))
      (write-octets (sub base "data.bin") (pseudo-random-octets (* 1024 1024)))
      (let ((area (sp:fs-open-area *fs* base :directory (sub dir "area/"))))
        (unwind-protect
             (let ((report (sp:area-clone-report area)))
               (ok (eq :clone (sp:area-mechanism area)) "an area on ReFS is a :clone area")
               (ok (eq :block-clone (sp:clone-report-mechanism report)))
               (ok (= 4 (sp:clone-report-cloned report)))
               (ok (zerop (sp:clone-report-copied report)))
               (ok (shares-clusters-p (sub base "data.bin") (sub dir "area/data.bin"))
                   "the area's file shares the base's clusters")
               (ok (null (sp:fs-area-changes area))))
          (sp:fs-close-area area)))
      (let ((area (sp:fs-open-area *fs* base)))
        (unwind-protect
             (progn
               (ok (eq :copy (sp:area-mechanism area))
                   "the default area directory (temp, NTFS) is another volume")
               (ok (eq :cross-volume (sp:clone-report-reason (sp:area-clone-report area)))))
          (sp:fs-close-area area))))))
