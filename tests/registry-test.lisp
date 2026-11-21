(in-package #:fs-backend-windows/tests)

;;; Registration at load time, the constants, availability (every OS).

(deftest rdcw-registered
  (let ((names (sp:watch-mechanisms)))
    (ok (member :rdcw names) "loading fs-backend-windows registers :rdcw")
    (ok (< (position :rdcw names) (position :poll names)) ":rdcw ranks above :poll")
    (ok (= 100 +rdcw-rank+))))

(deftest rdcw-constants
  (ok (= #x5F +rdcw-notify-filter+)
      "FILE_NAME | DIR_NAME | ATTRIBUTES | SIZE | LAST_WRITE | CREATION")
  (ok (zerop (logand +rdcw-notify-filter+
                     (logior +file-notify-change-last-access+ +file-notify-change-security+)))
      "no LAST_ACCESS, no SECURITY")
  (ok (= 7 +rdcw-share-mode+) "share read | write | delete")
  (ok (= #x42000000 +rdcw-create-flags+) "BACKUP_SEMANTICS | OVERLAPPED")
  (ok (equal '(1 2 3 4 5)
             (list +file-action-added+ +file-action-removed+ +file-action-modified+
                   +file-action-renamed-old-name+ +file-action-renamed-new-name+)))
  (ok (equal '(5 995 1022) (list +error-access-denied+ +error-operation-aborted+ +error-notify-enum-dir+)))
  (ok (typep *rdcw-buffer-size* '(integer 4096 1048576)))
  (ok (zerop (mod *rdcw-buffer-size* 4)))
  (ok (typep *rdcw-barrier-timeout* '(real 0))))

(deftest rdcw-conditions
  (let ((c (make-condition 'rdcw-error :operation "CreateFileW" :code 5)))
    (ok (typep c 'sp:journal-error) "rdcw-error is a journal-error")
    (ok (equal "CreateFileW" (rdcw-error-operation c)))
    (ok (eql 5 (rdcw-error-code c)))
    (ok (search "CreateFileW" (princ-to-string c))))
  (let ((c (make-condition 'malformed-notify-buffer :offset 12 :reason "odd FileNameLength")))
    (ok (typep c 'error))
    (ok (not (typep c 'sp:journal-error)) "a parser error, not a journal error")
    (ok (eql 12 (malformed-notify-buffer-offset c)))
    (ok (search "odd" (princ-to-string c)))))

(deftest rdcw-availability
  (let ((memory (sp:make-memory-filesystem)))
    (sp:with-filesystem (memory)
      (sp:mkdir "/w" :parents t))
    (ok (not (rdcw-available-p memory #p"/w/")) "never on a memory filesystem")
    (ok (not (member :rdcw (sp:watch-mechanisms memory #p"/w/"))))
    (ok (signals (sp:fs-watch memory #p"/w/" :mechanism :rdcw) 'sp:unsupported-operation)))
  (with-work-dir (dir)
    (if (windows-p)
        (progn
          (ok (rdcw-available-p *fs* dir) "available for the (local) temp directory")
          (ok (eq :rdcw (first (sp:watch-mechanisms *fs* dir)))
              ":auto picks :rdcw for a local directory on Windows"))
        (progn
          (ok (not (rdcw-available-p *fs* dir)) "unavailable off Windows")
          (ok (not (member :rdcw (sp:watch-mechanisms *fs* dir))))
          (ok (signals (sp:fs-watch *fs* dir :mechanism :rdcw) 'sp:unsupported-operation))
          (let ((j (sp:fs-watch *fs* dir)))
            (ok (not (eq :rdcw (sp:journal-mechanism j))) ":auto does not pick :rdcw off Windows")
            (sp:journal-close j))))))

(deftest rdcw-bindings-off-windows
  ;; Off Windows the kernel32 names exist and signal RDCW-ERROR.
  (if (windows-p)
      (pass "Windows: the real bindings")
      (progn
        (ok (signals (process-handle-count) 'rdcw-error))
        (ok (signals (short-path-name "C:\\x") 'rdcw-error)))))
