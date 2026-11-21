(in-package #:fs-backend-windows)

;;; Loading the system registers :RDCW on every OS; RDCW-AVAILABLE-P keeps it
;;; out of (WATCH-MECHANISMS fs directory) off Windows, for non-local
;;; filesystems and on network volumes, so FS-WATCH :AUTO and FS-OPEN-AREA
;;; :JOURNAL :AUTO pick it exactly where it works. :POLL always ranks below it.

(sp:register-watch-mechanism :rdcw 'make-rdcw-journal
                             :rank +rdcw-rank+
                             :available-p 'rdcw-available-p)
