(in-package #:fs-backend-windows)

;;; Loading the system registers :RDCW on every OS; RDCW-AVAILABLE-P keeps it
;;; out of (WATCH-MECHANISMS fs directory) off Windows, for non-local
;;; filesystems and on network volumes, so FS-WATCH :AUTO and FS-OPEN-AREA
;;; :JOURNAL :AUTO pick it exactly where it works. :POLL always ranks below it.

(sp:register-watch-mechanism :rdcw 'make-rdcw-journal
                             :rank +rdcw-rank+
                             :available-p 'rdcw-available-p)

;;; R4c-3: loading the system also registers the clone mechanism
;;; :BLOCK-CLONE on every OS. BLOCK-CLONE-AVAILABLE-P keeps it out of
;;; (CLONE-MECHANISMS fs source destination) off Windows, for non-local
;;; filesystems, on volumes without block refcounting (NTFS, FAT) and across
;;; volumes, so FS-CLONE-TREE :AUTO and FS-OPEN-AREA (default :AUTO) clone
;;; exactly where it works and copy elsewhere. No tree hook: pathlib walks.

(sp:register-clone-mechanism :block-clone 'block-clone-file
                             :rank +block-clone-rank+
                             :available-p 'block-clone-available-p)
