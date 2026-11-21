# fs-backend-windows

MIT. Windows **ReadDirectoryChangesW** change journals for [cl-stack-pathlib](https://github.com/egao1980/cl-stack-pathlib)'s
`fs-watch` (R4b-3 of the cl-stack harness plan). Brief:
[cl-stack/docs/capabilities/fs-backend-windows.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/fs-backend-windows.md).

Loading the system registers the journal mechanism `:rdcw` (rank 100, above the portable `:poll`).
It is available on Windows for the local filesystem on a local volume, so `fs-watch` and
`fs-open-area … :journal :auto` pick it there with no code change; elsewhere it is registered but
unavailable.

```lisp
(asdf:load-system "fs-backend-windows")
(let ((j (cl-stack-pathlib:fs-watch (cl-stack-pathlib:make-local-filesystem) #p"C:/work/project/")))
  ;; … edits …
  (cl-stack-pathlib:journal-drain j)   ; → ("src/a.lisp" "docs") T
  (cl-stack-pathlib:journal-close j))
```

kernel32 is opened through CFFI when the system loads (a system DLL, no native overlay). Each
journal owns one directory handle (shared read / write / delete) and a watcher thread that keeps an
overlapped, recursive `ReadDirectoryChangesW` pending. A drain meets the watcher at a barrier, so
every change made before the drain is reported. Overflow (zero bytes returned,
`ERROR_NOTIFY_ENUM_DIR`) makes the drain incomplete, and the caller rescans. So does a root that was
deleted or moved, for good.

The buffer parser, the path and name rules (UTF-16, 8.3 aliases, case) and the drain barrier are
plain Lisp and are tested on every OS. Tests: `cl-repo test` (Rove). They include
`cl-stack-pathlib/journal-suite` with `:rdcw`, and the Windows tests skip with a note elsewhere.
