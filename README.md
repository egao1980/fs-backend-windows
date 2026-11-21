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

## Block cloning (`:block-clone`, 0.2.0)

Loading the system also registers the clone mechanism `:block-clone` (rank 100) for
cl-stack-pathlib 0.10.0's `fs-clone-tree` / `fs-clone-file` / `fs-open-area`. On ReFS volumes
(a Dev Drive is one) each regular file is cloned with `FSCTL_DUPLICATE_EXTENTS_TO_FILE`: the clone
shares the source's clusters until either side is written. NTFS and FAT volumes answer
`:unsupported-volume`, two volumes `:cross-volume`, and `:auto` then copies.

```lisp
(cl-stack-pathlib:clone-tree #p"R:/work/project/" #p"R:/work/area/")   ; → report, :block-clone
```

The chunk plan, the error mapping, the volume flags and the ioctl encoding are plain Lisp, tested on
every OS. The Windows tests run on NTFS in the canned job; the ReFS tests (content, sparse files,
non-BMP names, cluster sharing via `FSCTL_GET_RETRIEVAL_POINTERS`, the clone suite) run in the `refs`
job on a ReFS VHDX, or locally with `FS_BACKEND_WINDOWS_REFS_DIR` set to a directory on ReFS.
