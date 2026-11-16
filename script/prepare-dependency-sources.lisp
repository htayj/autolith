;;;; Checked offline preparation, loaded before dependency packages exist.
(require :asdf)
(in-package #:cl-user)

(define-condition dependency-source-mismatch (error)
  ((reason :initarg :reason :reader dependency-source-mismatch-reason)
   (path :initarg :path :reader dependency-source-mismatch-path))
  (:documentation "A selected derived dependency candidate failed authentication.")
  (:report (lambda (condition stream)
             (format stream "Dependency source mismatch (~a): ~a"
                     (dependency-source-mismatch-reason condition)
                     (dependency-source-mismatch-path condition)))))

(defun dependency-source--refuse (reason path)
  "Refuse an incompatible candidate before publication or compilation."
  (error 'dependency-source-mismatch :reason reason :path path))

(defun dependency-source--read (path)
  "Read exactly one data form without reader evaluation."
  (with-open-file (stream path)
    (let ((*read-eval* nil) (*package* (find-package '#:cl-user)))
      (let ((value (read stream nil :eof)))
        (unless (eq :eof (read stream nil :eof))
          (dependency-source--refuse :extra-form path))
        value))))

(defun dependency-source--write (path value)
  "Write new readable metadata without overwriting an earlier producer."
  (with-open-file (stream path :direction :output :if-exists :error)
    (let ((*print-readably* t) (*print-pretty* t) (*print-right-margin* 90))
      (write value :stream stream)
      (terpri stream))))

(defun dependency-source--relative (name)
  "Accept only bounded portable relative file paths."
  (unless (and (stringp name) (< 0 (length name) 240)
               (every (lambda (char)
                        (or (alphanumericp char) (find char "-_.+/"))) name)
               (not (char= #\/ (char name 0)))
               (every (lambda (part) (not (member part '("" "." "..")
                                                  :test #'string=)))
                      (uiop:split-string (string-right-trim "/" name)
                                         :separator "/")))
    (dependency-source--refuse :relative-path name))
  name)

(defun dependency-source--files (root)
  "Enumerate canonical regular files with traversal-time entry and depth bounds."
  (let ((entries 0) (files nil))
    (labels ((walk (directory depth)
               (when (or (> depth 16) (> (incf entries) 512))
                 (dependency-source--refuse :tree-bound directory))
               (unless (equal (namestring directory) (namestring (truename directory)))
                 (dependency-source--refuse :linked-directory directory))
               (dolist (file (uiop:directory-files directory))
                 (when (or (> (incf entries) 512) (>= (length files) 256))
                   (dependency-source--refuse :tree-bound file))
                 (push (dependency-source--regular-file file root) files))
               (dolist (child (uiop:subdirectories directory))
                 (walk child (1+ depth)))))
      (walk root 0))
    (unless files
      (dependency-source--refuse :tree-size root))
    (sort files #'string< :key #'namestring)))

(defun dependency-source--run (program arguments logs label)
  "Run an available build utility synchronously with separate retained receipts."
  (let* ((output (merge-pathnames (concatenate 'string label ".out") logs))
         (errors (merge-pathnames (concatenate 'string label ".err") logs))
         (start (get-universal-time))
         (process (sb-ext:run-program program arguments :search nil :wait t
                                     :input nil :output output :error errors))
         (code (sb-ext:process-exit-code process)))
    (dependency-source--write
     (merge-pathnames (concatenate 'string label ".receipt") logs)
     (list :program program :arguments arguments :start start
           :end (get-universal-time) :code code))
    (unless (eql 0 code)
      (dependency-source--refuse :utility-failure output))
    output))

(defun dependency-source--hashes (paths logs label)
  "Hash complete files without loading Babel-dependent checksum libraries."
  (unless (<= 1 (length paths) 256)
    (dependency-source--refuse :hash-batch paths))
  (let* ((names (mapcar #'namestring paths))
         (output (dependency-source--run "/run/current-system/sw/bin/sha256sum"
                                        (cons "--" names) logs label))
         (lines (uiop:read-file-lines output)))
    (unless (= (length lines) (length names))
      (dependency-source--refuse :hash-count output))
    (loop for line in lines for name in names
          collect (progn
                    (unless (and (>= (length line) 66)
                                 (string= name (subseq line 66)))
                      (dependency-source--refuse :hash-output output))
                    (subseq line 0 64)))))

(defun dependency-source--cold ()
  "Reject preparation or selection after a factory consumer has loaded."
  (dolist (name '("BABEL" "BABEL-ENCODINGS" "YASON" "CFFI"))
    (when (find-package name)
      (dependency-source--refuse :preloaded-provider name))))

(defun dependency-source--output-boundary (outputs inputs)
  "Reject noncanonical output paths and writes beneath an input provider."
  (dolist (output outputs)
    (unless (and (uiop:absolute-pathname-p output)
                 (not (intersection '(:up :back) (pathname-directory output))))
      (dependency-source--refuse :output-path output))
    (let ((ancestor (uiop:ensure-directory-pathname output)))
      (loop until (probe-file ancestor)
            do (setf ancestor (uiop:pathname-parent-directory-pathname ancestor)))
      (unless (equal (namestring ancestor) (namestring (truename ancestor)))
        (dependency-source--refuse :linked-output ancestor)))
    (dolist (input inputs)
      (when (uiop:subpathp output input)
        (dependency-source--refuse :output-inside-input output)))))

(defun dependency-source--verify-tree (root records direction logs label)
  "Check every tree member and complete file against the selected manifest."
  (let* ((files (dependency-source--files root))
         (relative (mapcar (lambda (file) (enough-namestring file root)) files)))
    (mapc (lambda (record) (dependency-source--relative (getf record :path))) records)
    (unless (equal relative (mapcar (lambda (record) (getf record :path)) records))
      (dependency-source--refuse :tree-membership root))
    (let ((key (ecase direction (:input :input-bytes) (:output :output-bytes))))
      (unless (equal (mapcar (lambda (file)
                              (with-open-file (stream file :element-type '(unsigned-byte 8))
                                (file-length stream))) files)
                     (mapcar (lambda (record) (getf record key)) records))
        (dependency-source--refuse :tree-length root)))
    (unless (equal (dependency-source--hashes files logs label)
                   (mapcar (lambda (record) (getf record direction)) records))
      (dependency-source--refuse :tree-bytes root))))

(defun dependency-source--regular-file (path root)
  "Require a canonical regular file beneath a canonical selected root."
  (require :sb-posix)
  (unless (and (uiop:absolute-pathname-p path)
               (uiop:absolute-pathname-p root)
               (equal (namestring root) (namestring (truename root)))
               (uiop:subpathp path root)
               (equal (namestring path) (namestring (truename path)))
               (uiop:symbol-call '#:sb-posix '#:s-isreg
                                 (uiop:symbol-call '#:sb-posix '#:stat-mode
                                                   (uiop:symbol-call '#:sb-posix '#:stat
                                                                     (namestring path)))))
    (dependency-source--refuse :noncanonical-input path))
  path)

(defun dependency-source--manifest (manifest authority)
  "Check the explicitly selected provider authority and bounded manifest schema."
  (dependency-source--regular-file manifest (uiop:pathname-directory-pathname manifest))
  (let ((data (dependency-source--read manifest)))
    (unless (and (eql 1 (getf data :version))
                 (eq authority (getf data :authority))
                 (equal '(:babel :yason)
                        (mapcar (lambda (provider) (getf provider :name))
                                (getf data :providers))))
      (dependency-source--refuse :provider-authority manifest))
    (unless (equal '("cffi--cffi/cffi.asd" "cffi--cffi/src/strings.lisp")
                   (mapcar (lambda (record) (getf record :path))
                           (getf data :protocol-files)))
      (dependency-source--refuse :protocol-membership manifest))
    (loop for provider in (getf data :providers)
          for expected in '(("src/enc-iso-2022-jp.lisp" "src/enc-ksc-5601.lisp")
                            ("encode.lisp"))
          do (dolist (key '(:source :destination :patch :asd))
               (dependency-source--relative (getf provider key)))
             (unless (and (equal expected (getf provider :changed-paths))
                          (equal expected
                                 (loop for record in (getf provider :files)
                                       unless (equal (getf record :input) (getf record :output))
                                         collect (getf record :path))))
               (dependency-source--refuse :unexpected-delta manifest)))
    data))

(defun prepare-dependency-sources (&key manifest source-root output-root logs authority)
  "Prepare a distinct checked Babel/YASON candidate offline. Return its directory.
SOURCE-ROOT is an authenticated original provider collection. OUTPUT-ROOT must
be fresh; preparation never modifies originals or publishes compiler caches."
  (dependency-source--cold)
  (dependency-source--output-boundary
   (list output-root logs)
   (list source-root (uiop:pathname-directory-pathname manifest)))
  (let* ((data (dependency-source--manifest manifest authority))
         (patch-root (uiop:pathname-directory-pathname manifest))
         (providers (getf data :providers))
         (patches (mapcar (lambda (provider)
                           (dependency-source--regular-file
                            (merge-pathnames (getf provider :patch) patch-root) patch-root))
                         providers))
         (identity (first (dependency-source--hashes (list manifest) logs "manifest")))
         (destination (merge-pathnames (concatenate 'string identity "/") output-root)))
    (when (probe-file output-root)
      (dependency-source--refuse :existing-output output-root))
    (unless (equal (dependency-source--hashes patches logs "patches")
                   (mapcar (lambda (provider) (getf provider :patch-sha256)) providers))
      (dependency-source--refuse :patch-bytes patch-root))
    ;; Finish all input checks before creating a derived tree.
    (loop for provider in providers for index from 0
          do (dependency-source--verify-tree
              (merge-pathnames (getf provider :source) source-root)
              (getf provider :files) :input logs (format nil "input-~d" index)))
    (let* ((records (getf data :protocol-files))
           (paths (mapcar (lambda (record)
                            (dependency-source--regular-file
                             (merge-pathnames (dependency-source--relative (getf record :path))
                                              source-root) source-root)) records)))
      (unless (equal (dependency-source--hashes paths logs "protocol")
                     (mapcar (lambda (record) (getf record :input)) records))
        (dependency-source--refuse :consumer-protocol source-root)))
    (loop for provider in providers for patch in patches for index from 0
          for source = (merge-pathnames (getf provider :source) source-root)
          for target = (merge-pathnames (getf provider :destination) destination)
          do (dolist (record (getf provider :files))
               (let ((output (merge-pathnames (getf record :path) target)))
                 (ensure-directories-exist output)
                 (uiop:copy-file (merge-pathnames (getf record :path) source) output)))
             (dependency-source--run
              "/run/current-system/sw/bin/patch"
              (list "--batch" "--forward" "--fuzz=0" "--no-backup-if-mismatch"
                    "--reject-file=-" "-p1" "-d" (namestring target)
                    "-i" (namestring patch)) logs (format nil "patch-~d" index))
             (dependency-source--verify-tree target (getf provider :files)
                                              :output logs (format nil "output-~d" index)))
    (dependency-source--write (merge-pathnames "repair.sexp" destination)
                              (list :identity identity :candidate (getf data :candidate)
                                    :authority authority :manifest manifest))
    destination))
