;;;; Explicit derived-candidate selection before loading dependency systems.
(in-package #:cl-user)
(load (merge-pathnames "prepare-dependency-sources.lisp"
                       (uiop:pathname-directory-pathname *load-truename*)))

(defun select-derived-dependency-sources (&key manifest derived-root source-root
                                             cache-root logs authority expected-identity)
  "Authenticate and register a prepared candidate before Babel/YASON/CFFI load.
Call after the selected project setup. This opt-in adapter does not prepare
sources, install dependencies, or affect ordinary installed/recovery launches.
Return the manifest identity and the actual selected ASD paths."
  (dependency-source--cold)
  (dependency-source--output-boundary
   (list cache-root logs)
   (list source-root derived-root (uiop:pathname-directory-pathname manifest)))
  (let* ((data (dependency-source--manifest manifest authority))
         (identity (first (dependency-source--hashes (list manifest) logs "selected-manifest")))
         (marker (dependency-source--read (merge-pathnames "repair.sexp" derived-root)))
         (cache (merge-pathnames (concatenate 'string identity "/") cache-root))
         (paths nil))
    (when (and expected-identity (not (equal expected-identity identity)))
      (dependency-source--refuse :source-repair-identity manifest))
    (unless (equal identity (getf marker :identity))
      (dependency-source--refuse :derived-identity derived-root))
    (when (probe-file cache-root)
      (dependency-source--refuse :existing-cache cache-root))
    (unless (equal
             (dependency-source--hashes
              (mapcar (lambda (provider)
                        (dependency-source--regular-file
                         (merge-pathnames (getf provider :patch)
                                          (uiop:pathname-directory-pathname manifest))
                         (uiop:pathname-directory-pathname manifest)))
                      (getf data :providers)) logs "selected-patches")
             (mapcar (lambda (provider) (getf provider :patch-sha256))
                     (getf data :providers)))
      (dependency-source--refuse :patch-bytes manifest))
    (loop for provider in (getf data :providers) for index from 0
          for root = (merge-pathnames (getf provider :destination) derived-root)
          do (dependency-source--verify-tree root (getf provider :files) :output
                                              logs (format nil "selected-tree-~d" index))
             (push (merge-pathnames (getf provider :asd) root) paths))
    (let* ((records (getf data :protocol-files))
           (protocol-paths
             (mapcar (lambda (record)
                      (dependency-source--regular-file
                       (merge-pathnames (dependency-source--relative (getf record :path))
                                        source-root) source-root)) records)))
      (unless (equal (dependency-source--hashes protocol-paths logs "selected-consumer")
                     (mapcar (lambda (record) (getf record :input)) records))
        (dependency-source--refuse :consumer-protocol source-root))
      (push (first protocol-paths) paths))
    ;; A fresh identity-keyed translation excludes borrowed FASLs and cores.
    (ensure-directories-exist cache)
    (asdf:initialize-output-translations
     `(:output-translations ("/" (,cache :**/ :*.*.*))
       :ignore-inherited-configuration))
    (setf paths (nreverse paths))
    (asdf:initialize-source-registry
     `(:source-registry
       ,@(mapcar (lambda (path)
                   `(:directory ,(uiop:pathname-directory-pathname path))) paths)
       (:tree ,source-root) :ignore-inherited-configuration))
    (loop for path in paths
          for root in (list derived-root derived-root source-root)
          do (dependency-source--regular-file path root)
             (asdf:load-asd path))
    (loop for system in '("babel" "yason" "cffi") for path in paths
          unless (equal (truename path)
                        (truename (asdf:system-source-file (asdf:find-system system))))
            do (dependency-source--refuse :asdf-selection path))
    (dependency-source--write (merge-pathnames "selected.sexp" logs)
                              (list :identity identity :authority authority
                                    :derived-root derived-root :asds paths :cache cache))
    (values identity paths)))
