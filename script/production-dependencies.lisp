;;;; Dependency selection at source and packaged entrypoints, before consumers load.
(require :asdf)
(in-package #:cl-user)

;; Define the optional selector before compiling its callers. Its source helpers
;; contain forward references, so load the complete definition unit together.
(with-compilation-unit ()
  (load (merge-pathnames "dependency-setup.lisp"
                         (uiop:pathname-directory-pathname *load-truename*))))

(defparameter *autolith-dependency-repair-identity*
  "4b956fb21e6afa662f0e8e9e2d1a5087c3f133a01d85c7995a33307b1e703e33"
  "Source-owned manifest identity; verify alongside the Nix JSON projection.")

(defvar *autolith-dependency-selection* nil
  "The explicit provider selection made before dependency loading in this process.")
(defvar *autolith-packaged-dependency-repair* nil
  "Immutable provider identities supplied by the generated Nix project setup.")

(defun autolith-prepared-dependency-sources-p ()
  "Return true for explicit or already selected experimental prepared providers."
  (or (let ((descriptor (uiop:getenv "AUTOLITH_DEPENDENCY_SELECTION")))
        (and descriptor (plusp (length descriptor)) t))
      (eq :prepared-source (getf *autolith-dependency-selection* :kind))))

(defun autolith-require-savable-dependency-sources ()
  "Refuse source-only core publication for experimental prepared providers."
  (when (autolith-prepared-dependency-sources-p)
    (error "Prepared dependency sources are diagnostic only; active-core saving is unsupported."))
  nil)

(defun autolith-setup-dependency-sources (source-root &key consumer)
  "Select an explicitly prepared source candidate or validate packaged providers.
Call immediately after project setup, before any Babel/YASON/CFFI consumer.
AUTOLITH_DEPENDENCY_SELECTION names a single readable keyword-argument list for
SELECT-DERIVED-DEPENDENCY-SOURCES. Preparation is an offline build step, never a
startup side effect. Without either selection, retain the existing source setup
and return NIL, without claiming repaired dependency provenance."
  (when *autolith-dependency-selection*
    (return-from autolith-setup-dependency-sources *autolith-dependency-selection*))
  (let ((descriptor (uiop:getenv "AUTOLITH_DEPENDENCY_SELECTION")))
    (cond
      ((and descriptor (plusp (length descriptor)))
       (load (merge-pathnames "script/dependency-setup.lisp" source-root))
       (let* ((path (pathname descriptor))
              (arguments
                (dependency-source--read
                 (dependency-source--regular-file
                  path (uiop:pathname-directory-pathname path)))))
         (unless (and (listp arguments) (evenp (length arguments))
                      (equal '(:manifest :derived-root :source-root :cache-root :logs :authority)
                             (loop for (key value) on arguments by #'cddr
                                   collect key)))
           (dependency-source--refuse :selection-schema path))
        ;; Internal cold consumers receive separate fresh outputs, not shared FASLs.
        (when consumer
          (unless (and (stringp consumer) (plusp (length consumer))
                       (every (lambda (character)
                                (find character "abcdefghijklmnopqrstuvwxyz0123456789-"))
                              consumer))
            (dependency-source--refuse :consumer-name consumer))
          (dolist (key '(:cache-root :logs))
            (setf (getf arguments key)
                  (merge-pathnames (concatenate 'string consumer "/")
                                   (uiop:ensure-directory-pathname (getf arguments key)))))
          (dependency-source--output-boundary
           (list (getf arguments :cache-root) (getf arguments :logs))
           (list (getf arguments :source-root) (getf arguments :derived-root)
                 (uiop:pathname-directory-pathname (getf arguments :manifest))))
          (when (probe-file (getf arguments :logs))
            (dependency-source--refuse :existing-logs (getf arguments :logs)))
          (ensure-directories-exist (getf arguments :logs)))
         (multiple-value-bind (identity paths)
             (apply #'select-derived-dependency-sources
                    :expected-identity *autolith-dependency-repair-identity* arguments)
           (setf *autolith-dependency-selection*
                 (list :identity identity :asds paths :kind :prepared-source)))))
      (*autolith-packaged-dependency-repair*
       (let ((record *autolith-packaged-dependency-repair*))
        (unless (and (equal *autolith-dependency-repair-identity* (getf record :identity))
                      (= 3 (length (getf record :asds))))
           (error "Invalid packaged dependency repair identity."))
         (loop for system in '("babel" "yason" "cffi")
               for path in (getf record :asds)
               unless (uiop:pathname-equal
                       (truename path)
                       (truename (asdf:system-source-file (asdf:find-system system))))
                 do (error "Packaged dependency selection differs for ~A." system))
         (setf *autolith-dependency-selection* record)))
      (t nil))))
