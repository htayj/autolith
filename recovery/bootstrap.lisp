;;;; -- Dependency-Free Recovery Bootstrap --

;; Standalone startup precedes AUTOLITH and third-party packages.
(in-package #:cl-user)

(define-condition recovery-bootstrap-error (error)
  ((phase
    :initarg :phase :reader recovery-bootstrap-error-phase
    :documentation "The prerequisite operation that failed.")
   (input
    :initarg :input :reader recovery-bootstrap-error-input
    :documentation "The selected setup pathname or ASDF system name.")
   (cause
    :initarg :cause :reader recovery-bootstrap-error-cause
    :documentation "The initiating condition, retained without replacement."))
  (:documentation "A failed recovery prerequisite with its original condition.")
  (:report
   (lambda (condition stream)
     (format stream "Recovery bootstrap failed during ~A (~A): ~A"
             (recovery-bootstrap-error-phase condition)
             (recovery-bootstrap-error-input condition)
             (recovery-bootstrap-error-cause condition)))))

(defun recovery-bootstrap--setup (source-root &key
                                               (override (uiop:getenv "AUTOLITH_PROJECT_SETUP"))
                                               (home (user-homedir-pathname)))
  "Select explicit, project or user setup, or NIL for plain offline ASDF."
  (handler-case
      (if (and override (plusp (length override)))
          (or (uiop:file-exists-p override)
              (error 'file-error :pathname override))
          (or (uiop:file-exists-p (merge-pathnames ".qlot/setup.lisp" source-root))
              (uiop:file-exists-p (merge-pathnames "quicklisp/setup.lisp" home))))
    (error (condition)
      (error 'recovery-bootstrap-error :phase ':setup-selection
             :input (or override source-root) :cause condition))))

(defun recovery-bootstrap--load-dependency (system)
  "Resolve SYSTEM offline, distinguishing absence, definition and load failures."
  (handler-case (asdf:find-system system)
    (asdf:missing-component (condition)
      (error 'recovery-bootstrap-error :phase ':missing-dependency
             :input system :cause condition))
    (error (condition)
      (error 'recovery-bootstrap-error :phase ':dependency-definition
             :input system :cause condition)))
  (handler-case (asdf:load-system system)
    (error (condition)
      (error 'recovery-bootstrap-error :phase ':dependency-load
             :input system :cause condition))))

(defun recovery-bootstrap (source-root &key
                                        (override (uiop:getenv "AUTOLITH_PROJECT_SETUP"))
                                        (home (user-homedir-pathname)))
  "Load the selected setup once, then genuine recovery libraries through ASDF."
  (let ((setup (recovery-bootstrap--setup source-root :override override :home home)))
    (when setup
      (handler-case (load setup)
        (error (condition)
          (error 'recovery-bootstrap-error :phase ':setup-load
                 :input setup :cause condition))))
    (recovery-bootstrap--load-dependency "serapeum")
    (recovery-bootstrap--load-dependency "sbcl-generations")
    t))
