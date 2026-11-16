;;;; -- Dependency-Free Recovery Bootstrap --

;; Like the standalone root helpers, these definitions precede AUTOLITH and
;; third-party packages. Keep this file readable with only ASDF/UIOP loaded.
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
  (:documentation "A failed recovery prerequisite, retaining its original condition.")
  (:report
   (lambda (condition stream)
     (format stream "Recovery bootstrap failed during ~A (~A): ~A"
             (recovery-bootstrap-error-phase condition)
             (recovery-bootstrap-error-input condition)
             (recovery-bootstrap-error-cause condition)))))

(defun recovery-bootstrap--setup (source-root &key
                                               (override (uiop:getenv "AUTOLITH_PROJECT_SETUP"))
                                               (home (user-homedir-pathname)))
  "Select an explicit setup before project/user setup; NIL permits plain ASDF.
An empty override is absent. An unusable explicit override is an error, never
permission to select another dependency authority. Return a pathname or NIL."
  (if (and override (plusp (length override)))
      (handler-case
          (or (uiop:file-exists-p override)
              (error 'file-error :pathname override))
        (error (condition)
          (error 'recovery-bootstrap-error :phase ':setup-selection
                 :input override :cause condition)))
      (or (uiop:file-exists-p (merge-pathnames ".qlot/setup.lisp" source-root))
          (uiop:file-exists-p (merge-pathnames "quicklisp/setup.lisp" home)))))

(defun recovery-bootstrap--load-dependency (system)
  "Find SYSTEM offline, then load it without interpreting load errors as absence."
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
  "Initialize the selected setup and load genuine recovery libraries offline.
Signal RECOVERY-BOOTSTRAP-ERROR with the initiating condition on refusal. Do not
read the runtime, start interaction, or claim recovery readiness here."
  (let ((setup (recovery-bootstrap--setup source-root :override override :home home)))
    (when setup
      (handler-case (load setup)
        (error (condition)
          (error 'recovery-bootstrap-error :phase ':setup-load
                 :input setup :cause condition))))
    (recovery-bootstrap--load-dependency "serapeum")
    (recovery-bootstrap--load-dependency "sbcl-generations")
    t))
