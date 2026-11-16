#!/usr/bin/env -S sbcl --script

(require :asdf)
(pushnew ".qlot" asdf::*default-source-registry-exclusions* :test #'string=)
(asdf:initialize-source-registry)
(require :sb-posix)
(load (merge-pathnames "bootstrap.lisp" (uiop:pathname-directory-pathname *load-truename*)))

(let* ((arguments       (uiop:command-line-arguments))
       (source-root
         (uiop:ensure-directory-pathname
          (or (first arguments)
               (error "The recovery launcher needs the source root.")))))
  (handler-case (recovery-bootstrap source-root)
    (recovery-bootstrap-error (condition)
      (format *error-output* "~&~A~%" condition)
      (uiop:quit 1)))
  (let ((package (or (find-package "AUTOLITH")
                     (make-package "AUTOLITH" :use '("CL")))))
    (export (mapcar (lambda (name) (intern name package))
                    '("RECOVERY-MAIN" "RECOVERY-IMAGE-SAVE"))
            package))
  (load (merge-pathnames "recovery/runtime.lisp" source-root))
  (uiop:symbol-call '#:autolith '#:recovery-main))
