(in-package #:autolith)

;;;; -- Mission Specification Boundaries --

(-> mission-specification-test--policy (&key (:objective string) (:constraints list) (:notes t)) list)
(defun mission-specification-test--policy (&key (objective "Deliver the artifact.") constraints notes)
  "Build a complete policy through the existing direct-start data surface."
  (append (list :objective objective :constraints constraints :notes notes)
          (nthcdr 2 (mission-test--specification))))

(-> mission-specification-test--refused-p (list) boolean)
(defun mission-specification-test--refused-p (specification)
  "Observe a typed refusal at the real policy validator."
  (handler-case (progn (mission--validate-specification specification) nil)
    (mission-error () t)))

(-> mission-specification-test--rules () list)
(defun mission-specification-test--rules ()
  "Return all thirty maximal legal rules with distinct ASCII identities."
  (loop for index below 30
        collect (list :id   (format nil "rule-~2,'0D" index)
                      :text (make-string 200 :initial-element #\x))))

(-> test-mission-specification-validation () null)
(defun test-mission-specification-validation ()
  "Exercise direct/native/slash limits, finite shapes and pre-replacement refusal."
  (let ((rules (mission-specification-test--rules)))
    (dolist (objective (list (make-string 300 :initial-element #\λ)
                            "One sentence?! Another sentence..."
                            "Read https://example.org/a.lisp and version 3.14. Deliver it"
                            "No final punctuation"))
      (let ((goal (mission--validate-specification
                   (mission-specification-test--policy :objective objective :constraints rules
                                                       :notes (make-string 4000 :initial-element #\λ)))))
        (test-assert (and (equal (getf goal :objective) objective)
                          (equal (getf goal :constraints) rules)
                          (= 4000 (length (getf goal :notes ""))))
                     "maximal legal characters and all rules survive direct validation")))
    (dolist (objective (list (make-string 301 :initial-element #\x) "One. Two! Three?" "   "
                            "Use e.g. this. Then finish."))
      (test-assert (mission-specification-test--refused-p
                    (mission-specification-test--policy :objective objective))
                   "overlong, blank and three-group objectives are refused"))
    (dolist (constraints
              (list (append rules (list (list :id "extra" :text "A rule")))
                    '((:id "same" :text "First") (:id "same" :text "Second"))
                    '((:id "UPPER" :text "Rule")) '((:id "é" :text "Rule"))
                    '((:id "a_b" :text "Rule")) '((:id "-a" :text "Rule"))
                    '((:id "a" :text "")) '((:id "a" :text "One. Two."))
                    '((:id "a" :id "b" :text "Rule")) '((:id "a" :text "Rule" :extra t))
                    '((:id "a" :wrong "Rule")) '((:id 1 :text "Rule"))
                    (list (list :id "a" :text (make-string 201 :initial-element #\x)))
                    (list (list :id (make-string 65 :initial-element #\a) :text "Rule"))
                    '((:id "a" :text 1)) '((:id "a" :text "Rule" . :bad))))
      (test-assert (mission-specification-test--refused-p
                    (mission-specification-test--policy :constraints constraints))
                   "invalid rule shape, identity, count and sentence bounds fail closed"))
    (dolist (notes (list 1 (make-string 4001 :initial-element #\x)))
      (test-assert (mission-specification-test--refused-p
                    (mission-specification-test--policy :notes notes)) "notes have an exact typed bound"))
    (let* ((cycle (list (list :id "a" :text "Rule")))
           (shape (mission-specification-test--policy :constraints cycle)))
      (setf (rest cycle) cycle)
      (test-assert (mission-specification-test--refused-p shape) "circular constraints refuse without traversal"))
    (with-test-configuration (configuration)
      (let* ((application (mission-test--application configuration))
             (policy      (mission-specification-test--policy
                           :objective "  Deliver it.  " :constraints '((:id " a " :text "  Never push.  "))
                           :notes "  Background  ")))
        (application-mission-command application :start policy)
        (let ((goal (application-goal application)))
          (test-assert (and (equal "Deliver it." (getf goal :objective))
                            (equal '((:id "a" :text "Never push.")) (getf goal :constraints))
                            (equal "Background" (getf goal :notes))) "trim once before storing accepted fields")
          (setf (getf goal :tokens-used) 7)
          (let ((refused nil))
            (handler-case
                (apply #'application-mission-command application
                       (application-mission-slash-arguments
                        (format nil "start ~S" (mission-specification-test--policy
                                                :objective (make-string 301 :initial-element #\x)))))
              (mission-error (condition)
                (setf refused (and (search ":constraints" (princ-to-string condition))
                                   (search ":notes" (princ-to-string condition))))))
            (test-assert refused "slash failure explains moving rules and background out of the outcome"))
          (test-assert (and (eq goal (application-goal application))
                            (= 7 (getf goal :tokens-used)) (eq :active (getf goal :status)))
                       "invalid start cannot replace or cancel the prior identity or usage")))))
  nil)

(-> test-mission-specification-retention () null)
(defun test-mission-specification-retention ()
  "Roundtrip supplied fields and uncapped legacy policy through the durable reader."
  (with-test-configuration (configuration)
    (let* ((application (mission-test--application configuration))
           (policy      (mission-specification-test--policy :constraints '((:id "local" :text "Never push."))
                                                            :notes       "Historical background")))
      (application-mission-start application policy)
      (let ((context (mission-context-find (application-conversation application))))
        (mission--record context)
        (let* ((loaded (conversation-load (conversation-pathname (application-conversation application))))
               (fresh  (mission-test--application configuration)))
          (setf (application-conversation fresh) loaded)
          (application--load-goal fresh)
          (test-assert (equal (getf (application-goal fresh) :constraints) (getf policy :constraints))
                       "fresh reader and restore retain every supplied rule")
          (test-assert (equal (getf (application-goal fresh) :notes) (getf policy :notes))
                       "fresh restore retains background")
          (application-mission-command fresh :status)
          (test-assert (search "Historical background" (recording-terminal-output
                                                        (terminal-ui-terminal (application-ui fresh))))
                       "real status displays new fields")
          (setf application fresh context (mission-context-find loaded)))
        (let* ((goal      (application-goal application))
               (objective (make-string 4000 :initial-element #\x)))
          (remf goal :constraints)
          (remf goal :notes)
          (setf (getf goal :objective) objective (getf goal :tokens-used) 17)
          (mission--record context)
          (let* ((loaded (conversation-load (conversation-pathname (application-conversation application))))
                 (fresh  (mission-test--application configuration)))
            (setf (application-conversation fresh) loaded)
            (application--load-goal fresh)
            (let ((restored (application-goal fresh)))
              (test-assert (and (equal objective (getf restored :objective))
                                (equal (getf goal :id) (getf restored :id))
                                (= 17 (getf restored :tokens-used))
                                (equal (getf goal :evidence) (getf restored :evidence))
                                (eq :active (getf restored :status))
                                (not (member :constraints restored)) (not (member :notes restored)))
                           "legacy restore preserves exact text, identity, counters and history without migration")))))))
  nil)
