(in-package #:autolith)

;;;; -- Configured Candidate Review Preparation --

(-> draft-review-test--function (symbol) (option function))
(defun draft-review-test--function (name)
  "Find the new preparation interface without masking an absent implementation."
  (test-assert (fboundp name) "the configured candidate review interface exists")
  (and (fboundp name) (symbol-function name)))

(-> draft-review-test--role (configuration &rest t) pathname)
(defun draft-review-test--role (configuration &rest options)
  "Write only a private project role through the actual native role syntax."
  (task-tests--write-native-form
   (merge-pathnames ".autolith/agents/reviewer.sexp" (config :working-directory configuration))
   (apply #'task-tests--role-form "reviewer" "Private configured reviewer"
          "Assess every supplied gate." options)))

(-> test-mission-draft-review-discovery () null)
(defun test-mission-draft-review-discovery ()
  "Keep configured precedence, diagnostics, models and effort without primary fallback."
  (let ((resolve (draft-review-test--function 'mission-draft-review-definition)))
    (when resolve
      (with-test-configuration (configuration)
        (setf configuration (configuration-copy configuration :working-directory (config :data-root configuration)))
        (draft-review-test--role configuration :models '("@slow") :reasoning-effort ':high
                                :tools ':all :spawns ':all)
        (multiple-value-bind (definition snapshot) (funcall resolve configuration)
          (test-assert (and (eq ':project (task-agent-definition-source definition))
                            (equal '("@slow") (task-agent-definition-models definition))
                            (eq ':high (task-agent-definition-reasoning-effort definition)))
                       "private narrowing retains the effective configured role origin, models and reasoning")
          (test-assert (and (null (task-agent-definition-tools definition))
                            (null (task-agent-definition-spawns definition))
                            (task-agent-definition-output definition)
                            (task-agent-definition-blocking-p definition))
                       "candidate reviewers have only the native output contract, no ordinary tools or descendants")
          (test-assert (and (equal "reviewer" (getf snapshot :name))
                            (equal "gpt-6.1-terra" (getf snapshot :model))
                            (equal "high" (getf snapshot :reasoning-effort)))
                       "the host can persist the resolved configured identity"))
        (dolist (models '(nil ("unconfigured-model") ("@parent") ("@auto" "@slow")))
          (draft-review-test--role configuration :models models)
          (test-assert (handler-case (progn (funcall resolve configuration) nil)
                         (mission-error (condition) (eq ':review-configuration (mission-error-reason condition)))
                         (task-agent-definition-error () (equal models '("unconfigured-model"))))
                       "unfunded or parent-relative role models cannot silently use the primary model"))
        (mission-test--write-file
         (merge-pathnames ".autolith/agents/reviewer.sexp" (config :working-directory configuration)) "(invalid role)")
        (test-assert (handler-case (progn (funcall resolve configuration) nil)
                       (task-agent-definition-error () t))
                     "an invalid project role retains its actual diagnostic instead of falling through to bundled review"))))
  nil)

(-> draft-review-test--data () json-object)
(defun draft-review-test--data ()
  "Return an explicit verdict and one complete gate coverage receipt."
  (json-object "verdict" "pass" "summary" "The specification covers its gate."
               "coverage" (vector (json-object "gate" "build" "verdict" "pass"
                                                "reason" "A bounded build checks the artifact."))))

(-> draft-review-test--result (json-object) list)
(defun draft-review-test--result (data)
  "Represent a complete successful native artifact without granting mission authority."
  (list :status ':success :structured-output-present-p t :structured-output (task-json->sexp data)))

(-> test-mission-draft-review-verdict () null)
(defun test-mission-draft-review-verdict ()
  "Reject advisory, partial, duplicate, contradictory and unsuccessful results."
  (let ((parse (draft-review-test--function 'mission-draft-review-verdict)))
    (when parse
      (let ((result (draft-review-test--result (draft-review-test--data))))
        (test-assert (equal "pass" (getf (funcall parse result '("build")) :verdict))
                     "an explicit pass with exact gate coverage is accepted as review data")
        (dolist (mode '(:advisory :missing :foreign :duplicate :contradictory :failed :absent))
          (let* ((data     (draft-review-test--data))
                 (coverage (gethash "coverage" data))
                 (bad      (draft-review-test--result data)))
            (case mode
              (:advisory
               (remhash "verdict" data))
              (:missing
               (setf (gethash "coverage" data) #()))
              (:foreign
               (setf (gethash "gate" (aref coverage 0)) "other"))
              (:duplicate
               (setf (gethash "coverage" data) (vector (aref coverage 0) (aref coverage 0))))
              (:contradictory
               (setf (gethash "verdict" (aref coverage 0)) "fail"))
              (:failed
               (setf (getf bad :status) ':failed))
              (:absent
               (setf (getf bad :structured-output-present-p) nil)))
            (setf (getf bad :structured-output) (task-json->sexp data))
            (test-assert (handler-case (progn (funcall parse bad '("build")) nil)
                           (mission-error () t))
                         "review acceptance requires a successful explicit consistent exact-coverage artifact"))))))

  nil)

(-> test-mission-draft-review-failure () null)
(defun test-mission-draft-review-failure ()
  "Retain explicit FAIL as review data without converting it to PASS."
  (let ((parse (draft-review-test--function 'mission-draft-review-verdict)))
    (when parse
      (let ((data (draft-review-test--data)))
          (setf (gethash "verdict" data) "fail"
                (gethash "verdict" (aref (gethash "coverage" data) 0)) "fail")
          (test-assert (equal "fail" (getf (funcall parse (draft-review-test--result data) '("build")) :verdict))
                       "an explicit failed review is valid data, not a successful review"))))
  nil)

(-> test-mission-draft-review-native () null)
(defun test-mission-draft-review-native ()
  "Exercise the configured private contract through a genuine local native task."
  (let ((resolve (draft-review-test--function 'mission-draft-review-definition))
        (parse   (draft-review-test--function 'mission-draft-review-verdict)))
    (when (and resolve parse)
      (with-test-configuration (configuration)
        (let* ((application   (mission-review-test--prepare
                               configuration (list (mission-review-test--result (draft-review-test--data)))))
               (configuration (application-configuration application))
               (parent        (application-agent application))
               (registry      (application-tool-registry application))
               (runtime       (tool-registry-runtime-binding registry 'task-orchestrator)))
          (unwind-protect
               (progn
                 (draft-review-test--role configuration :models '("@slow") :reasoning-effort ':high
                                         :tools ':all :spawns ':all)
                 (multiple-value-bind (definition snapshot) (funcall resolve configuration)
                   (let* ((entry (first (task--resolve-items
                                         parent runtime (list definition) :registry registry
                                         :items                                     (list (list :name "configured-draft-review" :agent "reviewer"
                                                                                                :blocking t :task "Assess build, return explicit verdict and coverage.")))))
                          (job   (first (task-orchestrator-start-jobs
                                         runtime parent (list (list* :independent-context-p t entry))))))
                     (test-assert (task-job-await job 30) "the genuine native scheduler child terminates")
                     (let* ((job-state (task-job-snapshot job))
                            (result    (getf job-state :result))
                            (path      (getf result :output-path))
                            (artifact  (if path (with-open-file (stream path) (let ((*read-eval* nil)) (read stream))) result))
                            (verdict   (funcall parse artifact '("build"))))
                       (format t "~&DRAFT-REVIEW-NATIVE ~S~%"
                               (list :job (job-identifier job) :execution (task-job-execution-identifier job)
                                     :snapshot snapshot :terminal job-state :verdict verdict))
                       (test-assert (and (eq ':completed (getf job-state :state))
                                         (equal "pass" (getf verdict :verdict)))
                                    "the native output contract produces an explicit complete verdict")
                       (test-assert (and (= 1 (getf (application-goal application) :turns-used))
                                         (= 5 (getf (application-goal application) :tokens-used)))
                                    "this ordinary native contract fixture is billed once, without a reviewer allowance")
                       (test-assert (not (search "primary-hidden-reference"
                                                 (prin1-to-string (scripted-provider-input-snapshots (application-provider application)))))
                                    "the independent reviewer receives no inherited primary history")
                       (test-assert (null (application-mission-draft application))
                                    "preparation and parsing do not manufacture a candidate, review claim or activation")))))
            (mission-review-test--close application))))))
  nil)
