(in-package #:autolith)

;;;; -- Configured Candidate Reviewer Contract --

(defparameter *mission-draft-review-output-schema*
  '(:type :object
    :properties
    (("verdict" (:type :string :enum ("pass" "fail")))
     ("summary" (:type :string))
     ("coverage" (:type :array :max-items 64
                     :items (:type                  :object
                             :properties
                             (("gate" (:type :string))
                              ("verdict" (:type :string :enum ("pass" "fail")))
                              ("reason" (:type :string)))
                             :required              ("gate" "verdict" "reason")
                             :additional-properties nil))))
    :required ("verdict" "summary" "coverage") :additional-properties nil)
  "Explicit specification review data, never an executed acceptance gate proof.")

(-> mission-draft-review-definition (configuration) (values task-agent-definition list))
(defun mission-draft-review-definition (configuration)
  "Narrow the effective configured reviewer without changing its models or effort.
Return a private definition and its persistable identity snapshot. Discovery and
its precedence diagnostics are identical to ordinary task role discovery. An
unusable model list is refused before the native scheduler can inherit the
primary model. This preparation grants no candidate admission or work authority."
  (multiple-value-bind (definitions diagnostics) (task-discover-agents configuration)
    (let* ((diagnostic (task-find-agent-diagnostic diagnostics "reviewer"))
           (configured (task-find-agent-definition definitions "reviewer")))
      (when diagnostic (error diagnostic))
      (unless configured
        (mission--reject ':review-configuration "No configured reviewer role is available."))
      (let* ((models (task-agent-definition-models configured))
             (model  (loop for alias in models
                           thereis (task--model-alias alias nil))))
        (unless (and model
                     (notany (lambda (alias)
                               (member alias '("@parent" "@auto" "@task") :test #'string-equal))
                             models))
          (mission--reject ':review-configuration
                           "Configured review requires an independently resolved model; primary-model fallback is unavailable."))
        (let* ((definition
                 (task-agent-definition-create
                  :name             (task-agent-definition-name configured)
                  :description      (task-agent-definition-description configured)
                  :instructions
                  (format nil "~A~%Review the supplied candidate specification. Return an explicit pass or fail and cover every supplied gate exactly once. Coverage assesses the specification and never proves execution. Submit the required native output through yield.submit."
                          (task-agent-definition-instructions configured))
                  :source           (task-agent-definition-source configured)
                  :pathname         (task-agent-definition-pathname configured)
                  :models           (copy-list models)
                  :reasoning-effort (task-agent-definition-reasoning-effort configured)
                  :tools nil :spawns nil :blocking-p t
                  :output           *mission-draft-review-output-schema*))
               (child (task-configuration-for-definition configuration definition)))
          (values definition
                  (list :name                        (task-agent-definition-name configured)
                        :source                      (task-agent-definition-source configured)
                        :pathname                    (task-agent-definition-pathname configured)
                        :models                      (copy-list models)
                        :configured-reasoning-effort (task-agent-definition-reasoning-effort configured)
                        :model                       (config :model child)
                        :reasoning-effort            (config :reasoning-effort child)
                        :instructions-digest
                        (mission-draft--digest (list (task-agent-definition-instructions configured)))
                        :tools nil :spawns nil)))))))

(-> mission-draft-review-verdict (list list) list)
(defun mission-draft-review-verdict (result gate-ids)
  "Validate a complete native artifact and exact per-gate specification coverage.
Return the full explicit decision and coverage as data. This function does not
publish PASS, mutate a candidate, approve a start, or execute acceptance gates."
  (unless (and (proper-list-p gate-ids) (<= (length gate-ids) 64)
               (every #'non-empty-string-p gate-ids)
               (= (length gate-ids) (length (remove-duplicates gate-ids :test #'equal))))
    (mission--reject ':review-coverage "Expected review gate identities must be unique bounded strings."))
  (unless (and (eq (getf result :status) ':success)
               (getf result :structured-output-present-p))
    (mission--reject ':review-failed "Review requires a successful complete native output artifact."))
  (let* ((data   (task-sexp->json (getf result :structured-output)))
         (schema (task-output-schema-normalize *mission-draft-review-output-schema*)))
    (unless (task-output-schema-valid-p data schema)
      (mission--reject ':review-contract "Review requires an explicit verdict, summary and per-gate coverage."))
    (let* ((verdict  (gethash "verdict" data))
           (coverage (coerce (gethash "coverage" data) 'list))
           (names    (mapcar (lambda (entry) (gethash "gate" entry)) coverage)))
      (unless (and (non-empty-string-p (gethash "summary" data))
                   (= (length names) (length gate-ids))
                   (= (length names) (length (remove-duplicates names :test #'equal)))
                   (subsetp gate-ids names :test #'equal)
                   (every (lambda (entry) (non-empty-string-p (gethash "reason" entry))) coverage)
                   (or (equal verdict "fail")
                       (every (lambda (entry) (equal "pass" (gethash "verdict" entry))) coverage)))
        (mission--reject ':review-coverage "Review coverage is incomplete, duplicated, foreign or inconsistent with PASS."))
      (list :verdict verdict :summary (gethash "summary" data)
            :coverage (mapcar (lambda (entry)
                                (list :gate    (gethash "gate" entry)
                                      :verdict (gethash "verdict" entry)
                                      :reason  (gethash "reason" entry)))
                              coverage)
            :result   (copy-tree result)))))


;;;; -- Durable Host Claims and Native Admission --

(defclass mission-draft-review-claim (mission-draft-ticket)
  ((parent :initarg :parent :reader mission-draft-review-parent
           :documentation "Exact calling agent, never serialized authority.")
   (definition :initarg :definition :reader mission-draft-review-role
               :documentation "Private narrowed configured role.")
   (snapshot :initarg :snapshot :reader mission-draft-review-snapshot
             :documentation "Persistable configured reviewer identity.")
   (permissions :initarg :permissions :reader mission-draft-review-permissions
                :documentation "Permission mode at host claim creation.")
   (working :initarg :working :reader mission-draft-review-working
            :documentation "Exact ordinary working context beside this draft.")
   (generation :initarg :generation :reader mission-draft-review-generation
               :documentation "Working transition generation.")
   (state :initform :pending :accessor mission-draft-review-state
          :documentation "One-shot pending, reserving or assigned state."))
  (:documentation "Ephemeral host authority backed by a durable non-replayable claim."))

(-> mission-draft-review-current-p (mission-draft-review-claim) boolean)
(defun mission-draft-review-current-p (claim)
  "Revalidate exact slot, monotonic revision, role, permissions and working generation."
  (let* ((context       (mission-draft-ticket-context claim))
         (application   (mission-context-application context))
         (parent        (mission-draft-review-parent claim))
         (configuration (agent-configuration parent))
         (goal          (mission-context-goal context))
         (working       (mission-context-find (application-conversation application))))
    (and (application-candidate-active-p application)
         (eq parent (application-agent application))
         (eq context (application-mission-draft-context application))
         (eq goal (application-mission-draft application))
         (eq (mission-context-conversation context) (application-conversation application))
         (eq claim (mission-context-candidate-ticket context))
         (eq :draft (getf goal :status))
         (equal (mission-draft-ticket-review-id claim) (getf (getf goal :candidate-claim) :review-id))
         (eql (mission-draft-ticket-revision claim) (getf goal :specification-revision))
         (equal (mission-draft-ticket-digest claim) (getf goal :specification-digest))
         (eq working (mission-draft-review-working claim))
         (= (mission-context-transition-revision working) (mission-draft-review-generation claim))
         (equal (config :permission-mode configuration) (mission-draft-review-permissions claim))
         (handler-case
             (multiple-value-bind (definition snapshot) (mission-draft-review-definition configuration)
               (declare (ignore definition))
               (equal snapshot (mission-draft-review-snapshot claim)))
           (error () nil))
         t)))

(-> mission-draft-review-claim (application string &key (:retry-of (option mission-draft-review-claim))) mission-draft-review-claim)
(defun mission-draft-review-claim (application start-request &key retry-of)
  "Durably claim this revision before any session, pool or paid request identity."
  (unless (non-empty-string-p start-request)
    (mission--reject :review-request "Review requires the explicit start request identity."))
  (let* ((context (application-mission-draft-context application))
         (parent  (application-agent application))
         (working (mission-context-find (application-conversation application))))
    (unless (and context working) (mission--reject :review-context "Both exact mission slots are required."))
    (mission-draft-review-tooling application)
    (multiple-value-bind (definition snapshot) (mission-draft-review-definition (agent-configuration parent))
      (with-recursive-lock-held ((mission-context-request-lock context))
        (with-recursive-lock-held ((mission-context-lock context))
          (let* ((goal         (mission-context-goal context))
                 (old          (getf goal :candidate-claim))
                 (prior-ticket (mission-context-candidate-ticket context))
                 (history      (getf goal :candidate-review-history))
                 (claim        (make-instance 'mission-draft-review-claim :context context :job nil
                                             :review-id (make-identifier) :revision (getf goal :specification-revision)
                                             :digest (getf goal :specification-digest) :parent parent
                                             :definition definition :snapshot snapshot
                                             :permissions (config :permission-mode (agent-configuration parent))
                                             :working working :generation (mission-context-transition-revision working))))
            (when retry-of
              (unless (and (eq retry-of prior-ticket)
                           (mission-draft-review-current-p retry-of)
                           (eq :settled (mission-draft-review-state retry-of))
                           (equal "fail" (getf (getf old :artifact) :verdict))
                           (mission-draft-ticket-job retry-of)
                           (job-terminal-p (mission-draft-ticket-job retry-of))
                           (zerop (getf goal :requests-outstanding 0))
                           (zerop (getf goal :tokens-reserved 0)))
                (mission--reject :review-retry "Explicit retry requires the exact settled failed attempt.")))
            (when (and (not retry-of)
                       (or prior-ticket (eql (getf old :revision) (getf goal :specification-revision))))
              (mission--reject :candidate-claimed "Use explicit retry after the current attempt settles."))
            (mission--admit-budget context)
            (setf (mission-context-candidate-ticket context) claim)
            (handler-case
                (progn
                  (when retry-of
                    ;; Preserve the exact shared plist head when adding the first history slot.
                    (unless (get-properties goal '(:candidate-review-history))
                      (nconc goal (list :candidate-review-history nil)))
                    (setf (getf goal :candidate-review-history)
                          (append history (list (copy-tree old)))))
                  (setf (getf goal :candidate-claim)
                        (list :review-id    (mission-draft-ticket-review-id claim)
                              :candidate-id (getf goal :candidate-id)
                              :conversation (conversation-identifier (mission-context-conversation context))
                              :revision (mission-draft-ticket-revision claim) :digest (mission-draft-ticket-digest claim)
                              :start-request start-request :reviewer snapshot
                              :permissions  (mission-draft-review-permissions claim)
                              :working-id   (getf (mission-context-goal working) :id)
                              :working-generation (mission-draft-review-generation claim) :state :pending))
                  (unless (mission-draft-review-current-p claim)
                    (mission--reject :review-stale "Candidate changed before claim journaling."))
                  (mission--record context)
                  claim)
              (serious-condition (condition)
                (setf (getf goal :candidate-claim) old
                      (getf goal :candidate-review-history) history
                      (mission-context-candidate-ticket context) prior-ticket)
                (error condition)))))))))

(-> mission-draft-review-retry (application mission-draft-review-claim string) mission-draft-review-claim)
(defun mission-draft-review-retry (application previous start-request)
  "Explicitly retry a settled FAIL with a new host attempt and unchanged ordinary counters."
  (mission-draft-review-claim application start-request :retry-of previous))

(-> mission-draft-review-admit (t agent list) mission-context)
(defun mission-draft-review-admit (claim parent entries)
  "Consume only exact host authority before scheduler identity allocation."
  (unless (typep claim 'mission-draft-review-claim)
    (mission--reject :review-authority "Review requires the host claim object."))
  (let ((context (mission-draft-ticket-context claim)))
    (with-recursive-lock-held ((mission-context-request-lock context))
      (unless (and (eq parent (mission-draft-review-parent claim))
                   (mission-draft-review-current-p claim)
                   (eq :pending (mission-draft-review-state claim))
                   (null (mission-draft-ticket-job claim)) (= 1 (length entries))
                   (eq (getf (first entries) :definition) (mission-draft-review-role claim))
                   (getf (first entries) :independent-context-p)
                   (not (getf (first entries) :review-checkpoint)))
        (mission--reject :review-authority "Review claim is foreign, stale, replayed or not narrowed."))
      (setf (mission-draft-review-state claim) :reserving))
    context))

(-> mission-draft-review-bind-job (task-job t) t)
(defun mission-draft-review-bind-job (job review-claim)
  "Bind the actual native identity durably before the pool queues the child."
  (when review-claim
    (let* ((claim review-claim) (context (mission-draft-ticket-context claim))
           (goal (mission-context-goal context)))
      (with-recursive-lock-held ((mission-context-request-lock context))
        (unless (and (eq :reserving (mission-draft-review-state claim))
                     (mission-draft-review-current-p claim) (null (mission-draft-ticket-job claim))
                     (eq context (task-job-mission-context job)))
          (mission--reject :review-stale "Claim changed before native identity assignment."))
        (setf (mission-draft-ticket-job claim) job
              (mission-draft-review-state claim) :assigned
              (getf (getf goal :candidate-claim) :job-id) (job-identifier job)
              (getf (getf goal :candidate-claim) :execution-id) (task-job-execution-identifier job)
              (getf (getf goal :candidate-claim) :state) :assigned)
        (mission--record context)))))

(-> mission-draft-review-submit (application t) task-job)
(defun mission-draft-review-submit (application claim)
  "Resolve ordinary caller restrictions, then submit only the claimed native child."
  (unless (and (typep claim 'mission-draft-review-claim)
               (eq application (mission-context-application (mission-draft-ticket-context claim))))
    (mission--reject :review-authority "Claim belongs to another application."))
  (mission-draft-review-tooling application)
  (let* ((parent   (application-agent application))
         (registry (application-tool-registry application))
         (runtime  (tool-registry-runtime-binding registry 'task-orchestrator))
         (goal     (mission-context-goal (mission-draft-ticket-context claim)))
         (entry    (first (task--resolve-items parent runtime (list (mission-draft-review-role claim))
                           :registry registry
                           :items    (list (list :name "candidate-review" :agent "reviewer" :blocking t
                                                 :task (format nil "Review this exact candidate specification as data: ~S" goal)))))))
    (first (task-orchestrator-start-jobs runtime parent (list (list* :independent-context-p t entry)) :review-claim claim))))

(-> mission-draft-review-artifact (mission-draft-review-claim list) list)
(defun mission-draft-review-artifact (claim artifact)
  "Parse after exact origin settles and revalidates; never grant approval or start."
  (let* ((context (mission-draft-ticket-context claim)) (goal (mission-context-goal context))
         (job (mission-draft-ticket-job claim)))
    (with-recursive-lock-held ((mission-context-request-lock context))
      (unless (and (mission-draft-review-current-p claim) job (job-terminal-p job)
                   (zerop (getf goal :requests-outstanding 0)) (zerop (getf goal :tokens-reserved 0))
                   (not (mission--unknown-usage-blocks-p goal)))
        (mission--reject :review-settling "Review origin must be current and fully settled."))
      (let* ((state (task-job-snapshot job))
             (path  (getf (getf state :result) :output-path)))
        (unless (and (eq :completed (getf state :state)) path
                     (equal artifact (with-open-file (s path)
                                       (let ((*read-eval* nil)) (read s)))))
          (mission--reject :review-result-identity "Review artifact must equal the exact completed native child's output.")))
      (let ((verdict (mission-draft-review-verdict artifact (mapcar (lambda (gate) (getf gate :id)) (getf goal :gates)))))
        (setf (getf (getf goal :candidate-claim) :artifact) (copy-tree verdict)
              (getf (getf goal :candidate-claim) :state) :settled
              (mission-draft-review-state claim) :settled)
        (mission--record context)
        (mission--admit-budget context)
        (list :candidate-id (getf goal :candidate-id) :revision (mission-draft-ticket-revision claim)
              :digest (mission-draft-ticket-digest claim) :review-id (mission-draft-ticket-review-id claim)
              :review verdict)))))


;;;; -- Nonworking Promotion Readiness --

(-> mission-draft-review-tooling (application) null)
(defun mission-draft-review-tooling (application)
  "Refuse unavailable native review dependencies before journaling a claim."
  (let* ((registry (application-tool-registry application))
         (runtime  (tool-registry-runtime-binding registry 'task-orchestrator))
         (parent   (application-agent application)))
    (unless (and (typep runtime 'task-orchestrator) (tool-registry-find registry "task" "run"))
      (mission--reject :review-tooling "Candidate review requires the session's native task runtime and task.run."))
    (unless (task-parent-can-spawn-p parent "reviewer" runtime)
      (mission--reject :review-authority "The caller cannot spawn the configured reviewer at this depth.")))
  nil)

(-> mission-draft-review-retryable-p (application t) boolean)
(defun mission-draft-review-retryable-p (application claim)
  "Offer explicit retry only for the current fully settled failed native attempt."
  (and (typep claim 'mission-draft-review-claim)
       (eq application (mission-context-application (mission-draft-ticket-context claim)))
       (mission-draft-review-current-p claim)
       (eq :settled (mission-draft-review-state claim))
       (let* ((goal (mission-context-goal (mission-draft-ticket-context claim)))
              (job  (mission-draft-ticket-job claim)))
         (and job (job-terminal-p job)
              (eq :settled (getf (getf goal :candidate-claim) :state))
              (equal "fail" (getf (getf (getf goal :candidate-claim) :artifact) :verdict))
              (zerop (getf goal :requests-outstanding 0)) (zerop (getf goal :tokens-reserved 0))
              (not (mission--unknown-usage-blocks-p goal)))) t))

(-> application-mission-draft-review (application) t)
(defun application-mission-draft-review (application)
  "Request one native review, or explicitly recover a settled FAIL with a fresh identity."
  (let* ((context  (application-mission-draft-context application))
         (previous (and context (mission-context-candidate-ticket context))))
    (restart-case
        (mission-draft-review-submit application (mission-draft-review-claim application (make-identifier)))
      (abort-candidate-review ()
        :report "Return without changing or cancelling the candidate or its paid review."
        nil)
      (retry-candidate-review (identity)
        :report "Explicitly retry the exact settled failed review with a new request identity and remaining ordinary budget."
        :test   (lambda (condition) (declare (ignore condition)) (mission-draft-review-retryable-p application previous))
        (mission-draft-review-submit application (mission-draft-review-retry application previous identity))))))

(-> application-mission-draft-review-collect (application) list)
(defun application-mission-draft-review-collect (application)
  "Collect only the exact completed child's artifact after ordinary settlement."
  (let* ((context (application-mission-draft-context application))
         (claim   (and context (mission-context-candidate-ticket context)))
         (job     (and (typep claim 'mission-draft-review-claim) (mission-draft-ticket-job claim)))
         (path    (and job (job-terminal-p job) (getf (getf (task-job-snapshot job) :result) :output-path))))
    (unless path (mission--reject :review-settling "The candidate's exact native review artifact is not yet available."))
    (mission-draft-review-artifact claim (with-open-file (s path) (let ((*read-eval* nil)) (read s))))))
