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

(defparameter *review287-cases*
  '(("TEST-MISSION-TOOLING292-READY" (:artifacts 1))
    ("TEST-MISSION-TOOLING292-RESTARTS" (:retry 3))
    ("TEST-MISSION-TOOLING292-NORMALIZATION" (:valid 1) (:invalid 6))
    ("TEST-MISSION-TOOLING292-PRECLAIM" (:missing-runtime 1))
    ("TEST-MISSION-PROMOTION290-CAS" (:race 1))
    ("TEST-MISSION-PROMOTION290-CRASH-WRITER" (:writer 3))
    ("TEST-MISSION-PROMOTION290-CRASH-READER" (:reader 1))
    ("TEST-MISSION-PROMOTION290-READINESS" (:edit 2) (:generation 2) (:replace 2))
    ("TEST-MISSION-PROMOTION290-READINESS-STALE" (:config 2) (:clear 2))
    ("TEST-MISSION-PROMOTION290-STORAGE" (:absent 1) (:committed 1) (:normal 1))
    ("TEST-MISSION-PROMOTION290-TOMBSTONE" (:absent 1) (:committed 1))
    ("TEST-MISSION-REVIEW289-UNKNOWN-CANCEL" (:cancel 2))
    ("TEST-MISSION-REVIEW289-STALE-WRITER" (:writer 3))
    ("TEST-MISSION-REVIEW289-STALE-READER" (:reader 1))
    ("TEST-MISSION-REVIEW289-CANCEL" (:cancel 2) (:edit 2) (:clear 2))
    ("TEST-MISSION-REVIEW289-PENDING-WRITER" (:writer 2))
    ("TEST-MISSION-REVIEW289-DISPATCHED-WRITER" (:writer 3))
    ("TEST-MISSION-REVIEW289-SETTLED-WRITER" (:writer 3))
    ("TEST-MISSION-REVIEW289-PENDING-READER" (:reader 1))
    ("TEST-MISSION-REVIEW289-DISPATCHED-READER" (:reader 1))
    ("TEST-MISSION-REVIEW289-SETTLED-READER" (:reader 1))
    ("TEST-MISSION-REVIEW288-DIAGNOSIS" (:probe 1))
    ("TEST-MISSION-REVIEW288-POLICY" (:ask 2) (:evaluate 2))
    ("TEST-MISSION-REVIEW288-RETRY" (:retry 3))
    ("TEST-MISSION-REVIEW288-RACE" (:race 1))
    ("TEST-MISSION-REVIEW288-LIFETIMES" (:paused 2) (:blocked 2) (:generation 2))
    ("TEST-MISSION-POLICY269-TERMINALS" (:canonical-policy 6))
    ("TEST-MISSION-REVIEW287-NATIVE" (:native 2))
    ("TEST-MISSION-REVIEW287-REFUSALS" (:fabricated 1) (:foreign 1) (:duplicate 1)
     (:revision 1) (:config 1) (:permission 1) (:append 1))
    ("TEST-MISSION-DRAFT-REVIEW-NATIVE" (:ordinary-native 2))
    ("TEST-MISSION-ACCOUNTING" (:ordinary-accounting 1))
    ("TEST-MISSION-DRAFT-TICKET-ACCOUNTING" (:candidate-accounting 1)))
  "Source-only readable exact outer plus nested native expansion inventory.")

(-> review287--call (function) null)
(defun review287--call (function)
  "Create a genuine candidate beside an ordinary working mission, with no bootstrap ticket."
  (with-test-configuration (configuration)
    (let* ((application   (mission-review-test--prepare configuration
                            (list (mission-review-test--result (draft-review-test--data)))))
           (configuration (application-configuration application)))
      (unwind-protect
           (progn
             (draft-review-test--role configuration :models '("@slow") :reasoning-effort ':high
                                     :tools ':all :spawns ':all)
             (application-mission-draft-edit application
               (mission-test--specification :gates (list (list :id "build" :kind :command :description "Build artifact." :command "true"))))
             (funcall function application))
        (mission-review-test--close application))))
  nil)

(-> test-mission-review287-native () null)
(defun test-mission-review287-native ()
  "Submit a real pre-start candidate review and prove exact one-time ordinary billing."
  (dolist (case (rest (assoc "TEST-MISSION-REVIEW287-NATIVE" *review287-cases* :test #'equal)))
    (assert (equal case '(:native 2)))
    (when (draft-review-test--function 'mission-draft-review-claim)
      (review287--call
       (lambda (application)
         (let* ((working   (application-goal application))
                (before    (copy-tree working))
                (candidate (application-mission-draft application))
                (context   (application-mission-draft-context application))
                (claim     (mission-draft-review-claim application "explicit-test-start"))
                (record    (copy-tree (getf candidate :candidate-claim))))
           (test-assert (and (eq :pending (getf record :state)) (null (getf record :job-id))
                             (getf record :reviewer) (getf record :start-request))
                        "claim exists durably before native identity allocation")
           (let ((job (mission-draft-review-submit application claim)))
             (test-assert (task-job-await job 30) "actual candidate native child terminates")
             (let* ((state (task-job-snapshot job)) (result (getf state :result))
                    (path     (getf result :output-path))
                    (artifact (if path (with-open-file (s path) (let ((*read-eval* nil)) (read s))) result)))
               (format t "~&REVIEW287-NATIVE ~S~%" (list :claim record :job (job-identifier job)
                                                    :execution (task-job-execution-identifier job) :terminal state :artifact artifact
                                                    :candidate candidate :working working))
               (test-assert (eq :completed (getf state :state)) "native contract completes without forged child success")
               (test-assert (eq context (task-job-mission-context job)) "only the claimed native child owns candidate accounting")
               (test-assert (and (= 1 (getf candidate :turns-used)) (= 5 (getf candidate :tokens-used))
                                 (zerop (getf candidate :requests-outstanding)) (zerop (getf candidate :tokens-reserved)))
                            "full billable input plus output charges candidate exactly once and settles")
               (test-assert (and (eq working (application-goal application)) (equal before working))
                            "working identity state usage and history are unchanged")
               (test-assert (and (null (task-agent-definition-tools (mission-draft-review-role claim)))
                                 (null (task-agent-definition-spawns (mission-draft-review-role claim))))
                            "private definition grants no tools or descendants")
               (test-assert (not (search "primary-hidden-reference"
                                         (prin1-to-string (scripted-provider-input-snapshots (application-provider application)))))
                            "candidate context is independent of primary history")
               (test-assert (handler-case
                                 (progn (mission-draft-review-artifact claim (list* :status :success artifact)) nil)
                               (mission-error (condition) (eq :review-result-identity (mission-error-reason condition))))
                            "a fabricated substitute is not the completed native artifact")
               (test-assert (equal "pass" (getf (getf (mission-draft-review-artifact claim artifact) :review) :verdict))
                            "settled current artifact retains explicit verdict as data")
               (test-assert (eq :draft (getf candidate :status)) "review data grants no activation")
               (application-mission-draft-edit application (mission-test--specification))
               (test-assert (handler-case (progn (mission-draft-review-artifact claim artifact) nil)
                              (mission-error () t)) "an edited slot cannot accept an already-paid old result")
               (test-assert (= 5 (getf (application-mission-draft application) :tokens-used))
                            "staling a paid artifact does not erase its ordinary charge")
               (test-assert (handler-case (progn (mission-draft-review-submit application claim) nil)
                              (mission-error () t)) "a consumed host claim cannot dispatch again"))))))))
  nil)

(-> test-mission-review287-refusals () null)
(defun test-mission-review287-refusals ()
  "Refuse forged foreign stale duplicate and unjournaled claims before scheduler identity."
  (dolist (case (rest (assoc "TEST-MISSION-REVIEW287-REFUSALS" *review287-cases* :test #'equal)))
    (when (draft-review-test--function 'mission-draft-review-claim)
      (review287--call
       (lambda (application)
         (let* ((parent        (application-agent application))
                (configuration (application-configuration application))
                (context       (application-mission-draft-context application))
                (provider      (application-provider application))
                (runtime       (tool-registry-runtime-binding (application-tool-registry application) 'task-orchestrator))
                (before        (task-orchestrator-next-session-order runtime))
                (claim         (unless (eq :append (first case))
                                 (mission-draft-review-claim application "start-refusal"))))
           (test-assert
            (handler-case
                (progn
                  (case (first case)
                    (:fabricated
                     (mission-draft-review-submit application '(:candidate-review t)))
                    (:foreign
                     (mission-draft-review-admit claim (make-instance 'agent) nil))
                    (:duplicate
                     (mission-draft-review-claim application "another-start"))
                    (:revision
                     (let ((spec (mission-test--specification)))
                       (application-mission-draft-edit application spec)
                       (application-mission-draft-edit application
                         (mission-test--specification :gates (list (list :id "build" :kind :command
                                                                    :description "Build artifact." :command "true")))))
                     (mission-draft-review-submit application claim))
                    (:config
                     (draft-review-test--role configuration :models '("@slow") :reasoning-effort ':low)
                     (mission-draft-review-submit application claim))
                    (:permission
                     (setf (config :permission-mode configuration) :auto)
                     (mission-draft-review-submit application claim))
                    (:append
                     (test-call-with-function-replacements
                      (list (list 'mission--record (lambda (owner) (declare (ignore owner)) (error "journal unavailable"))))
                      (lambda () (mission-draft-review-claim application "journal-failure"))))
                    (otherwise
                     (error "Unknown expansion")))
                  nil)
              (error (condition)
                (if (eq :append (first case))
                    (and (typep condition 'simple-error)
                         (equal "journal unavailable" (simple-condition-format-control condition)))
                    (and (typep condition 'mission-error)
                         (member (mission-error-reason condition) '(:review-authority :candidate-claimed))))))
            "invalid host claims fail closed with the authentic typed refusal")
           (test-assert (and (= before (task-orchestrator-next-session-order runtime))
                             (null (scripted-provider-input-counts provider))
                             (zerop (getf (mission-context-goal context) :turns-used)))
                        "refusal precedes session allocation provider entry and paid work")
           (assert (eq parent (application-agent application))))))))
  nil)


;;;; -- Candidate Ordinary Policy and Explicit Retry --

(-> test-mission-draft-connection-ownership () null)
(defun test-mission-draft-connection-ownership ()
  "Check candidate recovery ownership at ordinary, streaming and native boundaries."
  (review287--call
   (lambda (application)
     (let* ((context (application-mission-draft-context application))
            (child   (conversation-create (application-configuration application)))
            (pool    (make-job-pool :name "candidate-recovery" :maximum-concurrency 1
                                   :maximum-batch-size 1 :maximum-live-jobs 1 :start-threads-p nil)))
       (unwind-protect
            (let* ((job                               (first (job-pool-submit-batch pool
                                                              (list (list :name "review" :inline-only-p t
                                                                          :function (lambda (ignored) (declare (ignore ignored))))))))
                   (ticket                            (mission-draft-register-review context job))
                   (*mission-admitted-operation*      job)
                   (*mission-candidate-ticket*        ticket)
                   (*provider-connection-owner-check* (lambda () 7)))
              (mission-context-bind context child)
              (labels ((check-owner ()
                         (test-assert (= 7 (funcall *provider-connection-owner-check*))
                                      "current candidate preserves the shorter caller lifetime")
                         (unwind-protect
                              (progn
                                (setf (mission-context-candidate-ticket context) nil)
                                (test-assert
                                 (handler-case (progn (funcall *provider-connection-owner-check*) nil)
                                   (provider-owner-ended () t))
                                 "a revoked candidate ticket stops recovery before another attempt"))
                           (setf (mission-context-candidate-ticket context) ticket))))
                (mission-call-with-provider-operation child #'check-owner)
                (mission--call-with-native-connection-owner context #'check-owner)
                (test-call-with-function-replacements
                 (list (list 'provider-call-with-suspension
                             (lambda (conversation function &key suspended-p)
                               (declare (ignore conversation function suspended-p))
                               (check-owner))))
                 (lambda () (provider-stream-turn (make-instance 'scripted-provider :results nil) child)))))
         (mission-context-bind nil child)
         (job-pool-close pool)))))
  nil)

(-> test-mission-draft-native-command () null)
(defun test-mission-draft-native-command ()
  "Request and collect a real candidate review through native command actions."
  (review287--call
   (lambda (application)
     (application-mission-command application :review-draft)
     (let* ((context (application-mission-draft-context application))
            (claim   (mission-context-candidate-ticket context))
            (job     (mission-draft-ticket-job claim)))
       (test-assert (task-job-await job 30) "native command review terminates")
       (application-mission-command application :collect-draft-review)
       (test-assert (eq :settled (mission-draft-review-state claim)) "native collection settles the exact claim")
       (test-assert (equal "pass" (getf (getf (getf (application-mission-draft application) :candidate-claim) :artifact) :verdict))
                    "native collection retains the explicit verdict"))))
  nil)

(-> review-lifecycle-test--collect (application) mission-draft-review-claim)
(defun review-lifecycle-test--collect (application)
  "Run and collect one fixture-backed native review through the public actions."
  (application-mission-command application :review-draft)
  (let* ((context (application-mission-draft-context application))
         (claim   (mission-context-candidate-ticket context)))
    (test-assert (task-job-await (mission-draft-ticket-job claim) 30) "review terminates")
    (application-mission-command application :collect-draft-review)
    (test-assert (eq :settled (mission-draft-review-state claim)) "collection settles claim")
    claim))

(-> test-mission-draft-review-after-edit () null)
(defun test-mission-draft-review-after-edit ()
  "Review an edited candidate afresh without losing its identity or paid provenance."
  (review287--call
   (lambda (application)
     (let ((data (draft-review-test--data)))
       (setf (gethash "verdict" data) "fail"
             (scripted-provider-results (application-provider application))
             (list (mission-review-test--result data))))
     (let* ((candidate (application-mission-draft application))
            (identity  (getf candidate :candidate-id))
            (working   (copy-tree (application-goal application)))
            (previous  (review-lifecycle-test--collect application))
            (record    (copy-tree (getf candidate :candidate-claim))))
       (application-mission-draft-edit application
         (mission-test--specification
          :gates (list (list :id "build" :kind :command :command "printf revised"))))
       (setf (scripted-provider-results (application-provider application))
             (list (mission-review-test--result (draft-review-test--data))))
       (let* ((next    (review-lifecycle-test--collect application))
              (receipt (application-mission-draft-review-collect application)))
         (test-assert (eq candidate (application-mission-draft application)) "same candidate object")
         (test-assert (equal identity (getf candidate :candidate-id)) "same accounting identity")
         (test-assert (equal working (application-goal application)) "working mission unchanged")
         (test-assert (= 2 (getf candidate :turns-used)) "both paid turns retained")
         (test-assert (= 10 (getf candidate :tokens-used)) "both paid token charges retained")
         (test-assert (equal (list record) (getf candidate :candidate-review-history)) "exact old claim archived")
         (test-assert (not (equal (mission-draft-ticket-review-id previous) (mission-draft-ticket-review-id next)))
                      "distinct review identities")
         (test-assert (not (eq (mission-draft-ticket-job previous) (mission-draft-ticket-job next))) "distinct native jobs")
         (test-assert (> (mission-draft-ticket-revision next) (mission-draft-ticket-revision previous)) "revision advances")
         (test-assert (not (equal (mission-draft-ticket-digest next) (mission-draft-ticket-digest previous))) "edited digest advances")
         (test-assert (= (getf receipt :revision) (getf candidate :specification-revision)) "new receipt revision")
         (test-assert (equal (getf receipt :digest) (getf candidate :specification-digest)) "new receipt digest")
         (test-assert (handler-case (progn (mission-draft-review-submit application previous) nil)
                        (mission-error () t)) "retired authority cannot dispatch")))))
  nil)

(-> test-mission-draft-review-edit-refusals () null)
(defun test-mission-draft-review-edit-refusals ()
  "Refuse older attempts with unsettled or unknown usage without changing ownership."
  (dolist (case '(:assigned :requests-outstanding :tokens-reserved :unknown))
    (review287--call
     (lambda (application)
       (let* ((previous  (review-lifecycle-test--collect application))
              (context   (application-mission-draft-context application))
              (candidate (application-mission-draft application))
              (job       (mission-draft-ticket-job previous)))
         (application-mission-draft-edit application (mission-test--specification))
         (ecase case
           (:assigned
            (setf (mission-draft-review-state previous) :assigned))
           (:requests-outstanding
            (setf (getf candidate :requests-outstanding) 1))
           (:tokens-reserved
            (setf (getf candidate :tokens-reserved) 1))
           (:unknown
            (setf (getf candidate :unknown-usage) 1)))
         (let ((before (copy-tree candidate))
               (state  (task-job-snapshot job)))
           (test-assert (handler-case (progn (application-mission-draft-review application) nil)
                          (mission-error () t)) "unsettled prior work refuses fresh review")
           (test-assert (equal before candidate) "no counter claim or history mutation")
           (test-assert (eq previous (mission-context-candidate-ticket context)) "exact ownership retained")
           (test-assert (equal state (task-job-snapshot job)) "prior job unchanged"))))))
  nil)

(-> test-mission-draft-review-retry-currentness () null)
(defun test-mission-draft-review-retry-currentness ()
  "Keep same-revision FAIL retries explicit and reject them after an edit."
  (review287--call
   (lambda (application)
     (let ((data (draft-review-test--data)))
       (setf (gethash "verdict" data) "fail"
             (scripted-provider-results (application-provider application))
             (list (mission-review-test--result data))))
     (let* ((previous  (review-lifecycle-test--collect application))
            (candidate (application-mission-draft application))
            (before    (copy-tree candidate)))
       (test-assert (mission-draft-review-retryable-p application previous) "current settled FAIL offers retry")
       (test-assert (handler-case (progn (mission-draft-review-claim application "duplicate") nil)
                      (mission-error () t)) "same revision requires explicit retry")
       (test-assert (equal before candidate) "duplicate refusal does not mutate")
       (let ((next (mission-draft-review-retry application previous "explicit-retry")))
         (test-assert (mission-draft-review-current-p next) "explicit retry is current")
         (test-assert (= 5 (getf candidate :tokens-used)) "retry preserves paid usage")
         (application-mission-draft-edit application (mission-test--specification))
         (test-assert (not (mission-draft-review-retryable-p application next)) "edited claim is not retryable")
         (let ((edited (copy-tree candidate)))
           (test-assert (handler-case (progn (mission-draft-review-retry application next "stale-retry") nil)
                          (mission-error () t)) "explicit stale retry refuses")
           (test-assert (equal edited candidate) "stale retry does not mutate"))))))
  nil)
