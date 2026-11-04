(in-package #:autolith)

;;;; -- Evaluated Exhaustion --

(-> test-mission-exhaustion-operation-boundary () t)
(defun test-mission-exhaustion-operation-boundary ()
  "An evaluate operation uses its operation cap, not the total mission deadline."
  (with-test-configuration (configuration)
    (let ((application (mission-test--application configuration)))
      (unwind-protect
           (progn
             (application-mission-start application (mission-test--specification))
             (let* ((goal    (application-goal application))
                    (context (mission-context-find (application-conversation application)))
                    (result  nil))
               (if (member :exhaustion-policy goal)
                   (setf (getf goal :exhaustion-policy) :evaluate)
                   (nconc goal (list :exhaustion-policy :evaluate)))
               (setf (getf goal :deadline) (1+ (get-universal-time)))
               (handler-case
                   (setf result (mission--supervise context "Evaluate operation fixture"
                                                   (lambda () (sleep 1.2) :completed)))
                 (mission-error () nil))
               (test-assert (eq result :completed) "Evaluate work outlives the total deadline")
               (test-assert (eq (getf goal :status) :active) "Evaluate operation does not exhaust mission")))
        (terminal-ui-stop (application-ui application))))))


;;;; -- Isolated Evaluator Fixtures --

(defclass mission-exhaustion-test-provider (model-provider)
  ((calls :initform 0 :accessor mission-exhaustion-test-calls
          :documentation "Number of actual mocked provider requests.")
   (response :initform nil :accessor mission-exhaustion-test-response
             :documentation "Scripted response text.")
   (usage :initform (json-object "input_tokens" 2 "output_tokens" 3)
          :accessor mission-exhaustion-test-usage :documentation "Scripted billable usage.")
   (behavior :initform nil :accessor mission-exhaustion-test-behavior
             :documentation "Optional dispatch interleaving.")
   (trace :initform nil :accessor mission-exhaustion-test-trace
          :documentation "Private conversation supplied to the evaluator.")
   (model :initform nil :accessor mission-exhaustion-test-model
          :documentation "Explicit evaluator model selected by configuration."))
  (:documentation "Tool-free provider with no network or credentials."))

(defmethod provider-with-configuration ((provider mission-exhaustion-test-provider)
                                        (configuration configuration))
  "Keep the private scripted provider and observe its selected model."
  (setf (mission-exhaustion-test-model provider) (config :model configuration))
  provider)

(defmethod provider-stream-turn ((provider mission-exhaustion-test-provider)
                                 (conversation conversation) &key tool-namespaces event-callback &allow-other-keys)
  "Return one fixture response, with optional interruption before it returns."
  (declare (ignore tool-namespaces event-callback))
  (setf (mission-exhaustion-test-trace provider) conversation)
  (incf (mission-exhaustion-test-calls provider))
  (when (mission-exhaustion-test-behavior provider)
    (funcall (mission-exhaustion-test-behavior provider)))
  (mission-test--result (mission-exhaustion-test-response provider)
                        :usage (mission-exhaustion-test-usage provider)))

(-> mission-exhaustion-test--data (&key (:decision string) (:limit string) (:amount integer) (:papercuts t)) json-object)
(defun mission-exhaustion-test--data (&key (decision "extend") (limit "turns") (amount 10000) papercuts)
  "Build one evaluator object with independently varied optional candidates."
  (json-object "decision" decision "reason" "Measured verification progress justifies this decision."
               "extensions" (vector (json-object "limit" limit "amount" amount))
               "papercuts" (or papercuts #())))

(-> mission-exhaustion-test--run (function &key (:policy keyword) (:model-p boolean)) t)
(defun mission-exhaustion-test--run (function &key (policy :evaluate) (model-p t))
  "Run FUNCTION with a private mission, mocked strong provider and local redaction."
  (with-test-configuration (configuration)
    (let* ((configuration (configuration-copy configuration
                                             :mission-exhaustion-policy policy
                                             :mission-exhaustion-model  (and model-p (config :model configuration))))
           (provider      (make-instance 'mission-exhaustion-test-provider))
           (application   (mission-inference-test--application configuration provider)))
      (unwind-protect
           (test-call-with-function-replacements
            (list (list 'redact (lambda (text &key language model-directory runtime-library)
                                  (declare (ignore language model-directory runtime-library))
                                  (values text :ok))))
            (lambda ()
              (setf (mission-exhaustion-test-response provider)
                    (json-encode (mission-exhaustion-test--data)))
              (application-mission-start application (mission-test--specification))
              (funcall function application provider
                       (mission-context-find (application-conversation application))
                       (application-goal application))))
        (terminal-ui-stop (application-ui application))))))

(-> mission-exhaustion-test--admit (mission-context) (option keyword))
(defun mission-exhaustion-test--admit (context)
  "Exercise the real exhaustion admission handler, returning a refusal reason."
  (handler-case
      (mission-exhaustion--admitting context (lambda () (mission--admit context)))
    (mission-error (condition) (mission-error-reason condition))))

(-> test-mission-exhaustion-extensions () t)
(defun test-mission-exhaustion-extensions ()
  "Three budget triggers clamp to immutable original steps and charge reviewers only."
  (dolist (trigger '(:turns :tokens :wall))
    (mission-exhaustion-test--run
     (lambda (application provider context goal)
       (let* ((state         (getf goal :exhaustion))
              (before-turns  (getf goal :turns-used))
              (before-tokens (getf goal :tokens-used))
              (originals     (copy-tree (getf state :originals)))
              (key           (ecase trigger (:turns
                                             :turn-limit)
                                            (:tokens
                                             :token-limit)
                                            (:wall
                                             :deadline))))
         (ecase trigger
           (:turns
            (setf (getf goal :turns-used) (getf goal :turn-limit)
                  before-turns (getf goal :turns-used)))
           (:tokens
            (setf (getf goal :tokens-used) (getf goal :token-limit)
                  before-tokens (getf goal :tokens-used)))
           (:wall
            (setf (getf goal :deadline) (get-universal-time))))
         (setf (mission-exhaustion-test-response provider)
               (json-encode (mission-exhaustion-test--data
                             :limit (ecase trigger (:turns
                                                    "turns")
                                                   (:tokens
                                                    "tokens")
                                                   (:wall
                                                    "wall-seconds")))))
         (let ((before (getf goal key)))
           (test-assert (null (mission-exhaustion-test--admit context)) "Extended admission succeeds")
           (test-assert (> (getf goal key) before) "Named exhausted limit increases")
           (test-assert (<= (- (getf goal key) before)
                            (ceiling (getf originals (if (eq trigger :wall) :wall-seconds key)) 2))
                        "Extension step derives from the immutable original"))
         (test-assert (eq goal (application-goal application)) "Exact active goal identity preserved")
         (test-assert (equal originals (getf state :originals)) "Originals immutable")
         (test-assert (= before-turns (getf goal :turns-used)) "Evaluator does not charge mission turns")
         (test-assert (= before-tokens (getf goal :tokens-used)) "Evaluator does not charge mission tokens")
         (test-assert (zerop (getf goal :tokens-reserved)) "Mission reservations untouched")
         (test-assert (zerop (getf goal :unknown-usage)) "Mission unknown usage untouched")
         (test-assert (= 1 (mission-exhaustion-test-calls provider)) "One provider request")
         (test-assert (= 1 (getf (getf state :evaluator) :turns-used)) "One reviewer turn")
         (test-assert (= 5 (getf (getf state :evaluator) :tokens-used)) "Reported reviewer usage settled")
         (test-assert (zerop (getf (getf state :evaluator) :requests-outstanding)) "Reviewer request settled")
         (test-assert (= 1 (getf state :extensions-used)) "One durable extension")
         (test-assert (= 1 (length (getf state :evaluations))) "One durable decision"))))))

(-> test-mission-exhaustion-fallbacks () t)
(defun test-mission-exhaustion-fallbacks ()
  "Stop, malformed decisions, errors, unknown usage, absent model and redaction fail closed."
  (dolist (case '(:stop :malformed :provider :unknown :allowance :model :redaction :timeout))
    (mission-exhaustion-test--run
     (lambda (application provider context goal)
       (declare (ignore application))
       (setf (getf goal :turns-used) (getf goal :turn-limit))
       (case case
         (:stop
          (setf (mission-exhaustion-test-response provider)
                (json-encode (mission-exhaustion-test--data :decision "stop"))))
         (:malformed
          (setf (mission-exhaustion-test-response provider) "{\"decision\":\"invalid\"}"))
         (:provider
          (setf (mission-exhaustion-test-behavior provider) (lambda () (error "fixture failure"))))
         (:unknown
          (setf (mission-exhaustion-test-usage provider) nil))
         (:allowance
          (setf (getf (getf (getf goal :exhaustion) :evaluator) :turns-used) 6))
         (:timeout
          (setf (mission-exhaustion-test-behavior provider) (lambda () (sleep 1)))))
       (let ((*mission-exhaustion-evaluator-milliseconds* (if (eq case :timeout) 30 300000)))
         (if (eq case :redaction)
             (test-call-with-function-replacements
              (list (list 'redact (lambda (&rest ignored) (declare (ignore ignored)) (values nil :unavailable))))
              (lambda () (mission-exhaustion-test--admit context)))
             (mission-exhaustion-test--admit context)))
       (let* ((state (getf goal :exhaustion)) (allowance (getf state :evaluator)))
         (test-assert (eq (getf goal :status) :exhausted) "Fallback keeps existing exhausted semantics")
         (test-assert (null (getf state :pending)) "Trigger finalized")
         (test-assert (= 20 (getf goal :turns-used)) "Mission turns unchanged")
         (test-assert (zerop (getf goal :tokens-used)) "Mission tokens unchanged")
         (test-assert (zerop (getf goal :unknown-usage)) "Evaluator uncertainty never quarantines working mission")
         (test-assert (zerop (getf allowance :requests-outstanding)) "Allowance settled once")
         (test-assert (<= (mission-exhaustion-test-calls provider) 1) "No repair request")
         (test-assert (= 1 (length (getf state :evaluations))) "One decision record")
         (when (member case '(:unknown :provider :timeout))
           (test-assert (= 1 (getf allowance :unknown-usage)) "Dispatched unknown reviewer usage retained"))))
     :model-p (not (eq case :model)))))

(-> test-mission-exhaustion-ceilings-and-restore () t)
(defun test-mission-exhaustion-ceilings-and-restore ()
  "Two extensions reach the original ceiling; reload preserves progress and notices."
  (mission-exhaustion-test--run
   (lambda (application provider context goal)
     (dotimes (index 2)
       (setf (getf goal :turns-used) (getf goal :turn-limit))
       (test-assert (null (mission-exhaustion-test--admit context)) "Bounded extension admits")
       (test-assert (= (1+ index) (getf (getf goal :exhaustion) :extensions-used)) "Count advances once"))
     (test-assert (= 40 (getf goal :turn-limit)) "Ceiling is twice original twenty, not compounded")
     (let* ((conversation (application-conversation application))
            (loaded       (conversation-load (conversation-pathname conversation)))
            (saved        (copy-tree (getf goal :exhaustion))))
       (setf (application-conversation application) loaded
             (application-goal application) (copy-tree (rest (find :goal (reverse (conversation-records-newest loaded 1000)) :key #'first))))
       (mission-restore application)
       (setf goal (application-goal application)
             context (mission-context-find loaded))
       (test-assert (equal saved (getf goal :exhaustion)) "Restore preserves originals, count, progress and audit"))
     (setf (getf goal :turns-used) (getf goal :turn-limit))
     (mission-exhaustion-test--admit context)
     (test-assert (eq (getf goal :status) :exhausted) "Third evaluation cannot extend")
     (test-assert (= 3 (mission-exhaustion-test-calls provider)) "Third trigger has one stop evaluation"))))

(-> test-mission-exhaustion-concurrency () t)
(defun test-mission-exhaustion-concurrency ()
  "Concurrent admissions share one provider request and retry outside mission locks."
  (mission-exhaustion-test--run
   (lambda (application provider context goal)
     (declare (ignore application))
     (setf (getf goal :turns-used) (getf goal :turn-limit)
           (mission-exhaustion-test-behavior provider) (lambda () (sleep 0.1)))
     (let ((first  (mission-inference-test--start (lambda () (mission-exhaustion-test--admit context))))
           (second (mission-inference-test--start (lambda () (mission-exhaustion-test--admit context)))))
       (mission-inference-test--join (list first second) (bordeaux-threads:make-semaphore :count 0) 0)
       (test-assert (null (mission-inference-test-thread-condition first)) "First waiter completed")
       (test-assert (null (mission-inference-test-thread-condition second)) "Second waiter completed")
       (test-assert (= 1 (mission-exhaustion-test-calls provider)) "Concurrent trigger evaluated once")
       (test-assert (= 1 (getf (getf goal :exhaustion) :extensions-used)) "Concurrent extension applied once")))))

(-> test-mission-exhaustion-stale-and-cancel () t)
(defun test-mission-exhaustion-stale-and-cancel ()
  "Stop or replacement during inference settles the old allowance and never applies limits."
  (dolist (replace-p '(nil t))
    (mission-exhaustion-test--run
     (lambda (application provider context goal)
       (setf (getf goal :turns-used) (getf goal :turn-limit)
             (mission-exhaustion-test-behavior provider)
             (lambda ()
               (if replace-p
                   (application-mission-start application (mission-test--specification))
                   (application-mission-stop application :cancelled "fixture stop"))))
       (mission-exhaustion-test--admit context)
       (test-assert (= 20 (getf goal :turn-limit)) "Stale evaluator never changes old limits")
       (test-assert (= 5 (getf (getf (getf goal :exhaustion) :evaluator) :tokens-used)) "Old reviewer usage retained")
       (test-assert (eq :stale (getf (first (getf (getf goal :exhaustion) :evaluations)) :decision)) "Stale result audited")
       (when replace-p
         (test-assert (zerop (getf (application-goal application) :tokens-used)) "Replacement never charged")
         (test-assert (zerop (getf (getf (getf (application-goal application) :exhaustion) :evaluator) :turns-used))
                      "Replacement reviewer allowance untouched"))))))

(-> mission-exhaustion-test--candidate (&key (:citation string)) json-object)
(defun mission-exhaustion-test--candidate (&key (citation "E5"))
  "Produce an evidence-backed diagnostic without private details."
  (json-object "title" "Harness accounting counter became negative"
               "issue_kind" "broken_tool"
               "diagnostic" "Expected nonnegative request accounting after settlement; observed a negative reservation counter. The accounting settlement order is the suspected cause."
               "expected" "Nonnegative accounting counters" "observed" "Negative reservation counter"
               "citations" (vector citation)))

(-> test-mission-exhaustion-papercuts () t)
(defun test-mission-exhaustion-papercuts ()
  "Private store deduplication and invalid candidates cannot alter a valid extension."
  (mission-exhaustion-test--run
   (lambda (application provider context goal)
     (declare (ignore application))
     (setf (getf goal :turns-used) 20 (getf goal :tokens-reserved) -1)
     (let ((valid   (mission-exhaustion-test--candidate))
           (invalid (mission-exhaustion-test--candidate :citation "E1")))
       (setf (mission-exhaustion-test-response provider)
             (json-encode (mission-exhaustion-test--data :papercuts (vector valid valid invalid 42))))
       (mission-exhaustion-test--admit context)
       (let ((record (first (getf (getf goal :exhaustion) :evaluations))))
         (test-assert (eq :extend (getf record :decision)) "Invalid candidates do not reject valid decision")
         (test-assert (= 1 (getf record :new-papercuts)) "Only newly created report counted")
         (test-assert (equal '(:created :duplicate :invalid :invalid)
                             (mapcar (lambda (outcome) (getf outcome :outcome)) (getf record :papercuts)))
                      "Central in-batch dedup and candidate outcomes truthful")
         (test-assert (= 5 (getf (getf (getf goal :exhaustion) :evaluator) :tokens-used)) "Papercuts need no extra request")
         (test-assert (zerop (getf goal :tokens-used)) "Papercuts never charge mission"))))))

(-> test-mission-exhaustion-papercut-failures () t)
(defun test-mission-exhaustion-papercut-failures ()
  "Reporting failures, unconfirmed writes and malformed candidate fields preserve decisions."
  (dolist (mode '(:failed :unconfirmed :invalid))
    (mission-exhaustion-test--run
     (lambda (application provider context goal)
       (declare (ignore application))
       (setf (getf goal :turns-used) 20 (getf goal :tokens-reserved) -1
             (mission-exhaustion-test-response provider)
             (json-encode (mission-exhaustion-test--data
                           :papercuts (vector (if (eq mode :invalid)
                                                  (json-object "title" 42 "citations" 1)
                                                  (mission-exhaustion-test--candidate))))))
       (test-call-with-function-replacements
        (list (list 'papercut-report (lambda (&rest arguments)
                                      (declare (ignore arguments))
                                      (if (eq mode :failed) (error "fixture store failure") (values nil nil)))))
        (lambda () (mission-exhaustion-test--admit context)))
       (let ((record (first (getf (getf goal :exhaustion) :evaluations))))
         (test-assert (eq (getf record :decision) :extend) "Store outcome cannot change valid decision")
         (test-assert (zerop (getf record :new-papercuts)) "No unconfirmed success count")
         (test-assert (eq mode (getf (first (getf record :papercuts)) :outcome)) "Failure outcome recorded"))))))

(-> test-mission-exhaustion-policy () t)
(defun test-mission-exhaustion-policy ()
  "Capture explicit policy, persist settings and conservatively handle legacy goals."
  (with-test-configuration (configuration)
    (test-assert (eq :ask (config :mission-exhaustion-policy configuration)) "Default policy asks")
    (preferences-store configuration :mission-exhaustion-policy :evaluate)
    (preferences-store configuration :mission-extension-limit 3)
    (preferences-store configuration :mission-extension-multiple 4)
    (let ((stored (preferences-load-values configuration)))
      (test-assert (eq :evaluate (getf stored :mission-exhaustion-policy)) "Durable policy")
      (test-assert (= 3 (getf stored :mission-extension-limit)) "Durable extension count")
      (test-assert (= 4 (getf stored :mission-extension-multiple)) "Durable ceiling multiple")))
  (mission-exhaustion-test--run
   (lambda (application provider context goal)
     (declare (ignore context))
     (application-mission-start application (append (mission-test--specification) '(:exhaustion-policy :ask)))
     (let ((goal    (application-goal application))
           (context (mission-context-find (application-conversation application))))
       (test-assert (eq :ask (mission-exhaustion--policy context)) "Per-mission override wins")
       (remf goal :exhaustion-policy)
       (test-assert (eq :ask (mission-exhaustion--policy context)) "Legacy policy asks")
       (setf (getf goal :turns-used) 20)
       (mission-exhaustion-test--admit context)
       (test-assert (eq :exhausted (getf goal :status)) "Legacy mission stops at exhaustion")
       (test-assert (zerop (mission-exhaustion-test-calls provider)) "Ask makes no evaluator request"))
     (test-assert (eq :evaluate (getf goal :exhaustion-policy)) "Original policy captured")
     (test-assert (eq :invalid
                      (handler-case
                          (progn (mission--validate-specification
                                  (append (mission-test--specification) '(:exhaustion-policy :invalid))) nil)
                        (mission-error (condition) (mission-error-reason condition))))
                  "Invalid per-mission policy rejected"))))

(-> test-mission-exhaustion-runtime-caps () t)
(defun test-mission-exhaustion-runtime-caps ()
  "Exercise the real operation cap, shorter gate deadline and preparation deadline."
  (mission-exhaustion-test--run
   (lambda (application provider context goal)
     (declare (ignore application))
     (dolist (gate-p '(nil t))
       (let* ((*mission-operation-milliseconds* (if gate-p 500 30))
              (reason                           (handler-case
                                                    (progn (apply #'mission--supervise context "Bounded operation fixture"
                                                                               (lambda () (sleep 1)) (and gate-p (list :timeout 30))) nil)
                                                  (mission-error (condition) (mission-error-reason condition)))))
         (test-assert (eq reason (if gate-p :gate-timeout :operation-timeout)) "Correct runtime boundary")
         (test-assert (eq :active (getf goal :status)) "Operation timeout leaves total-budget policy to admission")))
     (setf (getf goal :turns-used) 20)
     (let ((*mission-exhaustion-evaluator-milliseconds* 30))
       (test-call-with-function-replacements
        (list (list 'redact (lambda (&rest ignored) (declare (ignore ignored)) (sleep 1) (values "" :ok))))
        (lambda () (mission-exhaustion-test--admit context))))
     (test-assert (eq :exhausted (getf goal :status)) "Preparation timeout falls back to ask")
     (test-assert (zerop (mission-exhaustion-test-calls provider)) "No request after preparation timeout"))))

(-> test-mission-exhaustion-nonlocal-cancellation () t)
(defun test-mission-exhaustion-nonlocal-cancellation ()
  "Nonlocal exits before and after usage extraction settle exactly once."
  (dolist (known-p '(nil t))
    (mission-exhaustion-test--run
     (lambda (application provider context goal)
       (declare (ignore application))
       (setf (getf goal :turns-used) 20)
       (catch 'exhaustion-fixture-cancel
         (if known-p
             (let ((sanitize (symbol-function 'provider--sanitize-wire-string)))
               (test-call-with-function-replacements
                (list (list 'provider--sanitize-wire-string
                            (lambda (text)
                              (when (plusp (mission-exhaustion-test-calls provider))
                                (throw 'exhaustion-fixture-cancel :cancelled))
                              (funcall sanitize text))))
                (lambda () (mission-exhaustion-test--admit context))))
             (progn
               (setf (mission-exhaustion-test-behavior provider)
                     (lambda () (throw 'exhaustion-fixture-cancel :cancelled)))
               (mission-exhaustion-test--admit context))))
       (let* ((state (getf goal :exhaustion)) (allowance (getf state :evaluator)))
         (test-assert (eq :exhausted (getf goal :status)) "Interrupted evaluation stops")
         (test-assert (null (getf state :pending)) "Interrupted trigger finalized")
         (test-assert (= (if known-p 5 0) (getf allowance :tokens-used)) "Known usage retained")
         (test-assert (= (if known-p 0 1) (getf allowance :unknown-usage)) "Unknown usage isolated")
         (test-assert (zerop (getf allowance :requests-outstanding)) "Reservation settled")
         (test-assert (zerop (getf allowance :tokens-reserved)) "Reservation released")
         (mission-exhaustion-test--admit context)
         (test-assert (= 1 (mission-exhaustion-test-calls provider)) "Cancellation never repeats request")
         (test-assert (= 1 (length (getf state :evaluations))) "Cancellation recorded once"))))))

(-> test-mission-exhaustion-interrupted-restore () t)
(defun test-mission-exhaustion-interrupted-restore ()
  "Restore an admitted reviewer request without redispatch or duplicate fallback."
  (mission-exhaustion-test--run
   (lambda (application provider context goal)
     (let* ((state (getf goal :exhaustion)) (allowance (getf state :evaluator)))
       (setf (getf state :pending) '(:id 1 :trigger :turns)
             (getf allowance :requests-outstanding) 1
             (getf allowance :tokens-reserved) 25
             (getf allowance :turns-used) 1)
       (mission--record context)
       (let ((loaded (conversation-load (conversation-pathname (application-conversation application)))))
         (setf (application-conversation application) loaded
               (application-goal application)
               (copy-tree (rest (find :goal (reverse (conversation-records-newest loaded 1000)) :key #'first)))))
       (mission-restore application)
       (setf goal (application-goal application) state (getf goal :exhaustion) allowance (getf state :evaluator))
       (test-assert (eq :exhausted (getf goal :status)) "Interrupted reviewer restores exhausted")
       (test-assert (= 1 (getf allowance :unknown-usage)) "Interrupted usage quarantines reviewer only")
       (test-assert (zerop (getf goal :unknown-usage)) "Working accounting untouched")
       (test-assert (zerop (getf allowance :requests-outstanding)) "Interrupted reservation cleared")
       (test-assert (null (getf state :pending)) "Interrupted pending cleared")
       (mission-restore application)
       (test-assert (= 1 (length (getf state :evaluations))) "Repeated restore does not repeat decision")
       (test-assert (zerop (mission-exhaustion-test-calls provider)) "Restore never redispatches")))))

(-> test-mission-exhaustion-durable-trace () t)
(defun test-mission-exhaustion-durable-trace ()
  "Reload private input, sanitized response and measured usage records."
  (mission-exhaustion-test--run
   (lambda (application provider context goal)
     (declare (ignore application))
     (setf (getf goal :turns-used) 20
           (getf (first (getf goal :criteria)) :evidence) "Measured artifact hash and passing verification")
     (mission-exhaustion-test--admit context)
     (let* ((record  (first (getf (getf goal :exhaustion) :evaluations)))
            (trace   (conversation-load (conversation-pathname (mission-exhaustion-test-trace provider))))
            (records (conversation-records-newest trace 100)))
       (test-assert (equal (getf record :trace) (conversation-identifier trace)) "Durable trace reference resolves")
       (test-assert (test-object-contains-string-p records "Measured artifact hash") "Substantive criterion evidence retained")
       (test-assert (find :exhaustion-response records :key #'first) "Sanitized response durable")
       (test-assert (find :exhaustion-evaluation records :key #'first) "Decision durable")
       (let ((usage (find :exhaustion-usage records :key #'first)))
         (test-assert (= 5 (getf (rest usage) :tokens)) "Measured reviewer usage durable"))))))

(-> test-mission-exhaustion-independent-candidates () t)
(defun test-mission-exhaustion-independent-candidates ()
  "Reject malformed candidates independently and report even after a malformed decision."
  (dolist (mode '(:unknown-citation :bad-kind :oversize :secret-field :malformed-decision :existing))
    (mission-exhaustion-test--run
     (lambda (application provider context goal)
       (setf (getf goal :turns-used) 20 (getf goal :tokens-reserved) -1)
       (let* ((candidate (mission-exhaustion-test--candidate))
              (data      (mission-exhaustion-test--data :papercuts (vector candidate))))
         (case mode
           (:unknown-citation
            (setf (gethash "citations" candidate) #("unknown")))
           (:bad-kind
            (setf (gethash "issue_kind" candidate) "bad-kind"))
           (:oversize
            (setf (gethash "title" candidate) (make-string 201 :initial-element #\a)))
           (:secret-field
            (setf (gethash "raw_private_data" candidate) "excluded fixture"))
           (:malformed-decision
            (setf (gethash "decision" data) "invalid"))
           (:existing
            (papercut-report
             (application-configuration application) :title (gethash "title" candidate)
             :content                                       (mission-exhaustion--text (list :expected (gethash "expected" candidate)
                                                                                          :observed   (gethash "observed" candidate)
                                                                                          :citations  (gethash "citations" candidate)))
             :diagnostic (gethash "diagnostic" candidate) :issue-kind "broken_tool" :tool "mission.exhaustion")))
         (setf (mission-exhaustion-test-response provider) (json-encode data))
         (mission-exhaustion-test--admit context)
         (let* ((record  (first (getf (getf goal :exhaustion) :evaluations)))
                (outcome (getf (first (getf record :papercuts)) :outcome)))
           (test-assert (eq (if (eq mode :malformed-decision) :fallback :extend) (getf record :decision))
                        "Candidate validity independent of decision")
           (test-assert (eq (case mode (:existing
                                        :duplicate)
                                       (:malformed-decision
                                        :created)
                                       (t
                                        :invalid)) outcome)
                        (format nil "Candidate outcome truthful (~S: ~S)" mode outcome))
           (test-assert (= (if (eq mode :malformed-decision) 1 0) (getf record :new-papercuts))
                        (format nil "Only confirmed new reports counted (~S)" mode))
           (test-assert (= 1 (mission-exhaustion-test-calls provider)) "No candidate repair inference")))))))

(-> test-mission-exhaustion-private-redaction () t)
(defun test-mission-exhaustion-private-redaction ()
  "Persist only locally redacted objective and response fields."
  (mission-exhaustion-test--run
   (lambda (application provider context goal)
     (declare (ignore application))
     (let ((data (mission-exhaustion-test--data)))
       (setf (getf goal :turns-used) 20
             (getf goal :objective) "SENSITIVE-FIXTURE objective"
             (gethash "reason" data) "SENSITIVE-FIXTURE reason"
             (mission-exhaustion-test-response provider) (json-encode data))
       (test-call-with-function-replacements
        (list (list 'redact
                    (lambda (text &rest options)
                      (declare (ignore options))
                      (values (if (search "SENSITIVE-FIXTURE" text) "Redacted fixture" text) :ok))))
        (lambda () (mission-exhaustion-test--admit context))))
     (let* ((trace   (conversation-load (conversation-pathname (mission-exhaustion-test-trace provider))))
            (records (conversation-records-newest trace 100)))
       (test-assert (test-object-contains-string-p records "Redacted fixture") "Local redaction retained")
       (test-assert (not (test-object-contains-string-p records "SENSITIVE-FIXTURE")) "Private trace excludes sensitive fixture")
       (test-assert (not (test-object-contains-string-p (getf goal :exhaustion) "SENSITIVE-FIXTURE"))
                    "Compact audit excludes sensitive fixture")))))
