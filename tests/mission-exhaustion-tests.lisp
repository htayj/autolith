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
                                                   (lambda ()
                                                     (setf (getf goal :deadline) (1- (get-universal-time)))
                                                     :completed)))
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
          (setf (mission-exhaustion-test-behavior provider)
                (lambda () (sb-thread:wait-on-semaphore (sb-thread:make-semaphore :count 0) :timeout 2)))))
       (let ((*mission-exhaustion-evaluator-milliseconds* (if (eq case :timeout) 30 300000)))
         (if (eq case :redaction)
             (test-call-with-function-replacements
              (list (list 'redact (lambda (&rest ignored) (declare (ignore ignored)) (values nil :unavailable))))
              (lambda () (mission-exhaustion-test--admit context)))
             (mission-exhaustion-test--admit context)))
       (let* ((state (getf goal :exhaustion)) (allowance (getf state :evaluator)))
         (test-assert (if (eq case :stop) (eq (getf goal :status) :exhausted) (and (eq (getf goal :status) :active) (getf state :review-required))) "Valid STOP exhausts; other nonextensions latch review")
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
     (test-assert (and (eq (getf goal :status) :active) (getf (getf goal :exhaustion) :review-required)) "Extension ceiling latches review")
     (test-assert (= 3 (mission-exhaustion-test-calls provider)) "Third trigger has one stop evaluation"))))

(-> test-mission-exhaustion-concurrency () t)
(defun test-mission-exhaustion-concurrency ()
  "Concurrent admissions share one provider request and retry outside mission locks."
  (mission-exhaustion-test--run
   (lambda (application provider context goal)
     (declare (ignore application))
     (let ((entered      (sb-thread:make-semaphore :count 0))
           (release      (sb-thread:make-semaphore :count 0))
           (second-ready (sb-thread:make-semaphore :count 0)))
       (setf (getf goal :turns-used) (getf goal :turn-limit)
             (mission-exhaustion-test-behavior provider)
             (lambda ()
               (sb-thread:signal-semaphore entered)
               (unless (sb-thread:wait-on-semaphore release :timeout 2)
                 (error "Concurrent evaluator release missing."))))
       (let* ((first  (mission-inference-test--start (lambda () (mission-exhaustion-test--admit context))))
              (ready  (sb-thread:wait-on-semaphore entered :timeout 2))
              (second (mission-inference-test--start
                       (lambda () (sb-thread:signal-semaphore second-ready)
                         (mission-exhaustion-test--admit context)))))
         (test-assert ready "First evaluator entered")
         (test-assert (sb-thread:wait-on-semaphore second-ready :timeout 2) "Second admission began")
         (sb-thread:signal-semaphore release)
         (mission-inference-test--join (list first second) (bordeaux-threads:make-semaphore :count 0) 0)
         (test-assert (null (mission-inference-test-thread-condition first)) "First waiter completed")
         (test-assert (null (mission-inference-test-thread-condition second)) "Second waiter completed")
         (test-assert (= 1 (mission-exhaustion-test-calls provider)) "Concurrent trigger evaluated once")
         (test-assert (= 1 (getf (getf goal :exhaustion) :extensions-used)) "Concurrent extension applied once"))))))

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
                                                                               (lambda () (sb-thread:wait-on-semaphore (sb-thread:make-semaphore :count 0) :timeout 2))
                                                                               (and gate-p (list :timeout 30))) nil)
                                                  (mission-error (condition) (mission-error-reason condition)))))
         (test-assert (eq reason (if gate-p :gate-timeout :operation-timeout)) "Correct runtime boundary")
         (test-assert (eq :active (getf goal :status)) "Operation timeout leaves total-budget policy to admission")))
     (setf (getf goal :turns-used) 20)
     (let ((*mission-exhaustion-evaluator-milliseconds* 30))
       (test-call-with-function-replacements
        (list (list 'redact (lambda (&rest ignored) (declare (ignore ignored))
                             (sb-thread:wait-on-semaphore (sb-thread:make-semaphore :count 0) :timeout 2)
                             (values "" :ok))))
        (lambda () (mission-exhaustion-test--admit context))))
     (test-assert (and (eq :active (getf goal :status)) (getf (getf goal :exhaustion) :review-required)) "Preparation timeout latches review")
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
         (test-assert (and (eq :active (getf goal :status)) (getf state :review-required)) "Interrupted evaluation latches review")
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
       (test-assert (and (eq :active (getf goal :status)) (getf state :review-required)) "Interrupted reviewer restores the spending latch")
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

(-> test-mission-exhausted-resume () null)
(defun test-mission-exhausted-resume ()
  "Explicit recovery appends history without spending or changing accounting."
  (mission-exhaustion-test--run
   (lambda (application provider context goal)
     (setf (getf goal :turns-used) (getf goal :turn-limit))
     (setf (mission-exhaustion-test-response provider)
           (json-encode (mission-exhaustion-test--data :decision "stop")))
     (mission-exhaustion-test--admit context)
     (test-assert (eq :exhausted (getf goal :status)) "the real evaluator stop exhausts the fixture")
     (incf (getf goal :turn-limit) 10)
     (setf (getf goal :tokens-used) 13 (getf goal :tokens-reserved) 17)
     (let* ((keys     '(:turns-used :tokens-used :tokens-reserved :requests-outstanding :unknown-usage
                        :turn-limit :token-limit :deadline :criteria :gates :exhaustion))
            (before   (copy-tree (mapcar (lambda (key) (getf goal key)) keys)))
            (history  (copy-tree (getf goal :evidence)))
            (calls    (mission-exhaustion-test-calls provider))
            (sequence (conversation-next-sequence (application-conversation application))))
       (test-assert (eq :exhausted (getf goal :status)) "raising a limit alone does not resume")
       (test-assert (handler-case (progn (application-mission-command application :resume) t)
                      (mission-error () nil))
                    "the supported explicit command accepts eligible exhausted missions")
       (test-assert (eq :active (getf goal :status)) "explicit resume activates the same mission")
       (test-assert (eq goal (mission-context-goal context)) "mission identity is conserved")
       (test-assert (equal before (mapcar (lambda (key) (getf goal key)) keys))
                    "usage, reservations, pins, limits and evaluator history are conserved")
       (test-assert (and (= (1+ (length history)) (length (getf goal :evidence)))
                         (equal history (butlast (getf goal :evidence))))
                    "resume appends evidence without rewriting history")
       (test-assert (> (conversation-next-sequence (application-conversation application)) sequence)
                    "the state change is durable")
       (test-assert (= calls (mission-exhaustion-test-calls provider))
                    "resume neither replays nor starts a provider request"))))
  nil)

(-> test-mission-exhausted-resume-refusals () null)
(defun test-mission-exhausted-resume-refusals ()
  "Refuse exhausted recovery with spent limits, unsettled work or pending review."
  (dolist (reason '(:wall :turns :tokens :reservation :pending :outstanding :unknown))
    (mission-exhaustion-test--run
     (lambda (application provider context goal)
       (declare (ignore context))
       (setf (getf goal :status) :exhausted)
       (case reason
         (:wall
          (setf (getf goal :deadline) (get-universal-time)))
         (:turns
          (setf (getf goal :turns-used) (getf goal :turn-limit)))
         (:tokens
          (setf (getf goal :tokens-used) (getf goal :token-limit)))
         (:reservation
          (setf (getf goal :tokens-reserved) (getf goal :token-limit)))
         (:pending
          (setf (getf (getf goal :exhaustion) :pending) '(:id 7 :trigger :wall)))
         (:outstanding
          (setf (getf goal :requests-outstanding) 1))
         (:unknown
          (setf (getf goal :unknown-usage) 1)))
       (let ((before (copy-tree goal)))
         (test-assert (handler-case (progn (application-mission-command application :resume) nil)
                        (mission-error () t))
                      "ineligible recovery is refused")
         (test-assert (equal before goal) "refused recovery makes no state or accounting changes")
         (test-assert (zerop (mission-exhaustion-test-calls provider)) "refused recovery never dispatches")))))
  nil)

(-> test-mission-resume-status-controls () null)
(defun test-mission-resume-status-controls ()
  "Keep terminal refusals and paused or blocked explicit recovery."
  (dolist (status '(:verified :failed :cancelled :paused :blocked))
    (mission-exhaustion-test--run
     (lambda (application provider context goal)
       (declare (ignore context))
       (setf (getf goal :status) status)
       (let ((accepted (handler-case (progn (application-mission-command application :resume) t)
                         (mission-error () nil))))
         (test-assert (eq accepted (not (null (member status '(:paused :blocked)))))
                      "only recoverable legacy statuses resume")
         (test-assert (eq (getf goal :status) (if accepted :active status))
                      "terminal statuses are not changed")
         (test-assert (zerop (mission-exhaustion-test-calls provider)) "resume never starts work implicitly")))))
  nil)

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
             :diagnostic (gethash "diagnostic" candidate) :issue-kind :broken-tool :tool "mission.exhaustion")))
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

(-> test-mission-exhaustion-actionable-notice () null)
(defun test-mission-exhaustion-actionable-notice ()
  "Persist and display deadline recovery without relying on evaluator inference."
  (dolist (mode '(:ask :model :admission))
    (mission-exhaustion-test--run
     (lambda (application provider context goal)
       (setf (getf goal :deadline) (1- (get-universal-time)))
       (if (eq mode :admission)
           (test-call-with-function-replacements
            (list (list 'mission-exhaustion--supervise
                        (lambda (function) (declare (ignore function))
                          (error 'provider-context-budget-error :message "Fixture admission refusal"
                                 :estimated-input-tokens 300000 :output-reserve 8192
                                 :context-window 272000 :non-text-estimate-p nil))))
            (lambda () (mission-exhaustion-test--admit context)))
           (mission-exhaustion-test--admit context))
       (test-assert (if (eq mode :ask) (eq :exhausted (getf goal :status))
                            (and (eq :active (getf goal :status)) (getf (getf goal :exhaustion) :review-required)))
                    "ASK exhausts; failed evaluation requires administrative review")
       (test-assert (zerop (mission-exhaustion-test-calls provider)) "notices require no provider call")
       (let* ((conversation (application-conversation application))
              (records      (side-status-tests--records conversation))
              (text         (and records (terminal--spans-text (getf (rest (first records)) :spans))))
              (output       (recording-terminal-output (terminal-ui-terminal (application-ui application)))))
         (test-assert (= 1 (length records)) "the transition persists exactly one side-status notice")
         (test-assert (and text (search "deadline" text :test #'char-equal)
                           (search "limit" text :test #'char-equal)
                           (if (and (eq mode :ask) (fboundp 'mission--validate-exhausted-resume))
                               (search "mission :resume" text :test #'char-equal)
                               (and (search "mission :status" text :test #'char-equal)
                                    (not (search "mission :resume" text :test #'char-equal)))))
                      "the durable notice identifies deadline and supported recovery action")
         (test-assert (and (search "deadline" output :test #'char-equal)
                           (if (and (eq mode :ask) (fboundp 'mission--validate-exhausted-resume))
                               (search "mission :resume" output :test #'char-equal)
                               (and (search "mission :status" output :test #'char-equal)
                                    (not (search "mission :resume" output :test #'char-equal)))))
                      "the recovery action is visible before any inference or tool result")
         (let ((loaded (conversation-load (conversation-pathname conversation))))
           (test-assert (= 1 (length (side-status-tests--records loaded)))
                        "the notice survives reload")))
       (unless (eq mode :ask)
         (let ((record (first (getf (getf goal :exhaustion) :evaluations))))
           (test-assert (and (eq :wall (getf record :trigger))
                             (eq (if (eq mode :model) :evaluator-model :evaluator-error)
                                 (getf record :fallback)))
                        "the real fallback cause and wall trigger are retained"))))
     :policy (if (eq mode :ask) :ask :evaluate) :model-p (not (eq mode :model))))
  nil)


;;;; -- Reviewer Failure Provenance --

(define-condition mission-exhaustion-test-report-error (error)
  ()
  (:documentation "A private fixture condition whose printer also fails.")
  (:report (lambda (condition stream)
             (declare (ignore condition stream))
             (error "Private condition printer must not be called."))))

(-> test-mission-exhaustion-failure-provenance () t)
(defun test-mission-exhaustion-failure-provenance ()
  "A real reviewer failure retains exact cause and live identity before normalization."
  (mission-exhaustion-test--run
   (lambda (application provider context goal)
     (declare (ignore application))
     (let ((cause (make-condition 'simple-error :format-control "private-provider-detail")))
       (setf (mission-exhaustion-test-behavior provider) (lambda () (error cause))
             (getf goal :turns-used) (getf goal :turn-limit))
       (mission-exhaustion-test--admit context)
       (let* ((observation (mission-context-evaluation-observation context))
              (record      (first (getf (getf goal :exhaustion) :evaluations)))
              (diagnostic  (getf record :diagnostic))
              (allowance   (getf diagnostic :allowance-after)))
         (test-assert (eq cause (getf observation :cause)) "Original reviewer condition is retained EQ")
         (test-assert (eq ':provider-entry (getf diagnostic :phase)) "Entry is identified, not claimed network dispatch")
         (test-assert (eq ':failed (getf diagnostic :terminal-state)) "Actual failed reviewer snapshot is retained")
         (test-assert (and (stringp (getf diagnostic :evaluator-job))
                           (stringp (getf diagnostic :run-token))
                           (integerp (getf diagnostic :original-cap))) "Live reviewer identity and original cap survive terminal clearing")
         (test-assert (and (= 1 (getf allowance :unknown-usage))
                           (zerop (getf allowance :requests-outstanding))
                           (zerop (getf allowance :tokens-reserved))) "Entered missing usage settles once as unknown")
         (test-assert (not (search "private-provider-detail" (prin1-to-string diagnostic))) "Portable provenance excludes raw reports")
         (test-assert (and (eq ':active (getf goal :status)) (getf (getf goal :exhaustion) :review-required)) "Failed evaluator latches administrative review without exhausting work"))))))

(-> test-mission-exhaustion-supervisor-cause () t)
(defun test-mission-exhaustion-supervisor-cause ()
  "The actual supervisor boundary retains original EQ cause separately from its wrapper."
  (let* ((cause                            (make-condition 'simple-error :format-control "fixture"))
         (*mission-exhaustion-observation* (list :phase ':provider-entry :cause nil))
         (caught                           (handler-case
                                               (mission-exhaustion--supervise (lambda () (error cause)))
                                             (mission-error (condition) condition))))
    (test-assert (and caught (eq cause (mission-error-cause caught))) "Supervisor wrapper preserves original EQ cause")
    (test-assert (eq cause (getf *mission-exhaustion-observation* :cause)) "Observation is independent of wrapper")
    (test-assert (eq ':failed (getf *mission-exhaustion-observation* :terminal-state)) "Terminal state is captured before normalization")))

(-> test-mission-exhaustion-categorical-diagnostic () t)
(defun test-mission-exhaustion-categorical-diagnostic ()
  "Failure projection cannot invoke a broken printer or redactor."
  (test-call-with-function-replacements
   (list (list 'redact (lambda (&rest arguments) (declare (ignore arguments)) (error "redactor failed"))))
   (lambda ()
     (let* ((cause       (make-condition 'mission-exhaustion-test-report-error))
            (diagnostic  (mission-exhaustion--diagnostic
                          (list :cause cause :phase ':provider-entry :failure-phase ':provider-entry)
                          (list :turns-used 0 :unknown-usage 0)))
            (*read-eval* nil))
       (test-assert (eq ':serious-failure (getf diagnostic :condition-category)) "Categorical fallback needs neither report nor redaction")
       (test-assert (equal diagnostic (read-from-string (prin1-to-string diagnostic))) "Diagnostic is portable readable data")))))

(-> test-mission-exhaustion-reviewer-exits () t)
(defun test-mission-exhaustion-reviewer-exits ()
  "Real reviewer cancellation, deadline, local refusal and broken reports retain provenance."
  (dolist (mode '(:local :cancelled :timeout :printer :settlement))
    (mission-exhaustion-test--run
     (lambda (application provider context goal)
       (declare (ignore application))
       (let* ((cause                                       (make-condition 'mission-exhaustion-test-report-error))
              (gate                                        (sb-thread:make-semaphore :count 0))
              (append-record                               (symbol-function 'conversation-append-record))
              (*mission-exhaustion-evaluator-milliseconds* (if (eq mode :timeout) 30 300000)))
         (setf (getf goal :turns-used) (getf goal :turn-limit)
               (mission-exhaustion-test-behavior provider)
               (lambda ()
                 (ecase mode
                   (:local
                    nil)
                   (:cancelled
                    (job-cancel cl-jobpond::*current-job* :reason :cancelled)
                    (job-check-cancellation cl-jobpond::*current-job*))
                   (:timeout
                    (sb-thread:wait-on-semaphore gate :timeout 2)
                    (error "Deadline monitor failed to interrupt reviewer."))
                   ((:printer :settlement)
                    (error cause)))))
         (when (eq mode :local)
           (let ((allowance (getf (mission-exhaustion--ensure context) :evaluator)))
             (setf (getf allowance :unknown-usage) 1)))
         (test-call-with-function-replacements
          (if (eq mode :settlement)
              (list (list 'conversation-append-record
                          (lambda (conversation record &rest options)
                            (if (eq (first record) :exhaustion-usage)
                                (error "Settlement report fixture failed.")
                                (apply append-record conversation record options)))))
              nil)
          (lambda () (mission-exhaustion-test--admit context)))
         (let* ((observation (mission-context-evaluation-observation context))
                (diagnostic  (getf (first (getf (getf goal :exhaustion) :evaluations)) :diagnostic))
                (allowance   (getf diagnostic :allowance-after)))
           (test-assert (eq (if (eq mode :local) :local-allowance :provider-entry)
                            (getf diagnostic :phase)) "First failure phase survives cleanup")
           (test-assert (eq (case mode (:local
                                        :completed) ((:cancelled :timeout)
                                                     :aborted) (t
                                                                :failed))
                            (getf diagnostic :terminal-state)) "Actual reviewer terminal snapshot is distinct")
           (test-assert (and (= (if (eq mode :local) 0 1) (mission-exhaustion-test-calls provider))
                             (= 1 (getf allowance :unknown-usage))
                             (zerop (getf allowance :requests-outstanding))
                             (zerop (getf allowance :tokens-reserved))) "Local zero versus entered unknown, with one release")
           (when (member mode '(:printer :settlement))
             (test-assert (eq cause (getf observation :cause)) "Original survives printer and settlement failures"))
           (when (eq mode :settlement)
             (test-assert (equal '(:settlement-report-failed) (getf diagnostic :secondary))
                          "Settlement report failure is secondary"))
           (when (member mode '(:cancelled :timeout))
             (test-assert (eq mode (getf diagnostic :terminal-reason)) "Cancellation reason remains truthful"))))))))

(in-package #:autolith)

(define-condition mission-lease-test-decline (error) ()
  (:documentation "An inert provider failure for evaluator lease tests.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "592 inert evaluator provider failure" stream))))

(defstruct mission-lease-test-state
  "Finite barriers and observations shared by the actual child lease fixture."
  (lock (make-lock "592 event trace"))
  (events nil)
  (entered (bordeaux-threads:make-semaphore :count 0))
  (release (bordeaux-threads:make-semaphore :count 0))
  (descendant-entered (bordeaux-threads:make-semaphore :count 0))
  (descendant-release (bordeaux-threads:make-semaphore :count 0))
  (descendant-deferred-p nil)
  (guardian-done (bordeaux-threads:make-semaphore :count 0))
  child
  child-job
  operation
  descendant
  cap
  ancestor-cap
  (deferred-p nil)
  probe-mode
  probe-cause
  probe-receipts
  probe-attempt
  probe-guardian
  probe-before
  probe-artifact
  probe-terminal
  (probe-count 0)
  (probe-publications 0)
  (probe-artifact-calls 0)
  (probe-continuity-calls 0)
  probe-opening
  probe-evaluator-cause-eq-p
  (external-entries 0)
  (regular-entries 0)
  (evaluator-entries 0)
  wave-p
  wave-receipt
  wave-queued-job
  (wave-queued-entries 0)
  (wave-entered (bordeaux-threads:make-semaphore :count 0))
  wave-operation-receipt
  wave-descendant-receipt
  wave-observation
  wave-block-thread
  wave-sibling-thread)(defparameter *mission-lease-test-state* nil
                       "The dynamically bound state of the current lease fixture.")

(-> mission-lease-test-event (keyword &rest t) t)
(defun mission-lease-test-event (kind &rest data)
  "Append a timestamped KIND and DATA to the fixture trace."
  (let ((state *mission-lease-test-state*))
    (with-lock-held ((mission-lease-test-state-lock state))
      (push (list :kind kind :at (get-internal-real-time) :data data)
            (mission-lease-test-state-events state)))))

(-> mission-lease-test-counters (list) list)
(defun mission-lease-test-counters (goal)
  "Capture working usage and pending accounting from GOAL."
  (loop for key in '(:turns-used :tokens-used :tokens-reserved
                     :requests-outstanding :unknown-usage)
        append (list key (getf goal key))))

(-> mission-lease-test-wait (t &optional t) t)
(defun mission-lease-test-wait (semaphore &optional deadline)
  "Wait for a finite fixture barrier without extending DEADLINE."
  (let ((seconds (if deadline
                     (min 10 (/ (max 0 (- deadline (get-internal-real-time)))
                                internal-time-units-per-second))
                     10)))
    (unless (and (plusp seconds)
                 (bt:wait-on-semaphore semaphore :timeout seconds))
      (error "Finite barrier ~A expired; trace ~S; child ~S; descendant ~S" (cond ((eq semaphore (mission-lease-test-state-entered *mission-lease-test-state*))
                                                                                   :controller) ((eq semaphore (mission-lease-test-state-descendant-entered *mission-lease-test-state*))
                                                                                                 :descendant-entry) (t
                                                                                                                     :release)) (mission-lease-test-state-events *mission-lease-test-state*) (mission-lease-test-state-child-job *mission-lease-test-state*) (mission-lease-test-state-descendant *mission-lease-test-state*)))))

(defclass mission-lease-test-child-provider (mission-exhaustion-test-provider) ()
  (:documentation "Script one held tool call before evaluator refusal."))

(defmethod provider-with-configuration ((provider mission-lease-test-child-provider)
                                        (configuration configuration))
  "Use the inert fixture provider with the supplied configuration."
  (declare (ignore configuration))
  provider)

(defmethod provider-stream-turn
           ((provider mission-lease-test-child-provider)
            (conversation conversation)
            &key tool-namespaces event-callback &allow-other-keys)
  "Return inert working calls and delegate evaluator requests to the fixture provider."
  (declare (ignorable event-callback))
  (if (zerop (length tool-namespaces))
      (call-next-method)
      (let* ((state   *mission-lease-test-state*)
             (request (incf (mission-lease-test-state-regular-entries state))))
        (mission-lease-test-event :working-provider-entry :number request
         :conversation                                    (conversation-identifier conversation))
        (agent-test-result (format nil "mission-lease-test-working-~D" request)
         (if (and (= request 1) (mission-lease-test-state-wave-p state))
             (list
              (agent-test-call :call-id "592-hold" :namespace "test" :name
               "block" :arguments "{}")
              (agent-test-call :call-id "625-sibling" :namespace "test" :name
               "wave" :arguments "{}"))
             (list
              (if (= request 1)
                  (agent-test-call :call-id "592-hold" :namespace "test" :name
                   "block" :arguments "{}")
                  (agent-test-call :call-id "592-yield" :namespace "yield"
                   :name "submit" :arguments
                   "{\"status\":\"success\",\"text\":\"unexpected continuation\"}"))))))))

(defclass mission-lease-test-hold-tool (task-test-child-safe-tool) ()
  (:documentation "Hold an actual admitted operation and descendant at finite barriers."))

(defmethod tool-execute ((tool mission-lease-test-hold-tool) (context tool-context) (arguments hash-table))
  "Enter the actual operation and descendant before coordinating evaluator refusal."
  (declare (ignore tool arguments))
  (let* ((state     *mission-lease-test-state*)
         (child     (tool-context-agent context))
         (job       (task-child-agent-job child))
         (operation (execution-lease-receipt-job *execution-lease-receipt*))
         (runtime   (task-job-orchestrator job)))
    (setf (mission-lease-test-state-child state) child
          (mission-lease-test-state-child-job state) job
          (mission-lease-test-state-operation state) operation
          (mission-lease-test-state-cap state) (job-deadline operation)
          (mission-lease-test-state-ancestor-cap state) (job-deadline job)
          (mission-lease-test-state-wave-block-thread state) (current-thread))
    (mission-lease-test-event :operation-entered :child (job-identifier job) :execution
     (task-job-execution-identifier job) :operation (job-identifier operation) :run-token
     (job-run-token operation) :absolute-cap (job-deadline operation) :ancestor-cap (job-deadline job)
     :context-eq (eq (task-job-mission-context job) (mission--agent-context child)))
    (when (mission-lease-test-state-descendant-deferred-p state)
      (let ((pool (task-orchestrator-execution-pool runtime)))
        (job-pool-update-limits pool :maximum-concurrency (job-pool-maximum-concurrency pool)
                                :maximum-batch-size (job-pool-maximum-batch-size pool) :maximum-live-jobs
                                (job-pool-maximum-live-jobs pool) :maximum-runtime-milliseconds 1000)))
    (setf (mission-lease-test-state-descendant state)
          (task-orchestrator-start-execution-job runtime child :tool-name
                                                 "mission-lease-test.inert-descendant" :description
                                                 "Inert session descendant, no subprocess." :summary
                                                 "Wait for release within fixture watchdog." :detached-p t
                                                 :completion-policy :notify :operation-function
                                                 (lambda ()
                                                   (let ((*mission-admitted-operation* job))
                                                     (mission--account-inference
                                                      (task-job-mission-context job)
                                                      (lambda ()
                                                        (mission-lease-test-event :descendant-entered)
                                                        (bordeaux-threads:signal-semaphore
                                                         (mission-lease-test-state-descendant-entered
                                                          state))
                                                        (unwind-protect
                                                            (progn
                                                             (if (mission-lease-test-state-descendant-deferred-p
                                                                  state)
                                                                 (sb-sys:without-interrupts
                                                                   (mission-lease-test-wait
                                                                    (mission-lease-test-state-descendant-release
                                                                     state)))
                                                                 (mission-lease-test-wait
                                                                  (mission-lease-test-state-descendant-release
                                                                   state)))
                                                             (tool-success "released descendant"))
                                                          (mission-lease-test-event :descendant-unwind)))
                                                      (lambda (results)
                                                        (declare (ignore results))
                                                        (json-object "input_tokens" 2 "output_tokens"
                                                                     3)))))))
    (mission-lease-test-wait (mission-lease-test-state-descendant-entered state)
     (mission-lease-test-state-cap state))
    (progn
     (when (mission-lease-test-state-wave-p state)
       (mission-lease-test-wait (mission-lease-test-state-wave-entered state))
       (mission-lease-test-wave-queue state runtime child))
     (bordeaux-threads:signal-semaphore (mission-lease-test-state-entered state)))
    (unwind-protect
        (progn
         (if (mission-lease-test-state-deferred-p state)
             (sb-sys:without-interrupts
               (unless
                   (bordeaux-threads:wait-on-semaphore (mission-lease-test-state-release state) :timeout 10)
                 (error "Finite deferred fixture watchdog expired")))
             (mission-lease-test-wait (mission-lease-test-state-release state)
              (mission-lease-test-state-cap state)))
         (mission-lease-test-event :operation-accepted-result)
         (tool-success "operation released before its original cap"))
      (mission-lease-test-event :operation-unwind))))(-> mission-lease-test-refusal (function) (option condition))
(defun mission-lease-test-refusal (function)
  "Return a typed refusal without hiding an unexpected successful body."
  (handler-case (progn (funcall function) nil)
    (mission-error (condition) condition)))

(define-condition mission-lease-test-publication-fault (error) ()
  (:documentation "An inert pre-side-effect persistence fault in one owned child.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "612 inert publication fault" stream))))

(defparameter *mission-lease-test-probe-outcomes* nil
  "Bounded returned observations from the three finite publication controls.")

(-> mission-lease-test-probe-protect (mission-lease-test-state function) t)
(defun mission-lease-test-probe-protect (state function)
  "Keep exact-child replacements installed through actual fixture completion."
  (unwind-protect
       (funcall function)
    (when (mission-lease-test-state-probe-mode state)
      (bt:signal-semaphore (mission-lease-test-state-release state))
      (bt:signal-semaphore (mission-lease-test-state-descendant-release state))
      (dolist (job (list (mission-lease-test-state-operation state)
                        (mission-lease-test-state-descendant state)
                        (mission-lease-test-state-child-job state)))
        (when job
          (multiple-value-bind (snapshot terminal) (job-await job :timeout-seconds 10)
            (declare (ignore snapshot))
            (test-assert terminal "Owned child or descendant completed before replacement restoration")))))))

(-> mission-lease-test-probe-replacements (mission-lease-test-state) list)
(defun mission-lease-test-probe-replacements (state)
  "Fault only STATE's exact child once, before the selected writer has side effects."
  (when (mission-lease-test-state-probe-mode state)
    (loop for (name category) in '((task-job--publish-terminal-record :publication-fault)
                                   (task--write-result-artifact :artifact-fault)
                                   (task-continuity-record-terminal :continuity-fault))
          collect
          (let ((original (symbol-function name)) (kind category))
            (list name
                  (lambda (job &rest arguments)
                    (when (eq job (mission-lease-test-state-child-job state))
                      (ecase kind
                        (:publication-fault
                         (incf (mission-lease-test-state-probe-publications state)))
                        (:artifact-fault
                         (incf (mission-lease-test-state-probe-artifact-calls state)))
                        (:continuity-fault
                         (incf (mission-lease-test-state-probe-continuity-calls state))))
                      (when (if (eq (mission-lease-test-state-probe-mode state) ':combined-storage-fault)
                                (case kind
                                  (:artifact-fault
                                   (= 1 (mission-lease-test-state-probe-artifact-calls state)))
                                  (:continuity-fault
                                   (= 1 (mission-lease-test-state-probe-continuity-calls state))))
                                (and (eq kind (mission-lease-test-state-probe-mode state))
                                     (zerop (mission-lease-test-state-probe-count state))))
                        (let* ((parent (task-job-parent-agent job))
                               (root   (task--artifact-root (agent-configuration parent) job))
                               (ledger (mission-context-lease-ledger (task-job-mission-context job))))
                          (setf (mission-lease-test-state-probe-artifact state) (merge-pathnames "result.sexp" root)
                                (mission-lease-test-state-probe-terminal state) (merge-pathnames "terminal.sexp" root)
                                (mission-lease-test-state-probe-before state)
                                (list :exact-child t :job (job-identifier job)
                                      :execution        (task-job-execution-identifier job)
                                      :token            (cl-jobpond:job-run-token job)
                                      :reason           (job-cancellation-reason job)
                                      :attempt-eq       (eq (execution-lease-attempt ledger)
                                                            (mission-lease-test-state-probe-attempt state))
                                      :completion-owner (session-job-completion-owner-conversation job)
                                      :artifact-exists  (not (null (probe-file (merge-pathnames "result.sexp" root))))
                                      :terminal-exists  (not (null (probe-file (merge-pathnames "terminal.sexp" root))))))
                         (incf (mission-lease-test-state-probe-count state))
                         (error (mission-lease-test-state-probe-cause state)))))
                    (apply original job arguments)))))))

(-> mission-lease-test-probe-receipt (execution-lease-receipt) list)
(defun mission-lease-test-probe-receipt (receipt)
  "Project identity and immutable original bounds without printing a job graph."
  (let ((source (execution-lease-receipt-revocation-source receipt)))
    (list :job              (job-identifier (execution-lease-receipt-job receipt))
          :execution        (execution-lease-receipt-execution receipt)
          :token            (execution-lease-receipt-token receipt)
          :cap (execution-lease-receipt-cap receipt) :end (execution-lease-receipt-end receipt)
          :parent-ends      (mapcar #'execution-lease-receipt-end (execution-lease-receipt-parents receipt))
          :reason           (execution-lease-receipt-reason receipt)
          :closed-p         (execution-lease-receipt-closed-p receipt)
          :settlements      (execution-lease-receipt-settlements receipt)
          :terminal         (copy-list (execution-lease-receipt-terminal receipt))
          :source-execution (when source (execution-lease-receipt-execution source))
          :source-cap       (when source (execution-lease-receipt-cap source))
          :source-eq-self   (eq receipt source))))

(-> mission-lease-test-store-capture ((option pathname)) list)
(defun mission-lease-test-store-capture (path)
  "Retain bounded fixture-owned text before either durable reader runs."
  (let ((exists (and path (probe-file path))))
    (if exists
        (with-open-file (stream path :element-type '(unsigned-byte 8))
          (let ((size (file-length stream)))
            (list :exists t :bytes size :parse ':not-attempted
                  :capture (if (<= size 1048576) ':complete ':oversized)
                  :raw     (when (<= size 1048576) (uiop:read-file-string path)))))
        (list :exists nil :bytes 0 :capture ':absent :parse ':absent))))

(-> mission-lease-test-store-parse (list function) t)
(defun mission-lease-test-store-parse (capture reader)
  "Record an independent finite parse category without printing a condition."
  (when (getf capture :exists)
    (handler-case
        (let ((value (funcall reader)))
          (setf (getf capture :parse) ':accepted)
          value)
      (error ()
        (setf (getf capture :parse) ':rejected)
        nil))))

(-> mission-lease-test-probe-capture
    (mission-lease-test-state mission-context list &key (:snapshot list) (:terminal t)) null)
(defun mission-lease-test-probe-capture (state context goal &key snapshot terminal)
  "Save branch facts before cleanup, then assert cancellation semantics independently."
  (let* ((job               (mission-lease-test-state-child-job state))
         (operation         (mission-lease-test-state-operation state))
         (descendant        (mission-lease-test-state-descendant state))
         (ledger            (mission-context-lease-ledger context))
         (receipts          (mission-lease-test-state-probe-receipts state))
         (operation-receipt (find operation receipts :key #'execution-lease-receipt-job :test #'eq))
         (child-receipt     (find job receipts :key #'execution-lease-receipt-job :test #'eq))
         (guardian          (mission-lease-test-state-probe-guardian state))
         (before            (mission-lease-test-state-probe-before state))
         (artifact          (mission-lease-test-state-probe-artifact state))
         (metadata          (mission-lease-test-state-probe-terminal state)))
    (bt:signal-semaphore (mission-lease-test-state-descendant-release state))
    (multiple-value-bind (inner inner-terminal) (job-await operation :timeout-seconds 10)
      (multiple-value-bind (dependent dependent-terminal) (job-await descendant :timeout-seconds 10)
        (when (typep guardian 'sb-thread:thread)
          (sb-thread:join-thread guardian :timeout 10 :default :unsettled))
        (let* ((receipt-facts    (mapcar #'mission-lease-test-probe-receipt receipts))
               (artifact-capture (mission-lease-test-store-capture artifact))
               (terminal-capture (mission-lease-test-store-capture metadata))
               (stored           (mission-lease-test-store-parse artifact-capture
                                   (lambda () (task--read-result-artifact artifact))))
               (terminal-record  (mission-lease-test-store-parse terminal-capture
                                   (lambda () (task-continuity--read metadata))))
               (originals-p      (every (lambda (old)
                                           (let ((new (find (getf old :execution) receipt-facts :key (lambda (r) (getf r :execution)) :test #'equal)))
                                             (and new (eql (getf old :token) (getf new :token))
                                                  (eql (getf old :cap) (getf new :cap))
                                                  (eql (getf old :end) (getf new :end)))))
                                        (mission-lease-test-state-probe-opening state)))
             ;; Jobpond clears live run-token/deadline fields at terminal publication.
             ;; Compare the exact retained receipt, not those cleared live fields.
               (bounds-p         (and operation-receipt child-receipt
                                      (eql (mission-lease-test-state-cap state) (execution-lease-receipt-cap operation-receipt))
                                      (eql (mission-lease-test-state-ancestor-cap state) (execution-lease-receipt-cap child-receipt))
                                      (every (lambda (row)
                                               (let ((ends (remove nil (cons (getf row :cap) (getf row :parent-ends)))))
                                                 (eql (getf row :end) (when ends (reduce #'min ends)))))
                                             receipt-facts)))
               (source-p         (and operation-receipt
                                      (every (lambda (r) (eq operation-receipt (execution-lease-receipt-revocation-source r))) receipts)))
               (closed-p         (and (null (execution-lease-guardian ledger))
                                      (null (execution-lease-listeners ledger))
                                      (every (lambda (r) (zerop (execution-lease-receipt-settlements r))) receipts)
                                      (zerop (getf goal :requests-outstanding)))))
         (push (list :mode                                (mission-lease-test-state-probe-mode state)
                     :fault-count                         (mission-lease-test-state-probe-count state)
                     :fault-class                         (type-of (mission-lease-test-state-probe-cause state))
                     :before before :terminal terminal :state (getf snapshot :state)
                     :reason                              (getf snapshot :cancellation-reason)
                     :status                              (getf (getf snapshot :result) :status)
                     :error                               (getf (getf snapshot :result) :error)
                     :result                              (copy-tree (getf snapshot :result))
                     :evaluator-cause-eq-p                (mission-lease-test-state-probe-evaluator-cause-eq-p state)
                     :artifact-record                     stored
                     :terminal-record                     terminal-record
                     :terminal-capture                    terminal-capture
                     :artifact-capture                    artifact-capture
                     :terminal-raw                        (getf terminal-capture :raw)
                     :artifact-raw                        (getf artifact-capture :raw)
                     :condition-report                    (getf snapshot :condition-report)
                     :yielded-p                           (getf (getf snapshot :result) :yielded-p)
                     :trusted-checkpoint-p                (task-review-required-result-p (getf snapshot :result))
                     :inner-state (getf inner :state) :inner-reason (getf inner :cancellation-reason)
                     :inner-terminal                      inner-terminal
                     :descendant-state (getf dependent :state) :descendant-reason (getf dependent :cancellation-reason)
                     :descendant-terminal                 dependent-terminal
                     :operation-cap                       (mission-lease-test-state-cap state)
                     :ancestor-cap                        (mission-lease-test-state-ancestor-cap state)
                     :bounds-unchanged-p bounds-p :receipts receipt-facts
                     :source-original-operation-eq-p      source-p
                     :duplicate-publication-count         (max 0 (1- (mission-lease-test-state-probe-artifact-calls state)))
                     :original-receipt-bounds-unchanged-p originals-p
                     :opening-receipts                    (mission-lease-test-state-probe-opening state)
                     :publication-calls                   (mission-lease-test-state-probe-publications state)
                     :artifact-calls                      (mission-lease-test-state-probe-artifact-calls state)
                     :continuity-calls                    (mission-lease-test-state-probe-continuity-calls state)
                     :artifact-exists (not (null stored)) :artifact-status (getf stored :status)
                     :terminal-metadata-exists            (not (null terminal-record))
                     :terminal-metadata-state             (getf terminal-record :state)
                     :settled-and-controller-closed-p     closed-p
                     :requests-outstanding                (getf goal :requests-outstanding))
               *mission-lease-test-probe-outcomes*)
         (test-assert (= (if (eq (mission-lease-test-state-probe-mode state) ':combined-storage-fault) 2 1)
                         (mission-lease-test-state-probe-count state)) "Each exact owned writer faults once")
         (test-assert (and (getf before :exact-child) (getf before :attempt-eq)
                           (eq (getf before :reason) ':timeout)
                           (equal (getf before :execution) (task-job-execution-identifier job))
                           child-receipt (eq (getf before :token) (execution-lease-receipt-token child-receipt)))
                      "Same child, execution, run token and exact attempt had TIMEOUT before fault")
         (test-assert bounds-p "Original operation and ancestor caps and inherited minimum end are unchanged")
         (test-assert originals-p "Captured exact receipt tokens, caps and ends are unchanged")
         (test-assert source-p "Actual revocation source is EQ to the exact original-cap operation receipt")
         (test-assert (and inner-terminal dependent-terminal
                           (eq (getf inner :state) ':aborted)
                           (eq (getf dependent :state) ':aborted)
                           (eq (getf inner :cancellation-reason) ':timeout)
                           (eq (getf dependent :cancellation-reason) ':timeout))
                      "Actual innermost operation and descendant both terminated with TIMEOUT")
         (test-assert closed-p "Eventual settlement and guardian/listener closure completed")
         (test-assert (and terminal (eq (getf snapshot :state) ':aborted)
                           (eq (getf snapshot :cancellation-reason) ':timeout)
                           (eq (getf (getf snapshot :result) :status) ':aborted)
                           (not (getf (getf snapshot :result) :yielded-p))
                           (not (task-review-required-result-p (getf snapshot :result))))
                      "Accepted cancellation stays physically ABORTED/TIMEOUT with no trusted suspension")
         (unless (eq (mission-lease-test-state-probe-mode state) ':publication-fault)
           (test-assert
            (and (getf (getf snapshot :result) :persistence-failed)
                 (eq (getf (getf snapshot :result) :persistence-failure-phase)
                     (ecase (mission-lease-test-state-probe-mode state)
                       (:artifact-fault
                        ':artifact)
                       (:continuity-fault
                        ':terminal-metadata)
                       (:combined-storage-fault
                        ':artifact-and-terminal-metadata)))
                 (equal (getf snapshot :condition-report)
                        "Mission operation reached its per-operation cap.")
                 (equal (getf (getf snapshot :result) :error)
                        (getf snapshot :condition-report)))
            "Secondary storage phase is separate from the original cap error and report"))
         (test-assert (= 1 (mission-lease-test-state-probe-artifact-calls state))
                      "One artifact publication invocation, with no retry or duplicate write")
         (when (eq (mission-lease-test-state-probe-mode state) ':artifact-fault)
           (test-assert (and (not stored) terminal-record
                             (eq (getf terminal-record :state) ':aborted)
                             (eq (getf (getf terminal-record :result) :status) ':aborted))
                        "Pre-writer artifact fault leaves no artifact and actual ABORTED terminal metadata"))
         (when (eq (mission-lease-test-state-probe-mode state) ':combined-storage-fault)
           (test-assert (and (not stored) (not terminal-record)
                             (= 1 (mission-lease-test-state-probe-continuity-calls state)))
                        "Both pre-writer faults occurred once, without fabricated stores"))
         (when (eq (mission-lease-test-state-probe-mode state) ':continuity-fault)
           (test-assert (and (getf before :completion-owner) (getf before :artifact-exists)
                             (eq (getf stored :status) ':aborted) (not terminal-record))
                        "Continuity branch had a real owner and surviving ABORTED artifact, no terminal write"))
         (when (eq (mission-lease-test-state-probe-mode state) ':publication-fault)
           (test-assert (and (= 2 (mission-lease-test-state-probe-publications state))
                             stored terminal-record
                             (eq (getf stored :status) ':aborted)
                             (eq (getf terminal-record :state) ':aborted)
                             (eq (getf (getf terminal-record :result) :status) ':aborted)
                             (not (getf before :artifact-exists))
                             (not (getf before :terminal-exists))
                             (search "612 inert publication fault" (or (getf snapshot :condition-report) "")))
                        "Actual forced fallback published once after pre-side-effect hook failure"))))))
  nil)

(-> test-mission-lease-artifact-cancellation-fault () null)
(defun test-mission-lease-artifact-cancellation-fault ()
  "Cancellation must survive a one-shot artifact ERROR before writing."
  (mission-lease-test--real-child :artifact-fault))

(-> test-mission-lease-continuity-cancellation-fault () null)
(defun test-mission-lease-continuity-cancellation-fault ()
  "Cancellation must survive continuity ERROR after successful artifact writing."
  (mission-lease-test--real-child :continuity-fault))

(-> test-mission-lease-publication-cancellation-fault () null)
(defun test-mission-lease-publication-cancellation-fault ()
  "The actual forced publication fallback must preserve accepted cancellation."
  (mission-lease-test--real-child :publication-fault))

(-> test-mission-lease-combined-storage-cancellation-fault () null)
(defun test-mission-lease-combined-storage-cancellation-fault ()
  "Preserve accepted cancellation and both independent secondary storage failures."
  (mission-lease-test--real-child :combined-storage-fault))

(-> mission-lease-test--real-child (keyword &optional t t) null)
(defun mission-lease-test--real-child (mode &optional guardian-release guardian-observed)
  "A real child keeps admitted work and pending usage, then publishes review-required."
  (with-test-configuration (base root)
   (let* ((probe-mode
           (when
               (member mode '(:artifact-fault :continuity-fault :publication-fault :combined-storage-fault))
             mode))
          (mode
           (if probe-mode
               :deferred
               mode))
          (original-operation-cap *mission-operation-milliseconds*)
          (*task-default-maximum-runtime-milliseconds*
           (if (eq mode :ancestor-cap)
               1000
               *task-default-maximum-runtime-milliseconds*))
          (configuration
           (configuration-copy base :working-directory root :mission-exhaustion-policy ':evaluate
                               :mission-exhaustion-model (config :model base)))
          (provider               (make-instance 'mission-lease-test-child-provider))
          (application            (mission-inference-test--application configuration provider))
          (registry               (application-tool-registry application))
          (state
           (make-mission-lease-test-state :probe-mode probe-mode :probe-cause
            (when probe-mode (make-condition 'mission-lease-test-publication-fault)) :deferred-p
            (not (null (member mode '(:deferred :early-stop)))) :descendant-deferred-p
            (not (null (member mode '(:descendant-deferred :early-stop)))) :wave-p
            (not (null (member mode '(:wave :ancestor-cap))))))
          (cause                  (make-condition 'mission-lease-test-decline))
          (orchestrator           nil))
     (setf *mission-lease-test-state* state
           (agent-provider (application-agent application)) provider
           (mission-exhaustion-test-behavior provider)
           (lambda () (incf (mission-lease-test-state-evaluator-entries state)) (error cause)))
     (tool-registry-register registry
                             (make-instance 'mission-lease-test-hold-tool :namespace "test" :name "block"
                                            :description "Finite retained operation and descendant."
                                            :parameters  (tool-object-schema (json-object) nil)))
     (progn
      (tool-registry-register registry
                              (make-instance 'mission-lease-test-wave-tool :namespace "test" :name "wave"
                                             :description "Real multi-call wave sibling" :parameters
                                             (tool-object-schema (json-object) nil)))
      (task-augment-tool-registry registry))
     (let* ((run-tool (tool-registry-find registry "child" "spawn"))
            (tool-context
             (make-instance 'tool-context :configuration configuration :worker nil :conversation
                            (application-conversation application) :registry registry :agent
                            (application-agent application) :call-id "lease-child")))
       (setf orchestrator (task-run-tool-orchestrator run-tool))
       (unwind-protect
           (test-call-with-function-replacements
            (append (mission-lease-test-probe-replacements state)
                    (list
                     (list 'redact
                           (lambda (text &key language model-directory runtime-library)
                             (declare (ignore language model-directory runtime-library))
                             (values text ':ok)))))
            (lambda ()
              (mission-lease-test-probe-protect state
               (lambda ()
                 (application-mission-start application (mission-test--specification))
                 (when (member mode '(:operation-cap :deferred :early-stop))
                   (setf *mission-operation-milliseconds* 1000))
                 (let* ((goal    (application-goal application))
                        (context (mission-context-find (application-conversation application))))
                   (let ((result
                          (tool-execute run-tool tool-context
                                        (json-object "agent" "task" "task" "Call test.block." "blocking"
                                                     (json-false)))))
                     (mission-lease-test-event :task-run-return :error (tool-result-error-code result))
                     (test-assert (tool-result-success-p result) "Actual task.run allocated the child"))
                   (mission-lease-test-wait (mission-lease-test-state-entered state))
                   (when (mission-lease-test-state-wave-p state) (mission-lease-test-wave-before state))
                   (when (eq mode :replacement)
                     (let* ((opening (copy-tree goal)) (replacement (mission--attach application)))
                       (test-assert
                        (and (not (eq context replacement)) (eq goal (mission-context-goal replacement)))
                        "Same goal object has a new exact registered context")
                       (test-assert
                        (typep
                         (mission-lease-test-refusal (lambda () (mission-lease-check-admission context)))
                         'mission-error)
                        "Displaced exact context cannot admit work")
                       (multiple-value-bind (snapshot terminal)
                           (job-await (mission-lease-test-state-descendant state) :timeout-seconds 10)
                         (test-assert
                          (and terminal (eq (getf snapshot :state) ':aborted)
                               (eq (getf snapshot :cancellation-reason) ':replaced))
                          "Replacement promptly revokes actual cross-pool work"))
                       (job-await (mission-lease-test-state-child-job state) :timeout-seconds 10)
                       (test-assert (equal opening goal)
                        "Real cancelled old child cannot charge its successor")
                       (let ((original (mission--accounting-goal context)))
                         (test-assert (zerop (getf original :requests-outstanding))
                          "Old child request actually settled")
                         (test-assert (plusp (getf original :reservation-charged-tokens))
                          "Missing old child usage retains its charge")
                         (test-assert
                          (plusp (mission-task-job-unknown-usage (mission-lease-test-state-child-job state)))
                          "Original child UNKNOWN attribution survives replacement")))
                     (return-from mission-lease-test--real-child nil))
                   (when (eq mode ':early-stop)
                     (let* ((child      (mission-lease-test-state-child-job state))
                            (descendant (mission-lease-test-state-descendant state))
                            (ledger     (mission-context-lease-ledger context))
                            (cap        (mission-lease-test-state-cap state)))
                       (test-assert (< (get-internal-real-time) cap)
                        "Genuine STOP precedes the original operation cap")
                       (test-assert (task-job-cancel child ':user)
                        "Actual task cancellation accepts the exact owned STOP")
                       (test-assert (not (job-terminal-p child))
                        "STOP is accepted while actual cancellation is still deferred")
                       (mission-lease-test-controller-report ledger)
                       (bordeaux-threads:signal-semaphore guardian-release)
                       (test-assert (bordeaux-threads:wait-on-semaphore guardian-observed :timeout 10)
                        "Actual guardian observes the unchanged cap after accepted STOP")
                       (with-lock-held ((execution-lease-lock ledger))
                         (let ((receipt
                                (find descendant (execution-lease-receipts ledger) :key
                                      #'execution-lease-receipt-job :test #'eq)))
                           (test-assert (and receipt (eq ':user (execution-lease-receipt-reason receipt)))
                            "Delayed guardian preserves STOP provenance on the real dependent receipt")))
                       (bordeaux-threads:signal-semaphore (mission-lease-test-state-release state))
                       (bordeaux-threads:signal-semaphore
                        (mission-lease-test-state-descendant-release state))
                       (multiple-value-bind (snapshot terminal)
                           (job-await descendant :timeout-seconds 10)
                         (test-assert (and terminal (eq ':user (getf snapshot :cancellation-reason)))
                          "Actual descendant first cancellation reason remains the accepted STOP"))
                       (job-await child :timeout-seconds 10)
                       (test-assert (zerop (getf goal :requests-outstanding))
                        "Stopped original usage releases each reservation once"))
                     (return-from mission-lease-test--real-child nil))
                   (when (member mode '(:ancestor-cap :operation-cap))
                     (test-assert
                      (if (eq mode :ancestor-cap)
                          (< (mission-lease-test-state-ancestor-cap state)
                             (mission-lease-test-state-cap state))
                          (< (mission-lease-test-state-cap state)
                             (mission-lease-test-state-ancestor-cap state)))
                      "The selected original ancestor or operation cap is earlier")
                     (multiple-value-bind (snapshot terminal)
                         (job-await (mission-lease-test-state-descendant state) :timeout-seconds 10)
                       (test-assert
                        (and terminal (eq (getf snapshot :state) ':aborted)
                             (eq (getf snapshot :cancellation-reason) ':timeout))
                        (format nil
                                "Original cap cross-pool result: terminal=~S state=~S reason=~S report=~S"
                                terminal (getf snapshot :state) (getf snapshot :cancellation-reason)
                                (getf snapshot :condition-report))))
                     (multiple-value-bind (snapshot terminal)
                         (job-await (mission-lease-test-state-child-job state) :timeout-seconds 10)
                       (test-assert
                        (and terminal (eq (getf snapshot :state) ':aborted)
                             (not (task-review-required-result-p (getf snapshot :result))))
                        "Expired work has no administrative publication authority"))
                     (test-assert (zerop (mission-lease-test-state-evaluator-entries state))
                      "Guardian enforces original caps before any evaluator latch")
                     (when (eq mode :ancestor-cap) (mission-lease-test-wave-terminal state mode cause))
                     (return-from mission-lease-test--real-child nil))
                   (when (eq mode :deferred)
                     (setf (getf goal :deadline) (1- (get-universal-time)))
                     (let ((refusal
                            (mission-lease-test-refusal
                             (lambda ()
                               (mission-exhaustion--admitting context
                                                              (lambda () (mission--admit context)))))))
                       (test-assert (typep refusal 'mission-review-required)
                        "Actual decline precedes deferred cancellation")
                       (setf (mission-lease-test-state-probe-evaluator-cause-eq-p state)
                             (eq cause (mission-error-cause refusal)))
                       (test-assert (mission-lease-test-state-probe-evaluator-cause-eq-p state)
                        "Deferred original evaluator condition survives EQ"))
                     (when probe-mode
                       (let ((ledger (mission-context-lease-ledger context)))
                         (with-lock-held ((execution-lease-lock ledger))
                           (setf (mission-lease-test-state-probe-receipts state)
                                 (copy-list (execution-lease-receipts ledger))
                                 (mission-lease-test-state-probe-opening state)
                                 (mapcar #'mission-lease-test-probe-receipt
                                         (execution-lease-receipts ledger))
                                 (mission-lease-test-state-probe-attempt state)
                                 (execution-lease-attempt ledger)
                                 (mission-lease-test-state-probe-guardian state)
                                 (execution-lease-guardian ledger)))))
                     (let ((partial
                            (mission-lease-test-controller-report (mission-context-lease-ledger context))))
                       (test-assert
                        (and (eq (getf partial :kind) ':controller-only)
                             (not (getf partial :physical-termination-proven-p))
                             (plusp (getf partial :retained-count))
                             (not (job-terminal-p (mission-lease-test-state-operation state))))
                        "Controller reports retained unresolved work at the original cap"))
                     (bordeaux-threads:signal-semaphore (mission-lease-test-state-release state))
                     (multiple-value-bind (snapshot terminal)
                         (job-await (mission-lease-test-state-child-job state) :timeout-seconds 10)
                       (when probe-mode
                         (mission-lease-test-probe-capture state context goal :snapshot snapshot :terminal
                          terminal))
                       (test-assert
                        (and terminal (eq (getf snapshot :state) ':aborted)
                             (not (task-review-required-result-p (getf snapshot :result))))
                        (if (and terminal (eq (getf snapshot :state) ':aborted)
                                 (not (task-review-required-result-p (getf snapshot :result))))
                            "Expired deferred work is physically aborted"
                            (format nil
                                    "Expired deferred work: terminal=~S state=~S reason=~S status=~S condition=~S error=~S secondary=~S"
                                    terminal (getf snapshot :state) (getf snapshot :cancellation-reason)
                                    (getf (getf snapshot :result) :status) (getf snapshot :condition-report)
                                    (bounded-string (or (getf (getf snapshot :result) :error) "") :limit 512)
                                    (bounded-string
                                     (prin1-to-string
                                      (getf (getf snapshot :result) :review-required-evidence))
                                     :limit 512)))))
                     (return-from mission-lease-test--real-child nil))
                   (let* ((job              (mission-lease-test-state-child-job state))
                          (ledger           (mission-context-lease-ledger context))
                          (orders           (task-orchestrator-next-session-order orchestrator))
                          (before-tokens    (getf goal :tokens-used))
                          (inference-bodies 0)
                          (tool-bodies      0))
                     (test-assert (plusp (getf goal :requests-outstanding))
                      "Usage is genuinely pending before evaluator decline")
                     (test-assert
                      (and (non-empty-string-p (getf goal :completion-id))
                           (non-empty-string-p (execution-lease-generation ledger))
                           (eq context (task-job-mission-context job)))
                      "Durable goal and exact context generation were captured")
                     (setf (getf goal :deadline) (1- (get-universal-time)))
                     (let ((refusal
                            (mission-lease-test-refusal
                             (lambda ()
                               (mission-exhaustion--admitting context
                                                              (lambda () (mission--admit context)))))))
                       (test-assert (typep refusal 'mission-review-required)
                        "Evaluator decline latches review-required, not STOP")
                       (test-assert (eq cause (mission-error-cause refusal))
                        "Original evaluator condition survives EQ"))
                     (test-assert (not (job-terminal-p job))
                      "Refusals are tested while the real child is live")
                     (test-assert
                      (typep
                       (mission-lease-test-refusal
                        (lambda ()
                          (mission--account-inference context (lambda () (incf inference-bodies))
                                                      #'identity)))
                       'mission-review-required)
                      "Next inference is refused before entry")
                     (test-assert
                      (typep
                       (mission-lease-test-refusal
                        (lambda ()
                          (mission--supervise context "Forbidden tool" (lambda () (incf tool-bodies)))))
                       'mission-review-required)
                      "Next tool is refused before entry")
                     (test-assert
                      (typep
                       (mission-lease-test-refusal
                        (lambda ()
                          (task-orchestrator-start-execution-job orchestrator
                                                                 (mission-lease-test-state-child state)
                                                                 :tool-name "forbidden" :summary
                                                                 "Never enter" :operation-function
                                                                 (lambda () (incf tool-bodies)))))
                       'mission-review-required)
                      "Next allocation is refused before order reservation")
                     (test-assert
                      (handler-case
                       (not
                        (tool-result-success-p
                         (tool-execute run-tool tool-context
                                       (json-object "agent" "task" "task" "Forbidden after latch" "blocking"
                                                    (json-false)))))
                       (mission-review-required nil t))
                      "Actual new child is refused before allocation")
                     (test-assert
                      (and (zerop inference-bodies) (zerop tool-bodies)
                           (= orders (task-orchestrator-next-session-order orchestrator)))
                      "No post-latch body or session order was consumed")
                     (test-assert
                      (= (mission-lease-test-state-cap state)
                         (job-deadline (mission-lease-test-state-operation state)))
                      "Original operation cap is unchanged while live")
                     (when (eq mode :descendant-deferred)
                       (bordeaux-threads:signal-semaphore (mission-lease-test-state-release state))
                       (multiple-value-bind (snapshot terminal)
                           (job-await job :timeout-seconds 10)
                         (let* ((result  (getf snapshot :result))
                                (partial (getf result :review-required-evidence)))
                           (test-assert
                            (and terminal (eq (getf snapshot :state) ':aborted)
                                 (eq (getf result :outcome) ':review-required)
                                 (eq (getf result :execution-state) ':unresolved)
                                 (not (task-review-required-result-p result)))
                            "Controller publishes partial evidence before deferred descendant release")
                           (test-assert
                            (and (eq (getf partial :kind) ':controller-only)
                                 (not (getf partial :physical-termination-proven-p))
                                 (plusp (getf partial :retained-count)))
                            "Partial result retains unresolved ownership without termination credit")
                           (test-assert
                            (let* ((entry
                                    (task-continuity--find (application-agent application)
                                                           (session-job-execution-identifier job)))
                                   (directory
                                    (uiop/pathname:pathname-directory-pathname
                                     (pathname (getf entry :path)))))
                              (equal partial
                                     (getf
                                      (task--read-result-artifact (merge-pathnames "result.sexp" directory))
                                      :review-required-evidence)))
                            "The actual durable artifact agrees with the bounded controller report")))
                       (let* ((descendant (mission-lease-test-state-descendant state))
                              (receipt
                               (find descendant (execution-lease-receipts ledger) :key
                                     #'execution-lease-receipt-job :test #'eq)))
                         (test-assert
                          (and receipt (not (job-terminal-p descendant))
                               (plusp (execution-lease-receipt-settlements receipt))
                               (execution-lease-guardian ledger) (execution-lease-listeners ledger))
                          "Deferred execution and pending usage remain owned after outer publication")
                         (bordeaux-threads:signal-semaphore
                          (mission-lease-test-state-descendant-release state))
                         (multiple-value-bind (snapshot terminal)
                             (job-await descendant :timeout-seconds 10)
                           (test-assert
                            (and terminal (eq (getf snapshot :state) ':aborted)
                                 (eq (getf snapshot :cancellation-reason) ':timeout))
                            "The actual late descendant closure retains original TIMEOUT")))
                       (mission-lease-test-wait (mission-lease-test-state-guardian-done state))
                       (test-assert
                        (and (null (execution-lease-guardian ledger))
                             (null (execution-lease-listeners ledger))
                             (null (execution-lease-receipts ledger))
                             (zerop (getf goal :requests-outstanding)) (zerop (getf goal :tokens-reserved))
                             (plusp (getf goal :reservation-charged-tokens))
                             (plusp (mission-task-job-unknown-usage job)))
                        "Late UNKNOWN settles once before guardian and listeners retire")
                       (return-from mission-lease-test--real-child nil))
                     (when (eq mode :wave)
                       (setf (getf (mission-lease-test-state-wave-observation state) :live-latched)
                             (mission-lease-test-wave-live state ':latched-before-release :descendant-p nil)))
                     (bordeaux-threads:signal-semaphore (mission-lease-test-state-descendant-release state))
                     (when (eq mode :wave) (mission-lease-test-wave-terminal state mode cause))
                     (bordeaux-threads:signal-semaphore (mission-lease-test-state-release state))
                     (multiple-value-bind (snapshot terminal)
                         (job-await job :timeout-seconds 10)
                       (test-assert terminal "Real child settled and published")
                       (test-assert (eq (getf snapshot :state) ':aborted) "Physical execution is ABORTED")
                       (test-assert (task-review-required-result-p (getf snapshot :result))
                        "Trusted administrative suspension survives actual publication")
                       (test-assert
                        (if (eq mode :checkpoint-failure)
                            (and (getf (getf snapshot :result) :persistence-failed)
                                 (eq (getf (getf snapshot :result) :persistence-failure-phase) ':checkpoint)
                                 (not (getf (getf snapshot :result) :durable-checkpoint-p)))
                            (getf (getf snapshot :result) :durable-checkpoint-p))
                        "Cursor persistence outcome is reported truthfully"))
                     (multiple-value-bind (snapshot terminal)
                         (job-await (mission-lease-test-state-descendant state) :timeout-seconds 10)
                       (test-assert
                        (and terminal (eq (getf snapshot :state) ':completed)
                             (not (getf snapshot :cancellation-reason)))
                        "Admitted descendant completes without parent-finished cancellation"))
                     (test-assert
                      (and (= (+ before-tokens 5) (getf goal :tokens-used))
                           (zerop (getf goal :requests-outstanding)) (zerop (getf goal :tokens-reserved)))
                      "Pending known usage settled once without clearing uncertainty")
                     (let ((restored (mission--attach application))
                           (bodies   0)
                           (tokens   (getf goal :tokens-used)))
                       (test-assert
                        (typep
                         (mission-lease-test-refusal
                          (lambda ()
                            (mission--account-inference restored (lambda () (incf bodies))
                                                        (constantly nil))))
                         'mission-review-required)
                        "Reattached persisted review-required state refuses spending")
                       (test-assert (and (zerop bodies) (= tokens (getf goal :tokens-used)))
                        "Restoring the latch consumes no body or accounting"))
                     (test-assert (= 1 (mission-lease-test-state-evaluator-entries state))
                      "One evaluator attempt, no retry")))))))
         (bordeaux-threads:signal-semaphore (mission-lease-test-state-descendant-release state))
         (bordeaux-threads:signal-semaphore (mission-lease-test-state-release state))
         (when orchestrator (task-orchestrator-close orchestrator))
         (setf *mission-operation-milliseconds* original-operation-cap)))))
  nil)

(IN-PACKAGE #:AUTOLITH)

(in-package #:autolith)

(defparameter *mission-lease-test-wave-ticket* nil
  "Exact queued ticket observed on its execution-entry thread.")

(defparameter *mission-lease-test-wave-job* nil
  "Actual queued call job, independent of its associated ticket reader.")

(-> mission-lease-test-wave-observe (function) null)
(defun mission-lease-test-wave-observe (function)
  "Observe real constructor and scope calls without altering admission decisions."
  (let* ((call        (symbol-function 'execution-lease-call))
         (construct   (symbol-function 'make-execution-lease-receipt))
         (open        (symbol-function 'execution-lease--open-p))
         (ancestry    (symbol-function 'execution-lease--ancestry-current-p))
         (close       (symbol-function 'task-orchestrator-close))
         (observation (list :rows (make-array 16 :initial-element nil) :next 0
                            :attempts 0 :construct-attempts 0 :constructed 0
                            :entered 0 :condition nil :close-seen nil
                            :preconditions nil :before nil :terminal nil :mode nil :live-before nil :live-latched nil :live-terminal nil)))
    (labels ((state ()
               (let ((state *mission-lease-test-state*))
                 (when (and state (mission-lease-test-state-wave-p state))
                   (setf (mission-lease-test-state-wave-observation state) observation)
                   state)))

             (queued-p (job)
               (let ((state (state)))
                 (and state (eq job (mission-lease-test-state-wave-queued-job state)))))

             (entered (job)
               (loop for row across (getf observation :rows)
                     when (and row (eq job (first row))
                               (eq *execution-lease-receipt* (second row)))
                     do (setf (third row) t))
               (when (queued-p job) (incf (getf observation :entered)))))
      (test-call-with-function-replacements
       (list
        (list 'execution-lease-call
              (lambda (ticket job function)
                (let ((*mission-lease-test-wave-ticket* (when (queued-p job) ticket))
                      (*mission-lease-test-wave-job*    job))
                  (when *mission-lease-test-wave-ticket* (incf (getf observation :attempts)))
                  (handler-bind ((mission-review-required
                                   (lambda (condition)
                                     (when *mission-lease-test-wave-ticket*
                                       (setf (getf observation :condition) condition)))))
                    (funcall call ticket job (lambda () (entered job) (funcall function)))))))
        (list 'make-execution-lease-receipt
              (lambda (&rest arguments &key job ticket &allow-other-keys)
                (let ((queued (queued-p job)))
                  (when queued (incf (getf observation :construct-attempts)))
                  (let ((receipt (apply construct arguments)))
                    (when queued (incf (getf observation :constructed)))
                    (when (and (state) ticket (mission-lease-test-state-child-job (state)))
                      (let ((index (getf observation :next)))
                        (when (< index 16)
                          (setf (aref (getf observation :rows) index) (list job receipt nil)
                                (getf observation :next) (1+ index)))))
                    receipt))))
        (list 'execution-lease--open-p
              (lambda (ledger)
                (let ((result (funcall open ledger))
                      (ticket *mission-lease-test-wave-ticket*))
                  (when ticket
                    (let* ((job     *mission-lease-test-wave-job*)
                           (before  (getf observation :before))
                           (parents (execution-lease-ticket-parents ticket))
                           (now     (get-internal-real-time))
                           (cap     (job-deadline job)))
                      (setf (getf observation :preconditions)
                            (list :open result :attempt (execution-lease-attempt ledger)
                                  :closed     (execution-lease-closed-p ledger)
                                  :current    (execution-lease--current-p ledger)
                                  :associated (eq job (execution-lease-ticket-job ticket))
                                  :reserved   (eq ':reserved (execution-lease-ticket-state ticket))
                                  :capacity   (< (length (execution-lease-receipts ledger)) *execution-lease-receipt-limit*)
                                  :token      (not (null (job-run-token job))) :queued-token-absent (null (getf before :token)) :token-eq-before (eq (job-run-token job) (getf before :token))
                                  :parents    (and parents (equal parents (getf before :parents))
                                                   (every (lambda (parent) (funcall ancestry ledger parent now)) parents)
                                                   (equal (mapcar #'execution-lease-receipt-end parents) (getf before :parent-ends))
                                                   (equal (mapcar #'execution-lease-receipt-cap parents) (getf before :parent-caps)))
                                  :cap        (and (eql cap (getf before :cap)) (or (null cap) (< now cap)))))))
                  result)))
        (list 'task-orchestrator-close
              (lambda (runtime)
                (let ((state (state)))
                  (when (and state (mission-lease-test-state-child-job state)
                             (eq runtime (task-job-orchestrator (mission-lease-test-state-child-job state))))
                    (setf (getf observation :close-seen) t)))
                (funcall close runtime))))
       function))
    nil))

(-> mission-lease-test-wave-live (mission-lease-test-state keyword &key (:descendant-p boolean)) list)
(defun mission-lease-test-wave-live (state boundary &key (descendant-p t))
  "Capture live owned jobs on the main thread before releasing the held operation."
  (let* ((receipts (mission-lease-test-state-probe-receipts state))
         (child    (mission-lease-test-state-child-job state))
         (ledger   (mission-context-lease-ledger (task-job-mission-context child)))
         (jobs     (append (when descendant-p (list (mission-lease-test-state-descendant state)))
                           (list (mission-lease-test-state-operation state) child)))
         (rows     (mapcar (lambda (job)
                             (let ((receipt (find job receipts :key #'execution-lease-receipt-job :test #'eq)))
                               (list :job job :receipt receipt :snapshot (job-snapshot job) :context-current nil :ancestry-current nil
                                   :end             (and receipt (execution-lease-receipt-end receipt))
                                   :cap             (and receipt (execution-lease-receipt-cap receipt))
                                     :token-current (and receipt (eq (job-run-token job) (execution-lease-receipt-token receipt))))))
                           jobs)))
    (with-lock-held ((execution-lease-lock ledger))
      (dolist (row rows)
        (setf (getf row :context-current) (execution-lease--current-p ledger)
              (getf row :ancestry-current)
              (and (getf row :receipt)
                   (execution-lease--ancestry-current-p ledger (getf row :receipt) (get-internal-real-time))))))
    (dolist (row rows)
      (let ((original (find (getf row :job)
                            (getf (mission-lease-test-state-wave-observation state) :live-before)
                            :key (lambda (old) (getf old :job)) :test #'eq)))
        (when original
          (test-assert (and (eq (getf original :receipt) (getf row :receipt))
                            (eql (getf original :end) (getf row :end))
                            (eql (getf original :cap) (getf row :cap)))
                       "Live boundary preserves original receipt identity and absolute ends")))
      (test-assert (and (eq ':running (getf (getf row :snapshot) :state))
                        (null (getf (getf row :snapshot) :cancellation-reason))
                        (getf row :token-current) (getf row :context-current) (getf row :ancestry-current))
                   (format nil "Main-thread ~s live state ~s token ~s context ~s ancestry ~s" boundary
                           (getf (getf row :snapshot) :state) (getf row :token-current)
                           (getf row :context-current) (getf row :ancestry-current))))
    rows))

(-> mission-lease-test-wave-before (mission-lease-test-state) null)
(defun mission-lease-test-wave-before (state)
  "Capture actual queued state and positive descendant interception before the latch."
  (let* ((observation (mission-lease-test-state-wave-observation state))
         (job         (mission-lease-test-state-wave-queued-job state))
         (ticket      (session-job-lease-ticket job))
         (snapshot    (job-snapshot job))
         (descendant  (mission-lease-test-state-descendant state))
         (receipt     (find descendant (mission-lease-test-state-probe-receipts state)
                            :key #'execution-lease-receipt-job :test #'eq))
         (calibration (find-if (lambda (row) (and row (eq descendant (first row))
                                                  (eq receipt (second row)) (third row)))
                               (getf observation :rows))))
    (let ((parents (execution-lease-ticket-parents ticket)))
      (test-assert parents "Queued authority retains nonempty original parent edges")
      (setf (getf observation :before)
            (list :snapshot snapshot :ticket ticket :receipt receipt :calibrated (not (null calibration))
                  :parents (copy-list parents) :parent-ends (mapcar #'execution-lease-receipt-end parents)
                  :parent-caps (mapcar #'execution-lease-receipt-cap parents)
                  :cap (job-deadline job) :token (job-run-token job))))
    (setf (getf observation :live-before) (mission-lease-test-wave-live state ':before-latch))
    (test-assert (and (eq ':queued (getf snapshot :state))
                      (null (getf snapshot :cancellation-reason))
                      (eq ':reserved (execution-lease-ticket-state ticket))
                      (eq job (execution-lease-ticket-job ticket))
                      (zerop (getf observation :attempts))
                      (zerop (getf observation :construct-attempts))
                      (zerop (getf observation :constructed))
                      (zerop (getf observation :entered))
                      (zerop (mission-lease-test-state-wave-queued-entries state)))
                 "Actual queued reservation has no pre-latch entry or cancellation")
    (test-assert calibration "Actual retained descendant calibrates constructor and entered-scope observations"))
  nil)

(-> mission-lease-test-wave-terminal (mission-lease-test-state keyword condition) null)
(defun mission-lease-test-wave-terminal (state mode cause)
  "Join the queued job before releasing its held parent or closing its orchestrator."
  (let* ((observation (mission-lease-test-state-wave-observation state))
         (job         (mission-lease-test-state-wave-queued-job state))
         (ticket      (session-job-lease-ticket job)))
    (multiple-value-bind (snapshot terminal) (job-await job :timeout-seconds 10)
      (setf (getf observation :terminal) (list :snapshot snapshot :terminal terminal
                                             :before-close (not (getf observation :close-seen)))
            (getf observation :mode) mode)
      (when (eq mode :wave)
        (setf (getf observation :live-terminal)
              (mission-lease-test-wave-live state ':queue-terminal :descendant-p nil)))
      (test-assert (and terminal (not (getf observation :close-seen))
                        (member (getf snapshot :state) '(:failed :aborted))
                        (zerop (mission-lease-test-state-wave-queued-entries state)))
                   "Actual queue reaches terminal without body entry before orchestrator close")
      (when (eq mode :wave)
        (let* ((condition     (getf observation :condition))
               (preconditions (getf observation :preconditions))
               (context       (task-job-mission-context (mission-lease-test-state-child-job state)))
               (ledger        (mission-context-lease-ledger context)))
          (test-assert (and (plusp (getf observation :attempts))
                            (zerop (getf observation :construct-attempts))
                            (zerop (getf observation :constructed))
                            (zerop (getf observation :entered))
                            (eq ticket (getf (getf observation :before) :ticket))
                            (eq ':reserved (execution-lease-ticket-state ticket))
                            (null (getf snapshot :cancellation-reason)))
                       "Queued entry is attempted and refused without construction, scope or cancellation")
          (test-assert (and (typep condition 'mission-review-required)
                            (eq context (mission-review-required-context condition))
                            (eq cause (mission-error-cause condition))
                            (equal (execution-lease-generation ledger) (mission-review-required-generation condition))
                            (eql (execution-lease-attempt ledger) (mission-review-required-attempt condition))
                            (eql (execution-lease-attempt ledger) (getf preconditions :attempt))
                            (not (getf preconditions :open)) (not (getf preconditions :closed))
                            (every (lambda (key) (getf preconditions key))
                                   '(:current :associated :reserved :capacity :token :queued-token-absent :parents :cap)))
                       "Original evaluator latch refuses entry with other measured predicates satisfied")))))
  nil)

(in-package #:autolith)

(-> mission-lease-test-lineage-adversaries (function) null)
(defun mission-lease-test-lineage-adversaries (function)
  "Transport exact production checkpoint probes to the main test thread."
  (let* ((capture     (symbol-function 'task-job-capture-review-checkpoint))
         (validate    (symbol-function 'task-job--checkpoint-data))
         (constructed 0)
         (published   0)
         (names       '(:constructor-context :constructor-generation :constructor-attempt
                        :constructor-mission :constructor-parent :constructor-owner
                        :constructor-token :constructor-operation :constructor-positive
                        :publication-positive :publication-parent :publication-owner
                        :publication-token :publication-operation :execution-id :job-id
                        :owners :run-token :mission-id :generation :attempt
                        :publication-restored))
         (rows        (map 'vector (lambda (name) (list :name name :complete nil :result nil :error nil)) names)))
    (labels ((record (index thunk)
               (let ((row (aref rows index)))
                 (handler-bind ((error (lambda (condition) (setf (getf row :error) (type-of condition)))))
                   (setf (getf row :result) (funcall thunk)
                         (getf row :complete) t))))

             (receipts (job)
               (let* ((ledger (mission-context-lease-ledger (task-job-mission-context job)))
                      (outer  (find job (execution-lease-receipts ledger)
                                    :key #'execution-lease-receipt-job :test #'eq)))
                 (find-if (lambda (receipt)
                            (and (eq job (execution-lease-receipt-operation receipt))
                                 (typep (execution-lease-receipt-job receipt) 'execution-lease-inline-job)
                                 (eq outer (first (execution-lease-receipt-parents receipt)))))
                          (execution-lease-receipts ledger))))

             (receipt-negatives (job probe offset)
               (let* ((inner     (receipts job))
                      (parents   (and inner (execution-lease-receipt-parents inner)))
                      (owners    (and inner (execution-lease-receipt-owners inner)))
                      (token     (and inner (execution-lease-receipt-token inner)))
                      (operation (and inner (execution-lease-receipt-operation inner))))
                 (loop for field in '(:parent :owner :token :operation)
                       for index from offset do
                   (record index
                           (lambda ()
                             (and inner parents
                                  (if (and (eq field :token) (execution-lease-receipt-terminal inner))
                                      (and (= offset 10) ':not-applicable)
                                      (let ((refused nil))
                                        (unwind-protect
                                            (progn
                                             (ecase field
                                               (:parent
                                                (setf (execution-lease-receipt-parents inner) nil))
                                               (:owner
                                                (setf (execution-lease-receipt-owners inner) (cons "wrong-owner" owners)))
                                               (:token
                                                (setf (execution-lease-receipt-token inner) "wrong-token"))
                                               (:operation
                                                (setf (execution-lease-receipt-operation inner) nil)))
                                             (setf refused (null (funcall probe))))
                                          (setf (execution-lease-receipt-parents inner) parents
                                                (execution-lease-receipt-owners inner) owners
                                                (execution-lease-receipt-token inner) token
                                                (execution-lease-receipt-operation inner) operation))
                                        (and refused
                                             (eq parents (execution-lease-receipt-parents inner))
                                             (eq owners (execution-lease-receipt-owners inner))
                                             (eq token (execution-lease-receipt-token inner))
                                             (eq operation (execution-lease-receipt-operation inner))))))))))))
      (test-call-with-function-replacements
       (list
        (list 'task-job-capture-review-checkpoint
              (lambda (job &rest arguments &key context generation attempt mission-id &allow-other-keys)
                (if (and (typep context 'mission-context)
                         (eq job (mission-lease-test-state-child-job *mission-lease-test-state*)))
                    (progn
                     (incf constructed)
                     (loop for change in (list (list :context nil)
                                               (list :generation (concatenate 'string generation "-wrong"))
                                               (list :attempt (1+ attempt))
                                               (list :mission-id (concatenate 'string mission-id "-wrong")))
                           for index from 0 do
                       (record index
                               (lambda ()
                                 (let ((wrong (copy-list arguments)))
                                   (setf (getf wrong (first change)) (second change))
                                   (and (null (apply capture job wrong))
                                        (null (task-job-administrative-checkpoint job)))))))
                     (receipt-negatives job (lambda () (apply capture job arguments)) 4)
                     (let ((candidate (apply capture job arguments)))
                       (record 8 (lambda () (and candidate (eq candidate (task-job-administrative-checkpoint job)))))
                       candidate))
                    (apply capture job arguments))))
        (list 'task-job--checkpoint-data
              (lambda (job)
                (let ((candidate (task-job-administrative-checkpoint job)))
                  (when (and candidate (zerop published)
                             (eq job (mission-lease-test-state-child-job *mission-lease-test-state*)))
                    (incf published)
                    (record 9 (lambda () (not (null (funcall validate job)))))
                    (receipt-negatives job (lambda () (funcall validate job)) 10)
                    (let* ((inner  (receipts job))
                           (data   (task-administrative-checkpoint-data candidate))
                           (before (copy-tree data))
                           (spine  (loop for tail on data collect (list tail (first tail) (rest tail)))))
                      (labels ((identities ()
                                 (and (eq candidate (task-job-administrative-checkpoint job))
                                      inner
                                      (eq candidate (execution-lease-receipt-administrative-candidate inner))
                                      (eq data (task-administrative-checkpoint-data candidate))))

                               (restored ()
                                 (and (identities) (equal before data)
                                      (every (lambda (row) (and (eq (second row) (first (first row)))
                                                               (eq (third row) (rest (first row))))) spine))))
                        (loop for change in (list (list :execution-id "wrong-execution")
                                                  (list :job-id "wrong-job")
                                                  (list :owners '("wrong-owner"))
                                                  (list :run-token "wrong-token")
                                                  (list :mission-id "wrong-mission")
                                                  (list :generation "wrong-generation")
                                                  (list :attempt (1+ (getf data :attempt))))
                              for index from 14 do
                          (record index
                                  (lambda ()
                                    (let* ((cell     (loop for tail on data by (lambda (tail) (rest (rest tail)))
                                                           when (eq (first tail) (first change)) return (rest tail)))
                                           (original (and cell (first cell)))
                                           (wrong    (second change))
                                           (positive (and (identities) (not (null (funcall validate job)))))
                                           (schema nil) (refused nil) (associated nil))
                                      (when (equal original wrong)
                                        (setf wrong (if (stringp wrong) "different-wrong-value" '("different-owner"))))
                                      (and cell (not (equal original wrong))
                                           (progn
                                            (unwind-protect
                                                (progn
                                                 (setf (first cell) wrong)
                                                 (setf schema (task-checkpoint-data-p data)
                                                       associated (identities)
                                                       refused (null (funcall validate job))))
                                              (setf (first cell) original))
                                            (list :before positive :schema schema
                                                  :associated associated :refused refused
                                                  :value-restored (eq original (first cell))
                                                  :spine-restored (restored)
                                                  :after          (not (null (funcall validate job))))))))))
                        (record 21 (lambda () (and (restored) (not (null (funcall validate job)))))))))
                  (funcall validate job)))))
       function)
      (test-assert (= 1 constructed) "One production constructor window completed")
      (test-assert (= 1 published) "One production publication window completed")
      (test-assert (equal names (map 'list (lambda (row) (getf row :name)) rows))
                   "Exact finite checkpoint probe membership")
      (loop for row across rows for index from 0
            for result = (getf row :result) do
        (test-assert
         (and (getf row :complete) (null (getf row :error))
              (cond ((= index 12)
                     (eq result ':not-applicable))
                    ((and (<= 14 index 20) (listp result))
                     (and (= 14 (length result))
                          (loop for (key value) on result by (lambda (tail) (rest (rest tail))) always (and (keywordp key) (eq value t)))))
                    (t
                     (not (null result)))))
         (format nil "Callback checkpoint probe ~s completed without error: ~s" (getf row :name) row))))
    nil))

(in-package #:autolith)

(defclass mission-lease-test-wave-tool (task-test-child-safe-tool) ()
  (:documentation "Second genuine agent-wave call with an absent-receipt negative."))

(defmethod tool-execute ((tool mission-lease-test-wave-tool)
                         (context tool-context) (arguments hash-table))
  "Observe real parallel ancestry and reject a new host allocation without its receipt."
  (declare (ignore tool arguments))
  (let* ((state    *mission-lease-test-state*)
         (child    (tool-context-agent context))
         (job      (task-child-agent-job child))
         (runtime  (task-job-orchestrator job))
         (entered  0)
         (reserved 0)
         (thread   (bt:current-thread))
         (reserve  (symbol-function 'task-orchestrator-reserve-session-orders)))
    (setf (mission-lease-test-state-wave-receipt state) *execution-lease-receipt*)
    (test-assert (and *execution-lease-receipt* *mission-admitted-operation*)
                 "Real second tool thread carries exact ambient authority")
    (test-call-with-function-replacements
     (list (list 'task-orchestrator-reserve-session-orders
                 (lambda (&rest arguments)
                   (when (eq thread (bt:current-thread)) (incf reserved))
                   (apply reserve arguments))))
     (lambda ()
       (let ((*execution-lease-receipt* nil))
         (test-assert
          (typep (mission-lease-test-refusal
                  (lambda ()
                    (task-orchestrator-start-execution-job
                     runtime child :tool-name "test.missing-receipt"
                     :summary "Must refuse before reservation" :detached-p t
                     :completion-policy       ':notify
                     :operation-function      (lambda () (incf entered) (tool-success "unexpected")))))
                 'mission-error)
          "Real admitted child route rejects missing host receipt"))))
    (test-assert (and (zerop reserved) (zerop entered))
                 "Absent receipt refuses before order reservation and body")
    (bt:signal-semaphore (mission-lease-test-state-wave-entered state))
    (tool-success "wave sibling observed")))

(-> mission-lease-test-wave-queue (mission-lease-test-state task-orchestrator task-child-agent) null)
(defun mission-lease-test-wave-queue (state runtime child)
  "Queue one actual operation behind the already-entered retained descendant."
  (setf (mission-lease-test-state-probe-receipts state)
        (copy-list
         (execution-lease-receipts
          (mission-context-lease-ledger (task-job-mission-context (task-child-agent-job child))))))
  (let ((pool (task-orchestrator-execution-pool runtime)))
    (job-pool-update-limits pool :maximum-concurrency 1 :maximum-batch-size
                            (job-pool-maximum-batch-size pool) :maximum-live-jobs
                            (job-pool-maximum-live-jobs pool) :maximum-runtime-milliseconds
                            (job-pool-maximum-runtime-milliseconds pool)))
  (setf (mission-lease-test-state-wave-queued-job state)
        (task-orchestrator-start-execution-job runtime child :tool-name "test.queued-before-latch" :summary
                                               "Owned queued-entry negative" :detached-p t
                                               :completion-policy ':notify :operation-function
                                               (lambda ()
                                                 (incf
                                                  (mission-lease-test-state-wave-queued-entries state))
                                                 (tool-success "unexpected queued entry"))))
  (test-assert (zerop (mission-lease-test-state-wave-queued-entries state))
   "Actual reserved queued operation has not entered")
  nil)

(-> mission-lease-test-wave-assertions (mission-lease-test-state) null)
(defun mission-lease-test-wave-assertions (state)
  "Check actual wave edges and the final queued-entry refusal."
  (let* ((receipts (mission-lease-test-state-probe-receipts state))
         (operation
          (find (mission-lease-test-state-operation state) receipts :key #'execution-lease-receipt-job :test
                #'eq))
         (descendant
          (find (mission-lease-test-state-descendant state) receipts :key #'execution-lease-receipt-job :test
                #'eq))
         (sibling  (mission-lease-test-state-wave-receipt state)))
    (test-assert
     (and operation descendant sibling
          (equal (execution-lease-receipt-parents operation) (execution-lease-receipt-parents sibling))
          (first (execution-lease-receipt-parents operation))
          (eq operation (first (execution-lease-receipt-parents descendant))))
     "Actual multi-call siblings and asynchronous descendant retain exact receipt edges")
    (test-assert
     (and (<= (execution-lease-receipt-end descendant) (execution-lease-receipt-end operation))
          (<= (execution-lease-receipt-end operation)
              (execution-lease-receipt-end (first (execution-lease-receipt-parents operation)))))
     "Original absolute ancestor ends bound the innermost actual scope")
    (let* ((observation (mission-lease-test-state-wave-observation state))
           (terminal    (getf observation :terminal)))
      (test-assert (and (getf terminal :terminal) (getf terminal :before-close)
                        (zerop (mission-lease-test-state-wave-queued-entries state)))
                   "Queue evidence was collected before orchestrator close")))
  nil)

(-> test-mission-lease-real-child-decline () null)
(defun test-mission-lease-real-child-decline ()
  "Exercise actual multi-call ancestry and production checkpoint adversaries."
  (mission-lease-test-wave-observe
   (lambda () (mission-lease-test-lineage-adversaries (lambda () (mission-lease-test--real-child :wave)))))
  (mission-lease-test-wave-assertions *mission-lease-test-state*))

(-> test-mission-lease-ancestor-cap () null)
(defun test-mission-lease-ancestor-cap ()
  "Enforce an earlier original task ancestor cap across execution pools."
  (mission-lease-test-wave-observe (lambda () (mission-lease-test--real-child :ancestor-cap)))
  (mission-lease-test-wave-assertions *mission-lease-test-state*))

(-> test-mission-lease-operation-cap () null)
(defun test-mission-lease-operation-cap ()
  "Enforce an earlier original private operation cap before a review latch."
  (mission-lease-test--real-child :operation-cap))

(-> mission-lease-test-guardian-receipt (execution-lease-ledger job) t)
(defun mission-lease-test-guardian-receipt (ledger job)
  "Return the exact retained receipt under its bounded host gate."
  (with-lock-held ((execution-lease-lock ledger))
    (find job (execution-lease-receipts ledger) :key #'execution-lease-receipt-job :test #'eq)))

(-> mission-lease-test-guardian-cleanup nil null)
(defun mission-lease-test-guardian-cleanup ()
  "Hold the actual guardian through protected real-child cleanup."
  (let ((guardian     (symbol-function 'execution-lease--guardian-run))
        (cleanup      (symbol-function 'task-job-cancel-execution-descendants))
        (release      (bordeaux-threads:make-semaphore :count 0))
        (held         t)
        (observed     nil)
        (owned-ledger nil))
    (unwind-protect
        (test-call-with-function-replacements
         (list
          (list 'execution-lease--guardian-run
                (lambda (ledger)
                  (setf owned-ledger ledger)
                  (unless (bordeaux-threads:wait-on-semaphore release :timeout 10)
                    (error "Guardian cleanup barrier expired"))
                  (setf held nil)
                  (funcall guardian ledger)))
          (list 'task-job-cancel-execution-descendants
                (lambda (job reason)
                  (let* ((state *mission-lease-test-state*)
                         (match (and state (eq job (mission-lease-test-state-child-job state)))))
                    (if match
                        (let* ((ledger    (execution-lease-ledger-for (task-job-mission-context job)))
                               (dependent (mission-lease-test-state-descendant state))
                               (receipt   (mission-lease-test-guardian-receipt ledger dependent)))
                          (test-assert (and held (eq owned-ledger ledger))
                           "Guardian held at actual protected cleanup entry")
                          (multiple-value-prog1 (funcall cleanup job reason)
                            (test-assert held
                             "Guardian held through actual protected cleanup return")
                            (setf observed receipt)
                            (bordeaux-threads:signal-semaphore release)))
                        (funcall cleanup job reason))))))
         (lambda () (mission-lease-test--real-child :operation-cap)))
      (bordeaux-threads:signal-semaphore release))
    (test-assert observed "Observed exact real-child cleanup, not only propagated cancellation")
    (test-assert (eq :timeout (execution-lease-receipt-reason observed))
     "Cleanup retains TIMEOUT in the dependent receipt"))
  nil)

(-> mission-lease-test-guardian-stop nil null)
(defun mission-lease-test-guardian-stop ()
  "Preserve accepted STOP across the original cap with a live dependent."
  (let ((guardian (symbol-function 'execution-lease--guardian-run))
        (order    (symbol-function 'execution-lease--cancellation-order))
        (release  (bordeaux-threads:make-semaphore :count 0))
        (observed (bordeaux-threads:make-semaphore :count 0))
        (notified nil))
    (unwind-protect
        (test-call-with-function-replacements
         (list
          (list 'execution-lease--guardian-run
                (lambda (ledger)
                  (unless (bordeaux-threads:wait-on-semaphore release :timeout 10)
                    (error "Earlier STOP barrier expired"))
                  (funcall guardian ledger)))
          (list 'execution-lease--cancellation-order
                (lambda (ledger work)
                  (let* ((state     *mission-lease-test-state*)
                         (child     (and state (mission-lease-test-state-child-job state)))
                         (dependent (and state (mission-lease-test-state-descendant state))))
                    (when
                        (and child dependent (not notified)
                             (eq ledger
                                 (execution-lease-ledger-for (task-job-mission-context child)))
                             (>= (get-internal-real-time) (mission-lease-test-state-cap state)))
                      (setf notified t)
                      (test-assert (not (job-terminal-p dependent))
                       "Dependent retained live after original cap")
                      (test-assert (eq :user (job-cancellation-reason child))
                       "Actual source retains earlier accepted STOP")
                      (bordeaux-threads:signal-semaphore observed)))
                  (funcall order ledger work))))
         (lambda () (mission-lease-test--real-child :early-stop release observed)))
      (bordeaux-threads:signal-semaphore release))
    (test-assert notified "Observed actual delayed guardian after original cap outside host gate"))
  nil)

(-> mission-lease-test-guardian-failure (t) null)
(defun mission-lease-test-guardian-failure (late-p)
  "Compare an actual early failure with a fault during post-cap unwind."
  (mission-exhaustion-test--run
   (lambda (application provider context goal)
     (declare (ignore application provider goal))
     (let* ((guardian                         (symbol-function 'execution-lease--guardian-run))
            (release                          (bordeaux-threads:make-semaphore :count 0))
            (entered                          (bordeaux-threads:make-semaphore :count 0))
            (never                            (bordeaux-threads:make-semaphore :count 0))
            (source                           nil)
            (receipt                          nil)
            (dependent                        nil)
            (dependent-receipt                nil)
            (thread                           nil)
            (guardian-thread                  nil)
            (held                             t)
            (raw                              nil)
            (refusal                          nil)
            (cause                            (make-condition 'mission-lease-test-decline))
            (pool                             nil)
            (ledger                           nil)
            (cap                              nil)
            (*mission-operation-milliseconds* 1000))
       (unwind-protect
           (test-call-with-function-replacements
            (list
             (list 'execution-lease--guardian-run
                   (lambda (candidate)
                     (setf guardian-thread (current-thread))
                     (unless (bordeaux-threads:wait-on-semaphore release :timeout 10)
                       (error "Failure chronology guardian barrier expired"))
                     (setf held nil)
                     (funcall guardian candidate))))
            (lambda ()
              (handler-case
               (mission--supervise context "Guardian chronology source"
                                   (lambda ()
                                     (setf receipt *execution-lease-receipt*
                                           source (execution-lease-receipt-job receipt)
                                           cap (execution-lease-receipt-cap receipt)
                                           ledger (execution-lease-ledger-for context))
                                     (let ((ticket (execution-lease-ticket-create context)))
                                       (setf pool
                                               (make-job-pool :name                         "Guardian chronology dependent"
                                                              :job-class
                                                              'execution-lease-inline-job
                                                              :maximum-concurrency          1
                                                              :maximum-batch-size           1
                                                              :maximum-live-jobs            1
                                                              :maximum-runtime-milliseconds 5000
                                                              :start-threads-p              nil)
                                             dependent
                                               (first
                                                (job-pool-submit-batch pool
                                                                       (list
                                                                        (list :name
                                                                              "Entered dependent"
                                                                              :inline-only-p t
                                                                              :initargs
                                                                              (list :lease-ticket
                                                                                    ticket)
                                                                              :function
                                                                              (lambda (job)
                                                                                (execution-lease-call
                                                                                 ticket job
                                                                                 (lambda ()
                                                                                   (setf dependent-receipt
                                                                                           *execution-lease-receipt*)
                                                                                   (bordeaux-threads:signal-semaphore
                                                                                    entered)
                                                                                   (unless
                                                                                       (bordeaux-threads:wait-on-semaphore
                                                                                        never
                                                                                        :timeout
                                                                                        10)
                                                                                     (error
                                                                                      "Dependent watchdog expired"))))))))))
                                       (setf thread
                                               (make-thread (lambda () (job-run-inline dependent))
                                                            :name
                                                            "Guardian chronology dependent runner")))
                                     (test-assert
                                      (bordeaux-threads:wait-on-semaphore entered :timeout 10)
                                      "Actual dependent entered before source fault")
                                     (test-assert
                                      (eq receipt
                                          (first
                                           (execution-lease-receipt-parents dependent-receipt)))
                                      "Entered dependent retains exact production source edge")
                                     (test-assert
                                      (and (execution-lease-receipt-token receipt)
                                           (execution-lease-receipt-token dependent-receipt))
                                      "Both entered scopes have actual run tokens")
                                     (handler-bind ((serious-condition
                                                     (lambda (condition)
                                                       (when (eq condition cause)
                                                         (setf raw condition)))))
                                       (if late-p
                                           (unwind-protect
                                               (bordeaux-threads:wait-on-semaphore never :timeout
                                                                                   10)
                                             (test-assert (>= (get-internal-real-time) cap)
                                              "Late fault follows unchanged cap")
                                             (error cause))
                                           (error cause)))))
               (serious-condition (condition) (setf refusal condition)))
              (test-assert (eq raw cause) "Original raw source fault observed EQ")
              (test-assert (typep refusal 'mission-error)
               "Source escapes with actual typed refusal")
              (let ((snapshot (job-snapshot source)))
                (test-assert
                 (if late-p
                     (and (eq :aborted (getf snapshot :state))
                          (eq :timeout (getf snapshot :cancellation-reason))
                          (>= (job-ended-at source) cap))
                     (and (eq :failed (getf snapshot :state))
                          (null (getf snapshot :cancellation-reason))
                          (< (job-ended-at source) cap)))
                 "Actual source terminal preserves failure versus accepted timeout")
                (unless late-p
                  (test-assert (eq cause (mission-error-cause refusal))
                   "Early refusal preserves raw cause EQ")))
              (let ((remaining (- cap (get-internal-real-time))))
                (when (plusp remaining)
                  ;; Observe just past the cap despite timed-wait rounding.
                  ;; The job's original deadline is unchanged.
                  (bordeaux-threads:wait-on-semaphore never :timeout
                                                     (+ 0.01 (/ remaining internal-time-units-per-second)))))
              (test-assert (>= (get-internal-real-time) cap)
               "Guardian delayed across immutable cap")
              (test-assert (and held (not (job-terminal-p dependent)))
               "Live dependent retained across cap")
              (bordeaux-threads:signal-semaphore release)
              (multiple-value-bind (snapshot terminal)
                  (job-await dependent :timeout-seconds 10)
                (test-assert
                 (and terminal (eq :aborted (getf snapshot :state))
                      (eq
                       (if late-p
                           :timeout
                           :parent-finished)
                       (getf snapshot :cancellation-reason)))
                 "Actual dependent first cancellation follows source chronology")
                (test-assert
                 (and
                  (eq
                   (if late-p
                       :timeout
                       :parent-finished)
                   (execution-lease-receipt-reason dependent-receipt))
                  (eq receipt (execution-lease-receipt-revocation-source dependent-receipt)))
                 "Host first reason and exact source agree with actual cancellation"))
              (sb-thread:join-thread thread :timeout 10 :default :unsettled)
              (sb-thread:join-thread guardian-thread :timeout 10 :default :unsettled)
              (test-assert
               (and (null (execution-lease-guardian ledger))
                    (null (execution-lease-listeners ledger)))
               "Actual guardian and listeners retire after owned scopes terminate")))
         (bordeaux-threads:signal-semaphore release)
         (bordeaux-threads:signal-semaphore never)
         (when thread (sb-thread:join-thread thread :timeout 10 :default :unsettled))
         (when pool (job-pool-close pool))))))
  nil)

(-> test-mission-lease-delayed-guardian-timeout nil null)
(defun test-mission-lease-delayed-guardian-timeout ()
  "Retain actual cleanup ownership and earlier accepted STOP across a delayed guardian."
  (mission-lease-test-guardian-cleanup)
  (mission-lease-test-guardian-stop)
  nil)

(-> test-mission-lease-failure-chronology nil null)
(defun test-mission-lease-failure-chronology ()
  "Preserve genuine pre-cap failure and the accepted post-cap refusal converse."
  (mission-lease-test-guardian-failure nil)
  (mission-lease-test-guardian-failure t)
  nil)

(-> test-mission-lease-checkpoint-persistence-failure () null)
(defun test-mission-lease-checkpoint-persistence-failure ()
  "Keep administrative refusal truthful when cursor persistence fails."
  (let ((append-record (symbol-function 'conversation-append-record)))
    (test-call-with-function-replacements
     (list (list 'conversation-append-record
                 (lambda (conversation record)
                   (if (eq (first record) ':mission-review-checkpoint)
                       (error "Fixture cursor persistence failure")
                       (funcall append-record conversation record)))))
     (lambda () (mission-lease-test--real-child :checkpoint-failure)))))

(-> mission-lease-test-controller-report (execution-lease-ledger) list)
(defun mission-lease-test-controller-report (ledger)
  "Wait only to an unchanged original end, then read bounded controller evidence."
  (with-lock-held ((execution-lease-lock ledger))
    (let ((end (reduce #'min (remove nil (mapcar #'execution-lease-receipt-end
                                               (execution-lease-receipts ledger))))))
      (loop
        (let ((remaining (- end (get-internal-real-time))))
          (when (<= remaining 0)
            (return (execution-lease--controller-snapshot ledger)))
          (condition-wait (execution-lease-changed ledger) (execution-lease-lock ledger)
                          :timeout (/ remaining internal-time-units-per-second)))))))

(-> test-mission-lease-deferred-controller-report () null)
(defun test-mission-lease-deferred-controller-report ()
  "A deferred interrupt cannot turn original-cap expiry into a claimed stopped worker."
  (mission-lease-test--real-child :deferred))

(-> test-mission-lease-deferred-descendant () null)
(defun test-mission-lease-deferred-descendant ()
  "Publish durable bounded partial evidence while a distinct descendant defers cancellation."
  (let ((guardian (symbol-function 'execution-lease--guardian-run)))
    (test-call-with-function-replacements
     (list (list 'execution-lease--guardian-run
                 (lambda (ledger)
                   (unwind-protect
                        (funcall guardian ledger)
                     (let ((state *mission-lease-test-state*))
                       (when (and state (mission-lease-test-state-child-job state)
                                  (eq ledger (mission-context-lease-ledger
                                              (task-job-mission-context
                                               (mission-lease-test-state-child-job state)))))
                         (bt:signal-semaphore (mission-lease-test-state-guardian-done state))))))))
     (lambda () (mission-lease-test--real-child :descendant-deferred)))))

(-> test-mission-lease-context-replacement () null)
(defun test-mission-lease-context-replacement ()
  "Replacing the exact context revokes retained work even when goal identity is unchanged."
  (mission-lease-test--real-child :replacement))

(-> test-mission-lease-terminal-chronology () null)
(defun test-mission-lease-terminal-chronology ()
  "A delayed cap observer preserves earlier terminal STOP and FAILURE evidence."
  (dolist (entry '((:aborted :stopped :stopped) (:failed nil :parent-finished)))
    (destructuring-bind (state cancellation expected) entry
      (let ((receipt (make-execution-lease-receipt
                      :cap 20 :terminal (list :state state :ended-at 10
                                              :cancellation-reason cancellation))))
        (test-assert (eq expected (execution-lease--source-reason receipt 30))
                     "Earlier terminal cause wins over delayed cap observation")
        (setf (getf (execution-lease-receipt-terminal receipt) :ended-at) 25)
        (test-assert (eq ':timeout (execution-lease--source-reason receipt 30))
                     "A terminal after its original cap cannot erase expiry"))))
  nil)

(-> test-mission-lease-secondary-publication-faults () null)
(defun test-mission-lease-secondary-publication-faults ()
  "Real evaluator refusal survives trace, goal-record and notice publication errors."
  (dolist (fault '(:evaluation-trace :goal-record :terminal-notice))
    (mission-exhaustion-test--run
     (lambda (application provider context goal)
       (declare (ignore application))
       (let ((cause         (make-condition 'mission-lease-test-decline))
             (append-record (symbol-function 'conversation-append-record))
             (record        (symbol-function 'mission--record))
             (notice        (symbol-function 'mission--present-terminal-notice)))
         (setf (getf goal :turns-used) (getf goal :turn-limit)
               (mission-exhaustion-test-behavior provider) (lambda () (error cause)))
         (test-call-with-function-replacements
          (list
           (list 'conversation-append-record
                 (lambda (conversation data)
                   (if (and (eq fault ':evaluation-trace) (eq (first data) ':exhaustion-evaluation))
                       (error "Fixture final evaluation trace fault")
                       (funcall append-record conversation data))))
           (list 'mission--record
                 (lambda (candidate)
                   (if (and (eq fault ':goal-record)
                            (mission-context-lease-ledger candidate)
                            (execution-lease-attempt (mission-context-lease-ledger candidate)))
                       (error "Fixture latched goal record fault")
                       (funcall record candidate))))
           (list 'mission--present-terminal-notice
                 (lambda (&rest arguments)
                   (if (and (eq fault ':terminal-notice) (eq (second arguments) ':review-required))
                       (error "Fixture review notice fault")
                       (apply notice arguments)))))
          (lambda ()
            (test-assert (eq ':review-required (mission-exhaustion-test--admit context))
                         "Secondary publication failure preserves typed refusal")))
         (test-assert (eq cause (getf (mission-context-evaluation-observation context) :cause))
                      "The exact original evaluator condition is preserved")
         (test-assert (member fault (getf (mission-context-evaluation-observation context) :secondary-faults))
                      "Secondary failure retains its category")
         (test-assert (execution-lease-attempt (mission-context-lease-ledger context))
                      "Admission is latched despite secondary I/O failure")))))
  nil)

(-> test-mission-lease-missing-terminal-fence () null)
(defun test-mission-lease-missing-terminal-fence ()
  "Ownership-marker presence fences restart and image recovery without terminal artifacts."
  (dolist (marker '(nil :corrupt (:attempt :corrupt)))
    (task-continuity-tests--fixture
     (lambda (parent orchestrator context job path)
       (declare (ignore orchestrator))
       (let* ((record    (task-continuity--read path))
              (directory (uiop:pathname-directory-pathname path))
              (fresh     (task-orchestrator-create))
              (dispatches 0) (image-lookups 0))
         (unwind-protect
              (progn
                (test-assert (and (not (probe-file (merge-pathnames "result.sexp" directory)))
                                  (not (probe-file (merge-pathnames "terminal.sexp" directory))))
                             "Both terminal projections are absent in the retained-ownership fixture")
                (setf (getf record :restart-safety) "Inert read-only fixture"
                      (getf record :checkpoint) '(:image "inert" :repl "inert")
                      (getf record :administrative-checkpoint) marker)
                (task-continuity--write path record)
                (test-call-with-function-replacements
                 (list (list 'task-continuity-dispatch
                             (lambda (&rest arguments)
                               (declare (ignore arguments)) (incf dispatches)))
                       (list 'task-continuity--checkpoint-image
                             (lambda (&rest arguments)
                               (declare (ignore arguments)) (incf image-lookups) :inert-image)))
                 (lambda ()
                   (test-assert
                    (eq ':dead-needing-decision
                        (getf (task-continuity-classify
                               (task-continuity--find parent (session-job-execution-identifier job))
                               parent fresh) :classification))
                    "Missing projections and damaged marker retain the no-replay fence")
                   (test-assert
                    (task-continuity-tests--refused-p
                     (lambda () (task-continuity-revive context fresh
                                                        (session-job-execution-identifier job)
                                                        :authorized-p t)))
                    "Authorized revive cannot bypass retained suspension ownership")
                   (test-assert (zerop dispatches) "No restart or reconstruction dispatch occurs")
                   (test-assert (zerop image-lookups) "The ownership fence precedes image selection"))))
           (task-orchestrator-close fresh))))))
  nil)


;;;; -- Original Generation Accounting --

(-> test-mission-accounting-pending-cutover () null)
(defun test-mission-accounting-pending-cutover ()
  "Real pending usage callbacks settle in their original generation after same-goal cutover."
  (mission-exhaustion-test--run
   (lambda (application provider context goal)
     (declare (ignore provider))
     (let ((entered      (bt:make-semaphore :count 0))
           (release      (bt:make-semaphore :count 0))
           (cells        nil)
           (opening      nil)
           (conversation (application-conversation application)))
       (unwind-protect
            (progn
              (setf cells
                    (loop for known-p in '(t nil)
                          collect
                          (let ((known-p known-p))
                            (mission-inference-test--start
                             (lambda ()
                               (mission--account-inference
                                context (lambda () :provider-returned)
                                (lambda (results)
                                  (declare (ignore results))
                                  ;; The actual operation returned; its accounting owner
                                  ;; is still live. Cutover cannot discard this callback.
                                  (bt:signal-semaphore entered)
                                  (unless (bt:wait-on-semaphore release :timeout 10)
                                    (error "Pending usage barrier expired"))
                                  (when known-p (json-object "input_tokens" 2 "output_tokens" 3)))))))))
              (dotimes (i 2)
                (declare (ignorable i))
                (test-assert (bt:wait-on-semaphore entered :timeout 10) "Both real callbacks are pending"))
              (test-assert (= 2 (getf goal :requests-outstanding)) "Cutover sees both original holds")
              (let ((successor (mission--attach application)))
                (setf opening (copy-tree goal))
                (mission--record successor)
                (test-assert (eq goal (mission-context-goal context)) "Operational goal identity was not replaced")
                (test-assert (not (equal (mission-context-accounting-generation context)
                                         (mission-context-accounting-generation successor)))
                             "Replacement has a distinct generation")
                (push (mission-inference-test--start
                       (lambda ()
                         (mission--account-inference
                          successor (lambda () :successor-returned)
                          (lambda (results)
                            (declare (ignore results))
                            (bt:signal-semaphore entered)
                            (unless (bt:wait-on-semaphore release :timeout 10)
                              (error "Successor usage barrier expired"))
                            (json-object "input_tokens" 5 "output_tokens" 6))))) cells)
                (test-assert (bt:wait-on-semaphore entered :timeout 10)
                             "Successor has its own real pending request")
                 ;; Only the successor's own callback may change this opening.
                (incf (getf opening :turns-used))
                (incf (getf opening :tokens-used) 11)
                (mission-inference-test--join cells release 3)
                (test-assert (every (lambda (cell) (null (mission-inference-test-thread-condition cell))) cells)
                             "Both real late callbacks settle normally")
                (test-assert (equal opening goal) "Late old known and missing usage leave successor unchanged")
                (let ((original (mission--accounting-goal context)))
                  (test-assert (= 5 (getf original :tokens-used)) "Original identity retains billable known usage")
                  (test-assert (= 1 (getf original :unknown-usage)) "Original missing usage is not zero")
                  (test-assert (zerop (getf original :requests-outstanding)) "Original holds settle once")
                  (test-assert (zerop (getf original :tokens-reserved)) "Original reservation release is complete"))
                (let* ((loaded      (conversation-load (conversation-pathname conversation)))
                       (records     (conversation-records-newest loaded 1000))
                       (audits      (remove-if-not (lambda (record) (eq (first record) ':mission-accounting-settlement)) records))
                       (latest-goal (find ':goal (reverse records) :key #'first)))
                   (test-assert (= 3 (length audits)) "Actual durable bytes retain all three request audits")
                   (test-assert (= 2 (count (mission-context-accounting-generation context) audits
                                            :key  (lambda (record) (getf (rest record) :generation))
                                            :test #'equal))
                                "Both late audits identify the original generation")
                   (test-assert (= 1 (count (mission-context-accounting-generation successor) audits
                                            :key  (lambda (record) (getf (rest record) :generation))
                                            :test #'equal))
                                "The successor audit identifies only its own request")
                  (test-assert (= 2 (getf (rest latest-goal) :requests-outstanding))
                               "No stale goal snapshot follows successor publication")
                  (test-assert (eq ':as-of-cutover (getf (getf (rest latest-goal) :accounting-opening) :kind))
                               "Successor opening holds have explicit old attribution"))))
          (mission-inference-test--join cells release (length cells))))))
  nil)

(-> test-mission-accounting-exact-handles () null)
(defun test-mission-accounting-exact-handles ()
  "Equal amounts are distinct requests; duplicate, wrong-identity and terminal callbacks are safe."
  (dolist (identity '(:same-goal :same-id :distinct-goal :terminal))
    (mission-exhaustion-test--run
     (lambda (application provider context goal)
       (declare (ignore provider))
       (let* ((*provider-maximum-output-tokens* 7)
              (allowance                        (list :turn-limit 100 :turns-used 0 :token-limit 1000 :tokens-used 0))
              (first                            (mission--reserve-inference context :allowance allowance))
              (second                           (mission--reserve-inference context :allowance allowance)))
         (test-assert (= (mission-reservation-amount first) (mission-reservation-amount second))
                      "Distinct equal reservations are captured")
         (test-assert (not (eq first second)) "Amount equality is not request identity")
         (unless (eq identity ':terminal)
           (unless (eq identity ':same-goal)
             (setf (application-goal application) (copy-tree goal))
             (when (eq identity ':distinct-goal)
               (setf (getf (application-goal application) :completion-id) (make-user-identifier))))
           (mission--attach application))
         (when (eq identity ':terminal) (setf (getf goal :status) ':cancelled))
         (let ((opening           (copy-tree (application-goal application)))
               (allowance-opening (copy-tree allowance)))
           (test-assert
            (typep (mission-lease-test-refusal
                    (lambda () (mission--settle-inference context first 5 :allowance (copy-tree allowance))))
                   'mission-error)
            "A copied allowance cannot claim an exact request")
           (test-assert
            (typep (mission-lease-test-refusal
                    (lambda ()
                      (mission--settle-inference
                       (make-instance 'mission-context :application application :goal goal)
                       first 5 :allowance allowance))) 'mission-error)
            "Another context cannot claim the original handle")
           (test-assert
            (typep (mission-lease-test-refusal
                    (lambda ()
                      (mission--settle-inference context first 5 :allowance allowance
                                                 :originating-child         (make-instance 'task-job))))
                   'mission-error)
            "A different host child cannot claim the original handle")
           (test-assert (eq ':pending (mission-inference-request-state first))
                        "Identity refusals preserve the original pending claim")
           (mission--settle-inference context first 19 :allowance allowance)
           (mission--settle-inference context second 0 :allowance allowance)
           (let ((settled (copy-tree (mission--accounting-goal context))))
             (mission--settle-inference context first 99 :allowance allowance)
             (mission--settle-inference context second nil :allowance allowance)
             (test-assert (equal settled (mission--accounting-goal context)) "Repeated callbacks cannot repeat arithmetic")
             (test-assert (= 19 (getf settled :tokens-used)) "Known overrun is charged without clamping")
             (test-assert (zerop (getf settled :requests-outstanding)) "Equal-amount requests both release once"))
           (if (eq identity ':terminal)
               (test-assert (eq ':cancelled (getf goal :status)) "Late current settlement cannot reopen a terminal context")
               (progn
                 (test-assert (equal opening (application-goal application)) "Successor goal and policy are isolated")
                 (test-assert (equal allowance-opening allowance) "Aliased successor allowance does not receive old charges")
                 (test-assert (= 19 (getf (gethash allowance (mission-context-accounting-allowances context)) :tokens-used))
                              "Original allowance route retains billable overrun"))))))))
  (dolist (detached-p '(nil t))
    (dolist (settler (list #'mission--settle-inference #'mission--settle-inference-counters))
      (mission-exhaustion-test--run
       (lambda (application provider context goal)
         (declare (ignore provider goal))
         (let* ((allowance (list :turn-limit 100 :turns-used 0 :token-limit 1000 :tokens-used 0))
                (request   (mission--reserve-inference context :allowance allowance)))
           (when detached-p (mission--attach application))
           (let* ((before  (copy-tree (mission--accounting-goal context)))
                  (route   (copy-tree (gethash allowance (mission-context-accounting-allowances context))))
                  (refusal (mission-lease-test-refusal
                            (lambda () (funcall settler context request -5 :allowance allowance)))))
             (test-assert (and (typep refusal 'mission-error)
                               (eq ':accounting-usage (mission-error-reason refusal)))
                          "Negative usage receives a typed accounting refusal")
             (test-assert (eq ':pending (mission-inference-request-state request)) "Negative usage cannot consume the claim")
             (test-assert (equal before (mission--accounting-goal context)) "Negative usage cannot mutate counters")
             (test-assert (equal route (gethash allowance (mission-context-accounting-allowances context)))
                          "Negative usage cannot mutate the original allowance")
             (test-assert (handler-case
                              (progn (funcall settler context (mission-reservation-amount request) 5
                                              :allowance allowance) nil)
                            (type-error () t)
                            (mission-error () t))
                          "A bare integer cannot replace request identity")
             (funcall settler context request 5 :allowance allowance)
             (funcall settler context request 5 :allowance allowance)
             (test-assert (= 5 (getf (mission--accounting-goal context) :tokens-used))
                          "A valid callback after refusal is charged exactly once")))))))
  nil)

(-> mission-accounting-test--observe (simple-vector function) t)
(defun mission-accounting-test--observe (outcome function)
  "Retain signalled conditions separately from normal return and escaping failure."
  (incf (aref outcome 0))
  (setf (aref outcome 4) (eq sb-thread:*current-thread* (aref outcome 7)))
  (let ((returned-p nil))
    (unwind-protect
         (handler-bind ((serious-condition
                         (lambda (condition)
                           (unless (aref outcome 3) (setf (aref outcome 3) condition))
                           (incf (aref outcome 6)))))
           (let ((values (multiple-value-list (funcall function))))
             (incf (aref outcome 1))
             (setf (aref outcome 2) values
                   returned-p t)
             (values-list values)))
      (unless returned-p (setf (aref outcome 5) t)))))

(-> mission-accounting-test--diagnostic (keyword keyword simple-vector &key (:target condition)) null)
(defun mission-accounting-test--diagnostic (branch operation outcome &key target)
  "Export bounded categorical outcomes, retaining raw objects only in live cells."
  (format t "~&F655 ~s~%"
          (list :branch branch :operation operation :entered (aref outcome 0)
                :completed (aref outcome 1) :main-thread-p (aref outcome 4)
                :normal-return-p (plusp (aref outcome 1)) :escaped-p (aref outcome 5)
                :value-count (length (aref outcome 2)) :signals (aref outcome 6)
                :condition-type (when (aref outcome 3) (type-of (aref outcome 3)))
                :target-eq-p    (eq target (aref outcome 3))))
  nil)

(defparameter *mission-accounting-test-outcomes* nil
  "The two finite branch outcome cells, including live raw conditions, for private diagnosis.")

(-> mission-accounting-test--assert-facts (boolean string list) null)
(defun mission-accounting-test--assert-facts (known-p title facts)
  "Save every named operand before asserting the original conjunction's full membership."
  (format t "~&F655-FACTS ~s~%" (list :branch (if known-p ':known ':unknown) :title title :facts facts))
  (dolist (fact facts)
    (test-assert (second fact) (format nil "~a: ~a (~a)" title (first fact) (if known-p "known" "unknown"))))
  nil)

(-> mission-accounting-test--other-reload (keyword) null)
(defun mission-accounting-test--other-reload (mode)
  "Reopen real committed-append and settlement-policy retries without replaying their application."
  (mission-exhaustion-test--run
   (lambda (application provider context goal)
     (declare (ignore provider))
     (let* ((conversation (application-conversation application))
            (pathname     (conversation-pathname conversation))
            (reserve      (symbol-function 'mission--reserve-inference))
            (writer       (symbol-function 'conversation-append-record))
            (policy       (symbol-function 'mission--settlement-policy))
            (settler      (symbol-function 'mission--settle-inference-counters))
            (cause        (make-condition 'simple-error :format-control "Inert settlement writer cause"))
            (fault        (make-condition 'mission-error :reason ':fixture-writer :cause cause
                                          :message "Inert reached writer fault"))
            (allowance    (list :turn-limit 100 :turns-used 0 :token-limit 1000 :tokens-used 0))
            (outcomes     (loop for name in '(:reserve :body :usage :append :policy :settle :wrapper
                                              :goal-write :record :retry :load :map :map-callback)
                                collect (cons name (vector 0 0 nil nil nil nil 0 sb-thread:*current-thread*))))
            (request nil) (receipt nil) (successor nil) (opening nil)
            (payload nil) (snapshot nil) (native-opening nil) (route nil)
            (physical nil) (attempts 0) (completed-writes 0) (policy-writes 0)
            (armed-p nil) (fired-p nil))
       (labels ((observe (name function)
                  (mission-accounting-test--observe (rest (assoc name outcomes)) function))

                (facts (title values)
                  (format t "~&F657-FACTS ~s~%" (list :branch mode :title title :facts values))
                  (dolist (value values)
                    (test-assert (second value) (format nil "~a: ~a (~a)" title (first value) mode))))

                (cutover ()
                  (setf successor (mission--attach application) opening (copy-tree goal))
                  (observe :record (lambda () (mission--record successor))))

                (records (source)
                  (let ((values nil))
                    (observe :map
                     (lambda ()
                       (conversation-map-records source
                        (lambda (record)
                          (observe :map-callback (lambda () (push (copy-tree record) values)))))))
                    (nreverse values))))
         (push (list :branch mode :target fault :cause cause :outcomes outcomes)
               *mission-accounting-test-outcomes*)
         (test-call-with-function-replacements
          (list
           (list 'mission--reserve-inference
                 (lambda (owner &rest arguments)
                   (observe :reserve
                    (lambda ()
                      (let ((value (apply reserve owner arguments)))
                        (when (eq owner context) (setf request value))
                        value)))))
           (list 'mission--settle-inference-counters
                 (lambda (&rest arguments)
                   (observe :settle (lambda () (apply settler arguments)))))
           (list 'mission--settlement-policy
                 (lambda (&rest arguments)
                   (observe :policy (lambda () (apply policy arguments)))))
           (list 'conversation-append-record
                 (lambda (target record)
                   (cond
                     ((and (eq target conversation) armed-p (eq (first record) ':goal))
                      (observe :goal-write
                       (lambda ()
                         (incf policy-writes)
                         (when (and (eq mode ':policy) (not fired-p))
                           (setf fired-p t)
                           (error fault))
                         (funcall writer target record))))
                     ((and (eq target conversation) (eq (first record) ':mission-accounting-settlement))
                      (observe :append
                       (lambda ()
                         (incf attempts)
                         (let ((result (funcall writer target record)))
                           (incf completed-writes)
                           (push (copy-tree result) physical)
                           (when (and (eq mode ':committed-append) (not fired-p))
                             (setf fired-p t)
                             (error fault))
                           result))))
                     (t
                      (funcall writer target record))))))
          (lambda ()
            (let ((escaped
                   (handler-case
                       (observe :wrapper
                        (lambda ()
                          (mission--account-inference context
                           (lambda ()
                             (observe :body
                              (lambda ()
                                (setf receipt *execution-lease-receipt*)
                                :provider-returned)))
                           (lambda (values)
                             (declare (ignore values))
                             (observe :usage
                              (lambda ()
                                (facts "Original receipt closed with its accounting hold pending"
                                 (list (list "terminal" (not (null (execution-lease-receipt-terminal receipt))))
                                       (list "hold-positive" (plusp (execution-lease-receipt-settlements receipt)))
                                       (list "request-pending" (eq ':pending (mission-inference-request-state request)))))
                                (when (eq mode ':committed-append) (cutover))
                                (setf armed-p t)
                                (json-object "input_tokens" 2 "output_tokens" 3))))
                           :allowance allowance)))
                     (serious-condition (condition) condition))))
              (setf armed-p nil
                    payload (copy-tree (mission-inference-request-payload request))
                    snapshot (copy-tree (mission--accounting-values (mission--accounting-goal context)))
                    native-opening (copy-tree (mission-context-accounting-opening context))
                    route (copy-tree (gethash allowance (mission-context-accounting-allowances context))))
              (dolist (entry outcomes)
                (when (plusp (aref (rest entry) 0))
                  (mission-accounting-test--diagnostic mode (first entry) (rest entry) :target fault)))
              (facts "Actual reached typed writer failure"
               (list (list "fired" fired-p) (list "escaping-target-EQ" (eq fault escaped))
                     (list "typed" (typep escaped 'mission-error))
                     (list "cause-EQ" (eq cause (mission-error-cause escaped)))
                     (list "raw-wrapper-target-EQ" (eq fault (aref (rest (assoc :wrapper outcomes)) 3)))
                     (list "known-charge-once" (= 5 (getf (mission--accounting-goal context) :tokens-used)))
                     (list "pending-released-once" (zerop (getf (mission--accounting-goal context) :requests-outstanding)))
                     (list "reservation-released" (zerop (getf (mission--accounting-goal context) :tokens-reserved)))
                     (list "allocation-positive" (plusp (mission-inference-request-amount request)))
                     (list "allowance-identity" (eq allowance (mission-inference-request-allowance request)))
                     (list "original-context" (eq context (mission-inference-request-context request)))
                     (list "original-conversation" (eq conversation (mission-inference-request-conversation request)))
                     (list "original-goal" (eq goal (mission-context-goal context)))
                     (list "first-snapshot" (equal snapshot (getf (rest payload) :snapshot)))
                     (list "first-opening" (equal native-opening (getf (rest payload) :opening)))))
              (when (eq mode ':policy) (cutover))
              (let* ((generation    (mission-context-accounting-generation context))
                     (controller    (mission-inference-controller-find generation))
                     (report        (mission-inference-request-report controller))
                     (entry         (first (getf report :requests)))
                     (identifier    (getf entry :request-id))
                     (before        (records conversation))
                     (writes-before policy-writes))
                (format t "~&F657-COUNTERS ~s~%"
                        (list :branch mode :attempts attempts :completed-writes completed-writes
                              :policy-writes policy-writes :phase (getf entry :phase)
                              :append-uncertain-p (getf entry :append-uncertain-p)
                              :payload-failure-p  (getf (rest payload) :failure-p)
                              :allocation         (mission-inference-request-amount request)))
                (facts "Product discovery retains the original generation and exact request"
                 (list (list "controller-EQ" (eq context controller))
                       (list "one-retained" (= 1 (getf report :unresolved-count 0)))
                       (list "original-ID" (equal identifier (mission-inference-request-identifier request)))
                       (list "original-generation" (equal generation (getf report :generation)))
                       (list "distinct-successor" (not (equal generation (mission-context-accounting-generation successor))))
                       (list "registry-EQ" (eq request (gethash identifier (mission-context-accounting-requests context))))
                       (list "phase" (eq (getf entry :phase) (if (eq mode ':policy) ':settlement-policy ':audit-append)))
                       (list "prior-uncertainty" (eq (not (null (getf entry :append-uncertain-p)))
                                                     (eq mode ':committed-append)))))
                (multiple-value-bind (complete uncertain)
                    (observe :retry
                     (lambda () (mission-inference-request-retry (mission-inference-controller-find generation) identifier)))
                  (facts "Audit-only retry retains prior uncertainty"
                   (list (list "complete" complete)
                         (list "uncertainty" (eq uncertain (eq mode ':committed-append))))))
                (let* ((reopened        (observe :load (lambda () (conversation-load pathname))))
                       (after           (records reopened))
                       (settlements     (remove-if-not (lambda (record) (eq (first record) ':mission-accounting-settlement)) after))
                       (goal-records    (remove-if-not (lambda (record) (eq (first record) ':goal)) after))
                       (durable-opening (getf (rest (first (last goal-records))) :accounting-opening)))
                  (format t "~&F657-PHYSICAL ~s~%"
                          (list :branch mode :count (length settlements)
                                :metadata (mapcar (lambda (record) (list :seq (getf (rest record) :seq)
                                                                       :time  (getf (rest record) :time))) settlements)))
                  (facts "Independent post-retry reader preserves physical entries and successor"
                   (list (list "new-reader" (not (eq reopened conversation)))
                         (list "same-conversation-ID" (equal (conversation-identifier reopened) (conversation-identifier conversation)))
                         (list "physical-count" (= (if (eq mode ':policy) 1 2) (length settlements)))
                         (list "actual-returned-entries" (equal settlements (nreverse physical)))
                         (list "distinct-sequences" (= (length settlements)
                                                       (length (remove-duplicates settlements :key (lambda (record) (getf (rest record) :seq))))))
                         (list "ordered-sequence-and-time"
                               (loop for (left right) on settlements while right
                                     always (and (< (getf (rest left) :seq) (getf (rest right) :seq))
                                                 (<= (getf (rest left) :time) (getf (rest right) :time)))))
                         (list "full-logical-payload"
                               (every (lambda (record)
                                        (let ((logical (copy-tree record)))
                                          (remf (rest logical) :seq)
                                          (remf (rest logical) :time)
                                          (equal logical payload))) settlements))
                         (list "goal-records-unchanged" (equal goal-records (remove-if-not (lambda (record) (eq (first record) ':goal)) before)))
                         (list "durable-successor-opening" (equal durable-opening (mission-context-accounting-opening successor)))
                         (list "opening-kind" (eq ':as-of-cutover (getf durable-opening :kind)))
                         (list "opening-generation" (equal generation (getf durable-opening :generation)))
                         (list "old-pending" (= (if (eq mode ':policy) 0 1)
                                                (getf (getf durable-opening :old-pending) :requests-outstanding)))))
                  (dolist (record settlements)
                    (dolist (key '(:mission-id :generation :request-id :child-id :classification :known-tokens
                                   :reservation :reservation-charge :failure-p :snapshot :opening))
                      (facts "Decoded physical entry matches immutable first-applied payload"
                       (list (list (symbol-name key) (equal (getf (rest record) key) (getf (rest payload) key))))))))
                (facts "Successful retry releases only reporting ownership"
                 (list (list "payload-immutable" (equal payload (mission-inference-request-payload request)))
                       (list "snapshot-immutable" (equal snapshot (mission--accounting-values (mission--accounting-goal context))))
                       (list "opening-immutable" (equal native-opening (mission-context-accounting-opening context)))
                       (list "route-immutable" (equal route (gethash allowance (mission-context-accounting-allowances context))))
                       (list "successor-immutable" (equal opening goal))
                       (list "no-policy-replay" (= writes-before policy-writes))
                       (list "writes-completed" (= (if (eq mode ':policy) 1 2) completed-writes))
                       (list "persisted" (eq ':persisted (mission-inference-request-state request)))
                       (list "uncertainty-retained" (eq (mission-inference-request-append-uncertain-p request) (eq mode ':committed-append)))
                       (list "request-released" (null (gethash identifier (mission-context-accounting-requests context))))
                       (list "report-released" (null (mission-inference-request-report context)))
                       (list "controller-released" (null (mission-inference-controller-find generation)))))))))
         (dolist (entry outcomes)
           (mission-accounting-test--diagnostic mode (first entry) (rest entry) :target fault))
         (dolist (name '(:reserve :body :usage :settle :record :retry :load))
           (let ((outcome (rest (assoc name outcomes))))
             (facts "Main-thread transport has the exact finite operation outcome"
              (list (list (symbol-name name) (= 1 (aref outcome 0)))
                    (list "completed" (= (if (eq name ':settle) 0 1) (aref outcome 1)))
                    (list "main-thread" (aref outcome 4))
                    (list "raw-serious" (if (eq name ':settle) (eq fault (aref outcome 3)) (null (aref outcome 3))))))))
         (let ((policy-outcome   (rest (assoc :policy outcomes)))
               (append-outcome   (rest (assoc :append outcomes)))
               (goal-outcome     (rest (assoc :goal-write outcomes)))
               (map-outcome      (rest (assoc :map outcomes)))
               (callback-outcome (rest (assoc :map-callback outcomes))))
           (facts "Policy, physical append and complete mapping are not replayed"
            (list (list "policy-entered" (= (if (eq mode ':policy) 1 0) (aref policy-outcome 0)))
                  (list "policy-no-normal-return" (zerop (aref policy-outcome 1)))
                  (list "policy-raw-target" (if (eq mode ':policy) (eq fault (aref policy-outcome 3))
                                              (null (aref policy-outcome 3))))
                  (list "goal-write-attempted" (= (if (eq mode ':policy) 1 0) (aref goal-outcome 0)))
                  (list "goal-write-not-completed" (zerop (aref goal-outcome 1)))
                  (list "goal-raw-target" (if (eq mode ':policy) (eq fault (aref goal-outcome 3))
                                            (null (aref goal-outcome 3))))
                  (list "append-attempts" (= (if (eq mode ':policy) 1 2) attempts (aref append-outcome 0)))
                  (list "append-normal-return" (= 1 (aref append-outcome 1)))
                  (list "append-raw-target" (if (eq mode ':policy) (null (aref append-outcome 3))
                                              (eq fault (aref append-outcome 3))))
                  (list "map-calls" (= 2 (aref map-outcome 0) (aref map-outcome 1)))
                  (list "all-callbacks-completed" (and (plusp (aref callback-outcome 0))
                                                       (= (aref callback-outcome 0) (aref callback-outcome 1))))
                  (list "map-no-signals" (and (null (aref map-outcome 3)) (null (aref callback-outcome 3))))
                  (list "map-main-thread" (and (aref map-outcome 4) (aref callback-outcome 4)))
                  (list "receipt-hold-released" (zerop (execution-lease-receipt-settlements receipt)))
                  (list "original-route-charged-once" (= 5 (getf route :tokens-used)))
                  (list "original-route-released" (zerop (getf route :requests-outstanding))))))
         (facts "Exact callable restoration"
          (list (list "reserve" (eq reserve (symbol-function 'mission--reserve-inference)))
                (list "writer" (eq writer (symbol-function 'conversation-append-record)))
                (list "policy" (eq policy (symbol-function 'mission--settlement-policy)))
                (list "settler" (eq settler (symbol-function 'mission--settle-inference-counters)))))))))
  nil)

(-> test-mission-accounting-append-failure () null)
(defun test-mission-accounting-append-failure ()
  "The real ordinary wrapper hands failed append ownership to its original controller."
  (setf *mission-accounting-test-outcomes* nil)
  (dolist (known-p '(nil t))
    (mission-exhaustion-test--run
     (lambda (application provider context goal)
       (declare (ignore provider))
       (let* ((original-receipt nil)
              (conversation     (application-conversation application))
              (pathname         (conversation-pathname conversation))
              (reserve          (symbol-function 'mission--reserve-inference))
              (writer           (symbol-function 'log-append))
              (fault            (make-condition 'simple-error :format-control "Before-write settlement fault"))
              (write-outcome    (vector 0 0 nil nil nil nil 0 sb-thread:*current-thread*))
              (reserve-outcome  (vector 0 0 nil nil nil nil 0 sb-thread:*current-thread*))
              (body-outcome     (vector 0 0 nil nil nil nil 0 sb-thread:*current-thread*))
              (usage-outcome    (vector 0 0 nil nil nil nil 0 sb-thread:*current-thread*))
              (map-outcome      (vector 0 0 nil nil nil nil 0 sb-thread:*current-thread*))
              (map-call-outcome (vector 0 0 nil nil nil nil 0 sb-thread:*current-thread*))
              (record-outcome   (vector 0 0 nil nil nil nil 0 sb-thread:*current-thread*))
              (retry-outcome    (vector 0 0 nil nil nil nil 0 sb-thread:*current-thread*))
              (load-outcome     (vector 0 0 nil nil nil nil 0 sb-thread:*current-thread*))
              (body-complete-p  nil)
              (native-snapshot  nil)
              (native-opening   nil)
              (request          nil)
              (payload          nil)
              (successor        nil)
              (opening          nil)
              (goal-records     nil)
              (bodies           0)
              (writes           0)
              (armed-p          t))
         (push
          (list :known-p known-p :target fault :outcomes
                (list reserve-outcome body-outcome usage-outcome write-outcome record-outcome retry-outcome
                      load-outcome map-outcome map-call-outcome))
          *mission-accounting-test-outcomes*)
         (test-call-with-function-replacements
          (list
           (list 'mission--reserve-inference
                 (lambda (owner &rest arguments)
                   (mission-accounting-test--observe reserve-outcome
                    (lambda ()
                      (let ((value (apply reserve owner arguments)))
                        (when (eq owner context) (setf request value))
                        value)))))
           (list 'log-append
                 (lambda (target record &rest arguments)
                   (if (and (equal target (conversation-log-pathname conversation))
                            (eq (first record) ':mission-accounting-settlement))
                       (progn
                        (incf writes)
                        (mission-accounting-test--observe write-outcome
                         (lambda ()
                           (when armed-p
                             (setf payload (copy-tree (mission-inference-request-payload request)))
                             (error fault))
                           (apply writer target record arguments))))
                       (apply writer target record arguments)))))
          (lambda ()
            (test-assert
             (typep
              (handler-case
               (mission--account-inference context
                                           (lambda ()
                                             (mission-accounting-test--observe body-outcome
                                              (lambda ()
                                                (incf bodies)
                                                (mission-accounting-test--assert-facts known-p
                                                 "Production reservation retains exact original ownership before cutover"
                                                 (list (list "REQUEST" (not (null request)))
                                                       (list "*EXECUTION-LEASE-RECEIPT*"
                                                             (not (null *execution-lease-receipt*)))
                                                       (list
                                                        "(PLUSP (EXECUTION-LEASE-RECEIPT-SETTLEMENTS *EXECUTION-LEASE-RECEIPT*))"
                                                        (plusp
                                                         (execution-lease-receipt-settlements
                                                          *execution-lease-receipt*)))
                                                       (list
                                                        "(EQ CONTEXT (MISSION-INFERENCE-REQUEST-CONTEXT REQUEST))"
                                                        (eq context
                                                            (mission-inference-request-context request)))
                                                       (list "(EQ GOAL (MISSION-CONTEXT-GOAL CONTEXT))"
                                                             (eq goal (mission-context-goal context)))
                                                       (list
                                                        "(EQ CONVERSATION (MISSION-INFERENCE-REQUEST-CONVERSATION REQUEST))"
                                                        (eq conversation
                                                            (mission-inference-request-conversation
                                                             request)))
                                                       (list
                                                        "(NULL (MISSION-INFERENCE-REQUEST-CHILD REQUEST))"
                                                        (null (mission-inference-request-child request)))
                                                       (list
                                                        "(NULL (MISSION-INFERENCE-REQUEST-ALLOWANCE REQUEST))"
                                                        (null (mission-inference-request-allowance request)))
                                                       (list "(EQ REQUEST
    (GETHASH (MISSION-INFERENCE-REQUEST-IDENTIFIER REQUEST)
             (MISSION-CONTEXT-ACCOUNTING-REQUESTS CONTEXT)))"
                                                             (eq request
                                                                 (gethash
                                                                  (mission-inference-request-identifier
                                                                   request)
                                                                  (mission-context-accounting-requests
                                                                   context))))))
                                                (setf body-complete-p t
                                                      original-receipt *execution-lease-receipt*))))
                                           (lambda (values)
                                             (declare (ignore values))
                                             (mission-accounting-test--observe usage-outcome
                                              (lambda ()
                                                (progn
                                                 (test-assert
                                                  (and original-receipt
                                                       (execution-lease-receipt-terminal original-receipt)
                                                       (plusp
                                                        (execution-lease-receipt-settlements
                                                         original-receipt)))
                                                  "Actual supervised body closes while the original accounting hold survives to cutover")
                                                 (setf successor (mission--attach application)
                                                       opening (copy-tree goal)))
                                                (mission-accounting-test--observe record-outcome
                                                 (lambda () (mission--record successor)))
                                                (when known-p
                                                  (json-object "input_tokens" 2 "output_tokens" 3))))))
               (serious-condition (condition) condition))
              'conversation-invariant-error)
             "The reached leaf writer failure follows the production invariant-condition transport")
            (loop for name in '(:reserve :body :usage :leaf :record)
                  for outcome in (list reserve-outcome body-outcome usage-outcome write-outcome
                                       record-outcome)
                  do (mission-accounting-test--diagnostic
                      (if known-p
                          ':known
                          ':unknown)
                      name outcome :target fault))
            (dolist (outcome (list reserve-outcome body-outcome usage-outcome record-outcome))
              (mission-accounting-test--assert-facts known-p
               "Main thread receives every reservation, provider and usage callback outcome"
               (list (list "(= 1 (AREF OUTCOME 0))" (= 1 (aref outcome 0)))
                     (list "(= 1 (AREF OUTCOME 1))" (= 1 (aref outcome 1)))
                     (list "main-thread-p" (aref outcome 4))
                     (list "no-escaping-failure-p" (null (aref outcome 5)))
                     (list "(NULL (AREF OUTCOME 3))" (null (aref outcome 3))))))
            (mission-accounting-test--assert-facts known-p
             "Main thread observes completed provider body and exact raw leaf writer fault"
             (list (list "BODY-COMPLETE-P" (not (null body-complete-p)))
                   (list "(= 1 (AREF WRITE-OUTCOME 0))" (= 1 (aref write-outcome 0)))
                   (list "(ZEROP (AREF WRITE-OUTCOME 1))" (zerop (aref write-outcome 1)))
                   (list "(EQ FAULT (AREF WRITE-OUTCOME 3))" (eq fault (aref write-outcome 3)))))
            (setf armed-p nil
                  native-snapshot (mission--accounting-values (mission--accounting-goal context))
                  native-opening (copy-tree (mission-context-accounting-opening context)))
            (let* ((generation (mission-context-accounting-generation context))
                   (controller (mission-inference-controller-find generation))
                   (report     (mission-inference-request-report controller))
                   (entry      (first (getf report :requests)))
                   (identifier (getf entry :request-id)))
              (mission-accounting-test--assert-facts known-p
               "Product discovery preserves the original request and immutable applied payload"
               (list (list "(EQ CONTROLLER CONTEXT)" (eq controller context))
                     (list "(= 1 WRITES)" (= 1 writes))
                     (list "(= 1 (GETF REPORT :UNRESOLVED-COUNT 0))" (= 1 (getf report :unresolved-count 0)))
                     (list "(EQUAL IDENTIFIER (MISSION-INFERENCE-REQUEST-IDENTIFIER REQUEST))"
                           (equal identifier (mission-inference-request-identifier request)))
                     (list "(EQUAL GENERATION (GETF REPORT :GENERATION))"
                           (equal generation (getf report :generation)))
                     (list "(NOT (EQUAL GENERATION (MISSION-CONTEXT-ACCOUNTING-GENERATION SUCCESSOR)))"
                           (not
                            (null
                             (not (equal generation (mission-context-accounting-generation successor))))))
                     (list "(EQ ':UNPERSISTED (MISSION-INFERENCE-REQUEST-STATE REQUEST))"
                           (eq ':unpersisted (mission-inference-request-state request)))
                     (list "(GETF ENTRY :APPEND-UNCERTAIN-P)" (not (null (getf entry :append-uncertain-p))))
                     (list "(EQ REQUEST (GETHASH IDENTIFIER (MISSION-CONTEXT-ACCOUNTING-REQUESTS CONTEXT)))"
                           (eq request (gethash identifier (mission-context-accounting-requests context))))
                     (list "(EQ GOAL (MISSION-CONTEXT-GOAL CONTEXT))"
                           (eq goal (mission-context-goal context)))
                     (list "(EQ CONVERSATION (MISSION-INFERENCE-REQUEST-CONVERSATION REQUEST))"
                           (eq conversation (mission-inference-request-conversation request)))
                     (list "(EQUAL PAYLOAD (MISSION-INFERENCE-REQUEST-PAYLOAD REQUEST))"
                           (equal payload (mission-inference-request-payload request)))))
              (mission-accounting-test--observe
               map-call-outcome
               (lambda ()
                 (conversation-map-records conversation
                                           (lambda (record)
                                             (mission-accounting-test--observe map-outcome
                                              (lambda ()
                                                (when (eq (first record) ':goal)
                                                  (push (copy-tree record) goal-records))))))))
              (multiple-value-bind (complete uncertain)
                  (mission-accounting-test--observe retry-outcome
                   (lambda ()
                     (mission-inference-request-retry (mission-inference-controller-find generation)
                      identifier)))
                (mission-accounting-test--assert-facts known-p "Retry retains prior append uncertainty"
                 (list (list "COMPLETE" (not (null complete))) (list "UNCERTAIN" (not (null uncertain))))))
              (let ((reopened
                     (mission-accounting-test--observe load-outcome
                      (lambda () (conversation-load pathname))))
                    (settlements    nil)
                    (reloaded-goals nil))
                (mission-accounting-test--assert-facts known-p
                 "A new conversation reader reopens the original durable identity"
                 (list
                  (list "(NOT (EQ REOPENED CONVERSATION))" (not (null (not (eq reopened conversation)))))
                  (list "(EQUAL (CONVERSATION-IDENTIFIER REOPENED)
       (CONVERSATION-IDENTIFIER CONVERSATION))"
                        (equal (conversation-identifier reopened) (conversation-identifier conversation)))))
                (mission-accounting-test--observe
                 map-call-outcome
                 (lambda ()
                   (conversation-map-records reopened
                                             (lambda (record)
                                               (mission-accounting-test--observe map-outcome
                                                (lambda ()
                                                  (case (first record)
                                                    (:mission-accounting-settlement
                                                     (push record settlements))
                                                    (:goal
                                                     (push record reloaded-goals)))))))))
                (let* ((record (first settlements)) (fields (rest record)))
                  (test-assert (= 1 (length settlements))
                   "Before-write failure produces one actual settlement entry")
                  (dolist
                      (key
                       '(:mission-id :generation :request-id :child-id :classification :known-tokens
                         :reservation :reservation-charge :failure-p :snapshot :opening))
                    (test-assert (equal (getf fields key) (getf (rest payload) key))
                     "Every persisted settlement field matches the first applied payload"))
                  (format t "~&F655-FIELDS ~s~%"
                          (list :branch
                                (if known-p
                                    ':known
                                    ':unknown)
                                :identifier-p (equal identifier (getf fields :request-id)) :generation-p
                                (equal generation (getf fields :generation)) :classification
                                (getf fields :classification) :expected-known-p known-p :known-tokens
                                (getf fields :known-tokens) :mission-p
                                (equal (mission-inference-request-mission-id request)
                                       (getf fields :mission-id))
                                :allocation (mission-inference-request-amount request) :reservation
                                (getf fields :reservation) :snapshot-p
                                (equal native-snapshot (getf fields :snapshot)) :opening-p
                                (equal native-opening (getf fields :opening)) :charge
                                (getf fields :reservation-charge)))
                  (mission-accounting-test--assert-facts known-p
                   "Disk settlement belongs to the discovered original request"
                   (list
                    (list "(EQUAL IDENTIFIER (GETF FIELDS :REQUEST-ID))"
                          (equal identifier (getf fields :request-id)))
                    (list "(EQUAL GENERATION (GETF FIELDS :GENERATION))"
                          (equal generation (getf fields :generation)))
                    (list "(EQ
 (IF KNOWN-P
     ':KNOWN
     ':UNKNOWN)
 (GETF FIELDS :CLASSIFICATION))"
                          (eq
                           (if known-p
                               ':known
                               ':unknown)
                           (getf fields :classification)))
                    (list "(EQL
 (IF KNOWN-P
     5
     NIL)
 (GETF FIELDS :KNOWN-TOKENS))"
                          (eql
                           (if known-p
                               5
                               nil)
                           (getf fields :known-tokens)))
                    (list "(EQUAL (MISSION-INFERENCE-REQUEST-MISSION-ID REQUEST) (GETF FIELDS :MISSION-ID))"
                          (equal (mission-inference-request-mission-id request) (getf fields :mission-id)))
                    (list "(= (MISSION-INFERENCE-REQUEST-AMOUNT REQUEST) (GETF FIELDS :RESERVATION))"
                          (= (mission-inference-request-amount request) (getf fields :reservation)))
                    (list "(EQUAL NATIVE-SNAPSHOT (GETF FIELDS :SNAPSHOT))"
                          (equal native-snapshot (getf fields :snapshot)))
                    (list "(EQUAL NATIVE-OPENING (GETF FIELDS :OPENING))"
                          (equal native-opening (getf fields :opening)))
                    (list "(= 0 (GETF FIELDS :RESERVATION-CHARGE))"
                          (= 0 (getf fields :reservation-charge))))))
                (let ((durable-opening (getf (rest (first reloaded-goals)) :accounting-opening)))
                  (mission-accounting-test--assert-facts known-p
                   "An actual successor publication retains the original pending hold on disk"
                   (list
                    (list "(EQ ':AS-OF-CUTOVER (GETF DURABLE-OPENING :KIND))"
                          (eq ':as-of-cutover (getf durable-opening :kind)))
                    (list "(EQUAL GENERATION (GETF DURABLE-OPENING :GENERATION))"
                          (equal generation (getf durable-opening :generation)))
                    (list "(EQUAL DURABLE-OPENING (MISSION-CONTEXT-ACCOUNTING-OPENING SUCCESSOR))"
                          (equal durable-opening (mission-context-accounting-opening successor)))
                    (list "(= 1 (GETF (GETF DURABLE-OPENING :OLD-PENDING) :REQUESTS-OUTSTANDING))"
                          (= 1 (getf (getf durable-opening :old-pending) :requests-outstanding))))))
                (test-assert (equal goal-records reloaded-goals)
                 "Retry does not change successor durable goal records"))
              (loop for name in '(:retry :load :map-callback :map :leaf)
                    for outcome in (list retry-outcome load-outcome map-outcome map-call-outcome write-outcome)
                    do (mission-accounting-test--diagnostic
                        (if known-p
                            ':known
                            ':unknown)
                        name outcome :target fault))
              (dolist (outcome (list retry-outcome load-outcome map-call-outcome))
                (mission-accounting-test--assert-facts
                 known-p "Delegated retry, reopen and complete map outcomes"
                 (list (list "entered-count" (= (if (eq outcome map-call-outcome) 2 1) (aref outcome 0)))
                       (list "completed-count" (= (aref outcome 0) (aref outcome 1)))
                       (list "main-thread-p" (aref outcome 4))
                       (list "no-signalled-condition-p" (null (aref outcome 3)))
                       (list "no-escaping-failure-p" (null (aref outcome 5))))))
              (format t "~&F655-FINAL ~s~%"
                      (list :branch
                            (if known-p
                                ':known
                                ':unknown)
                            :writes writes :bodies bodies :snapshot-p
                            (equal native-snapshot
                                   (mission--accounting-values (mission--accounting-goal context)))
                            :opening-p             (equal native-opening (mission-context-accounting-opening context))
                            :tokens (getf (mission--accounting-goal context) :tokens-used) :successor-equal-p
                            (equal opening goal) :original-goal-eq-p (eq goal (mission-context-goal context))
                            :conversation-eq-p
                            (eq conversation (mission-inference-request-conversation request)) :state
                            (mission-inference-request-state request) :payload-p
                            (equal payload (mission-inference-request-payload request)) :request-released-p
                            (null (gethash identifier (mission-context-accounting-requests context)))
                            :report-released-p     (null (mission-inference-request-report context))
                            :controller-released-p (null (mission-inference-controller-find generation))))
              (mission-accounting-test--assert-facts known-p
               "Successful audit retry releases only product reporting ownership without replay or successor mutation"
               (list (list "(= 2 WRITES)" (= 2 writes)) (list "(= 1 BODIES)" (= 1 bodies))
                     (list "(= 2 (AREF WRITE-OUTCOME 0))" (= 2 (aref write-outcome 0)))
                     (list "(= 1 (AREF WRITE-OUTCOME 1))" (= 1 (aref write-outcome 1)))
                     (list "(EQ FAULT (AREF WRITE-OUTCOME 3))" (eq fault (aref write-outcome 3)))
                     (list "(EQUAL NATIVE-SNAPSHOT
       (MISSION--ACCOUNTING-VALUES (MISSION--ACCOUNTING-GOAL CONTEXT)))"
                           (equal native-snapshot
                                  (mission--accounting-values (mission--accounting-goal context))))
                     (list "(EQUAL NATIVE-OPENING (MISSION-CONTEXT-ACCOUNTING-OPENING CONTEXT))"
                           (equal native-opening (mission-context-accounting-opening context)))
                     (list "(PLUSP (AREF MAP-OUTCOME 0))" (plusp (aref map-outcome 0)))
                     (list "(= (AREF MAP-OUTCOME 0) (AREF MAP-OUTCOME 1))"
                           (= (aref map-outcome 0) (aref map-outcome 1)))
                     (list "(NULL (AREF MAP-OUTCOME 3))" (null (aref map-outcome 3)))
                     (list "(=
 (IF KNOWN-P
     5
     0)
 (GETF (MISSION--ACCOUNTING-GOAL CONTEXT) :TOKENS-USED))"
                           (=
                            (if known-p
                                5
                                0)
                            (getf (mission--accounting-goal context) :tokens-used)))
                     (list "(EQUAL OPENING GOAL)" (equal opening goal))
                     (list "(EQ GOAL (MISSION-CONTEXT-GOAL CONTEXT))"
                           (eq goal (mission-context-goal context)))
                     (list "(EQ CONVERSATION (MISSION-INFERENCE-REQUEST-CONVERSATION REQUEST))"
                           (eq conversation (mission-inference-request-conversation request)))
                     (list "(EQ ':PERSISTED (MISSION-INFERENCE-REQUEST-STATE REQUEST))"
                           (eq ':persisted (mission-inference-request-state request)))
                     (list "(EQUAL PAYLOAD (MISSION-INFERENCE-REQUEST-PAYLOAD REQUEST))"
                           (equal payload (mission-inference-request-payload request)))
                     (list "(NULL (GETHASH IDENTIFIER (MISSION-CONTEXT-ACCOUNTING-REQUESTS CONTEXT)))"
                           (null (gethash identifier (mission-context-accounting-requests context))))
                     (list "(NULL (MISSION-INFERENCE-REQUEST-REPORT CONTEXT))"
                           (null (mission-inference-request-report context)))
                     (list "(NULL (MISSION-INFERENCE-CONTROLLER-FIND GENERATION))"
                           (null (mission-inference-controller-find generation))))))))
         (mission-accounting-test--assert-facts known-p
          "Exact reservation and leaf writer bindings are restored"
          (list
           (list "(EQ RESERVE (SYMBOL-FUNCTION 'MISSION--RESERVE-INFERENCE))"
                 (eq reserve (symbol-function 'mission--reserve-inference)))
           (list "(EQ WRITER (SYMBOL-FUNCTION 'LOG-APPEND))" (eq writer (symbol-function 'log-append)))))))))
  (dolist (mode '(:policy :committed-append))
    (mission-accounting-test--other-reload mode))
  nil)

(-> test-mission-accounting-cutover-races () null)
(defun test-mission-accounting-cutover-races ()
  "Reserve, settlement and check-to-append all serialize with same-goal replacement."
  (dolist (mode '(:reserve :settle :publish))
    (mission-exhaustion-test--run
     (lambda (application provider context goal)
       (declare (ignore provider))
       (let* ((entered   (bt:make-semaphore :count 0))
              (release   (bt:make-semaphore :count 0))
              (attaching (bt:make-semaphore :count 0))
              (record    (symbol-function 'conversation-append-record))
              (request   (unless (eq mode ':reserve) (mission--reserve-inference context)))
              (paused-p  nil)
              (worker    nil)
              (cutover   nil))
         (test-call-with-function-replacements
          (list (list 'conversation-append-record
                      (lambda (conversation entry)
                        (when (and (not paused-p) (eq (first entry) ':goal)
                                   (eq conversation (application-conversation application))
                                   (eq (mission-context-find conversation) context))
                          ;; The original state lock already spans the currentness
                          ;; check. Pause immediately before the actual writer.
                          (setf paused-p t)
                          (bt:signal-semaphore entered)
                          (unless (bt:wait-on-semaphore release :timeout 10)
                            (error "Accounting publication barrier expired")))
                        (funcall record conversation entry))))
          (lambda ()
            (unwind-protect
                 (progn
                   (setf worker
                         (mission-inference-test--start
                          (lambda ()
                            (ecase mode
                              (:reserve
                               (setf request (mission--reserve-inference context)))
                              (:settle
                               (mission--settle-inference context request 5))
                              (:publish
                               (mission--record context))))))
                   (test-assert (bt:wait-on-semaphore entered :timeout 10) "Real goal publication is paused before append")
                   (setf cutover
                         (mission-inference-test--start
                          (lambda ()
                            (bt:signal-semaphore attaching)
                            (mission--attach application))))
                   (test-assert (bt:wait-on-semaphore attaching :timeout 10) "Actual attach is competing")
                   (test-assert (eq context (mission-context-find (application-conversation application)))
                                "Cutover cannot pass the original state/append transaction"))
              (mission-inference-test--join (remove nil (list worker cutover)) release 1))))
         (test-assert (and (null (mission-inference-test-thread-condition worker))
                           (null (mission-inference-test-thread-condition cutover)))
                      "Both actual transactions complete without lock inversion")
         (let ((opening (copy-tree goal)))
           (mission--record (mission-context-find (application-conversation application)))
           (unless (eq mode ':settle) (mission--settle-inference context request 5))
           (test-assert (equal opening goal) "Post-cutover callback cannot alter successor publication")
           (test-assert (= 5 (getf (mission--accounting-goal context) :tokens-used))
                        "The old charge is applied once on either side of the cutover")
           (test-assert (zerop (getf (mission--accounting-goal context) :requests-outstanding))
                        "No half-reservation is stranded across cutover"))))))
  nil)

(-> test-task-ordinary-storage-failures () null)
(defun test-task-ordinary-storage-failures ()
  "Keep uncancelled artifact and continuity failures physically FAILED."
  (dolist (writer '(task--write-result-artifact task-continuity-record-terminal))
    (let* ((configuration (test-configuration))
           (root          (test-configuration-root configuration))
           (orchestrator  (task-tests--orchestrator))
           (parent        (task-tests--primary-agent configuration "ordinary-storage"))
           (definition    (task-agent-definition-create :name         "ordinary-storage"
                                                        :description  "Test storage failures."
                                                        :instructions "Return an inert result."
                                                        :source       ':test))
           (job           (task-tests--register-job orchestrator parent definition :name "ordinary-storage"))
           (original      (symbol-function writer))
           (calls         0))
      (unwind-protect
           (progn
             (reinitialize-instance job :completion-owner-conversation
                                    (task-parent-root-conversation-identifier parent))
             (test-call-with-function-replacements
              (list (list writer
                          (lambda (target &rest arguments)
                            (if (eq target job)
                                (progn (incf calls)
                                       (error 'mission-lease-test-publication-fault))
                                (apply original target arguments)))))
              (lambda ()
                (let ((result (task-tests--terminal-result job :status ':success :output "inert")))
                  ;; This inert body did not submit a successful yield.
                  (setf (getf result :yielded-p) nil)
                  (task-tests--publish-terminal job :completed result))))
             (let* ((snapshot (task-job-snapshot job))
                    (result   (getf snapshot :result)))
               (push (list :mode (if (eq writer 'task--write-result-artifact)
                                    ':ordinary-artifact ':ordinary-continuity)
                           :snapshot snapshot :fault-count calls)
                     *mission-lease-test-probe-outcomes*)
               (test-assert (= calls 1) "Exact ordinary writer fault occurred once")
               (test-assert (and (eq (getf snapshot :state) ':failed)
                                 (eq (getf result :status) ':failed)
                                 (null (getf snapshot :cancellation-reason))
                                 (not (task-review-required-result-p result))
                                 (not (getf result :yielded-p))
                                 (stringp (getf result :error)))
                            "Uncancelled storage error is FAILED, without cancellation or suspension")))
        (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))))
  nil)

(-> test-mission-accounting-evidence-roundtrip () null)
(defun test-mission-accounting-evidence-roundtrip ()
  "Roundtrip actual missing-usage accounting through controller terminal evidence."
  (with-test-configuration (configuration root)
    (let* ((application  (mission-test--application configuration))
           (orchestrator (task-orchestrator-create))
           (child        (task-tests--make-job orchestrator :identifier "evidence-child"
                                             :parent-agent              (application-agent application))))
      (unwind-protect
           (progn
             (application-mission-start application (mission-test--specification :tokens most-positive-fixnum))
             (let* ((context (mission-context-find (application-conversation application)))
                    (goal    (application-goal application)))
               (setf (slot-value child 'mission-context) context)
               (let ((*mission-admitted-operation* child))
                 (mission--account-inference context (lambda () (json-object)) #'first))
               (setf (getf goal :exhaustion-policy) ':evaluate)
               (execution-lease-prepare context)
               ;; An unregistered report-only fixture has no execution authority.
               (setf (execution-lease-attempt (mission-context-lease-ledger context)) "evidence-fixture")
               (setf (mission-context-accounting-opening context)
                     (list :kind ':as-of-cutover :old-pending (mission--accounting-values goal)))
               (let* ((native   (mission--accounting-values goal))
                      (opening  (copy-tree (mission-context-accounting-opening context)))
                      (report   (task-controller-review-report context child))
                      (path     (merge-pathnames "terminal.sexp" root))
                      (positive (merge-pathnames "positive.sexp" root))
                      (record   (list :state ':aborted :result (list :status ':aborted :controller-review report))))
                 (test-assert (= 1 (rest (assoc "evidence-child" (getf goal :unknown-usage-by-child) :test #'equal)))
                              "Actual exact-child missing usage settled before controller capture")
                 (task-continuity--write positive '(:state :aborted :result (:status :aborted)) :require-absent t)
                 (test-assert (eq ':aborted (getf (task-continuity--read positive) :state)) "Proper-list positive reader control")
                 (task-continuity--write path record :require-absent t)
                 (let* ((capture (mission-lease-test-store-capture path))
                        (parsed  (mission-lease-test-store-parse capture (lambda () (task-continuity--read path)))))
                   (push (list :mode ':accounting-evidence :capture capture :report report
                              :native-dotted-path  '(:accounting :snapshot :unknown-usage-by-child 0 :rest)
                               :native-unchanged-p (equal native (mission--accounting-values goal)))
                         *mission-lease-test-probe-outcomes*)
                   (test-assert parsed "Actual controller accounting evidence is readable")
                   (test-assert (equal native (mission--accounting-values goal)) "Serialization does not mutate native accounting")
                   (test-assert (equal opening (mission-context-accounting-opening context))
                                "Evidence projection does not mutate the native opening")
                   (test-assert (and (equal '(("evidence-child" 1))
                                            (getf (getf (getf report :accounting) :snapshot) :unknown-usage-by-child))
                                     (equal '(("evidence-child" 1))
                                            (getf (getf (getf (getf report :accounting) :opening) :old-pending) :unknown-usage-by-child)))
                                "Snapshot and conservative old-pending evidence use proper two-element entries")
                   (when parsed
                     (test-assert (equal record parsed) "Exact terminal reader roundtrip"))))))
        (task-orchestrator-close orchestrator))))
  nil)

(-> test-task-continuity-prepublication-validation () null)
(defun test-task-continuity-prepublication-validation ()
  "Reject incompatible final text before immutable publication or overwrite."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let ((path     (merge-pathnames "rejected.sexp" root))
          (positive (merge-pathnames "positive.sexp" root))
          (rejected nil))
      (handler-case
          (task-continuity--write path (list :result (list :unsupported (cons "child" 1))) :require-absent t)
        (task-continuity-error () (setf rejected t)))
      (test-assert (and rejected (not (probe-file path)))
                   "Dotted data is rejected before any immutable target exists")
      (task-continuity--write positive '(:state :aborted :result (:status :aborted)) :require-absent t)
      (let ((before   (uiop:read-file-string positive))
            (conflict nil))
        (handler-case
            (task-continuity--write positive '(:state :failed) :require-absent t)
          (sexp-store:store-error () (setf conflict t)))
        (test-assert (and conflict (string= before (uiop:read-file-string positive)))
                     "Successful immutable publication cannot be overwritten")
        (test-assert (eq ':aborted (getf (task-continuity--read positive) :state))
                     "Repeated intended-reader inspection is current and readable"))))
  nil)

(-> test-task-continuity-validation-terminal-states () null)
(defun test-task-continuity-validation-terminal-states ()
  "Exercise real validation rejection in cancelled and ordinary host publication."
  (dolist (cancel-p '(nil t))
    (with-test-configuration (configuration root)
      (declare (ignore root))
      (let* ((orchestrator (task-tests--orchestrator))
             (parent       (task-tests--primary-agent configuration "validation-state"))
             (definition   (task-agent-definition-create :name "validation-state"
                            :description "Validate terminal evidence." :instructions "Return inert data." :source ':test))
             (job          (task-tests--register-job orchestrator parent definition :name "validation-state"))
             (original     (symbol-function 'task-continuity-record-terminal))
             (calls        0)
             (rejections   0)
             (prior        nil)
             (target       nil))
        (reinitialize-instance job :completion-owner-conversation (task-parent-root-conversation-identifier parent))
        (test-call-with-function-replacements
         (list (list 'task-continuity-record-terminal
                     (lambda (owned agent result &key state)
                       (if (eq owned job)
                           (progn
                             (incf calls)
                             (setf prior (copy-tree result)
                                   target (merge-pathnames "terminal.sexp" (task--artifact-root configuration job)))
                             (handler-case
                                 (funcall original owned agent
                                          (append result (list :unsupported (cons "child" 1))) :state state)
                               (task-continuity-error (condition)
                                 (incf rejections)
                                 (error condition))))
                           (funcall original owned agent result :state state)))))
         (lambda ()
           (if cancel-p
               (session-job-cancel job ':test)
               (let ((result (task-tests--terminal-result job :status ':success :output "inert")))
                 (setf (getf result :yielded-p) nil)
                 (task-tests--publish-terminal job ':completed result)))))
        (let* ((snapshot (task-job-snapshot job))
               (result   (getf snapshot :result)))
          (push (list :mode ':validation-terminal :cancel-p cancel-p :snapshot snapshot
                      :prior prior :calls calls :rejections rejections
                      :terminal-capture (mission-lease-test-store-capture target))
                *mission-lease-test-probe-outcomes*)
          (test-assert (and (= calls 1) (= rejections 1) (not (probe-file target)))
                       "One actual bounded-reader rejection, no terminal target or retry")
          (test-assert (and (eq (if cancel-p ':aborted ':failed) (getf snapshot :state))
                            (eq (if cancel-p ':aborted ':failed) (getf result :status)))
                       "Validation refusal preserves cancelled ABORTED and ordinary FAILED")
          (when cancel-p
            (test-assert (and (eq ':test (getf snapshot :cancellation-reason))
                              (equal (getf prior :error) (getf result :error))
                              (getf result :persistence-failed)
                              (eq ':terminal-metadata (getf result :persistence-failure-phase)))
                         "Accepted cancellation retains its error and finite secondary validation evidence"))))))
  nil)

(in-package #:autolith)


;;;; -- Actual Nested and Own Inline Authority --

(defstruct mission-authority-test-state
  "Finite unlocked body observations for one isolated authority fixture."
  mode
  (entered (bt:make-semaphore :count 0))
  (release (bt:make-semaphore :count 0))
  child outer receipt opening
  (row (list :entered nil :completed nil :result nil :condition nil :lineage nil :current nil))
  (failure (make-condition 'mission-lease-test-decline)))

(defparameter *mission-authority-test-state* nil
  "The sole process-isolated authority fixture, shared with actual task threads.")

(-> mission-authority-test--chain (execution-lease-receipt) list)
(defun mission-authority-test--chain (receipt)
  "Walk exact captured parent objects with a finite cycle and size check."
  (let ((seen nil))
    (labels ((visit (current)
               (unless (member current seen :test #'eq)
                 (assert (< (length seen) 32))
                 (push current seen)
                 (mapc #'visit (execution-lease-receipt-parents current)))))
      (visit receipt))
    (nreverse seen)))

(-> mission-authority-test--opening (execution-lease-receipt) list)
(defun mission-authority-test--opening (receipt)
  "Capture original identities and caps outside all admission gates."
  (mapcar (lambda (scope)
            (list :receipt scope :parents (copy-list (execution-lease-receipt-parents scope))
                  :cap    (execution-lease-receipt-cap scope)
                  :end    (execution-lease-receipt-end scope)
                  :job    (execution-lease-receipt-job scope)
                  :pool   (execution-lease-receipt-pool scope)
                  :owners (copy-list (execution-lease-receipt-owners scope))
                  :token  (execution-lease-receipt-token scope)))
          (mission-authority-test--chain receipt)))

(-> mission-authority-test--enter (task-child-agent) t)
(defun mission-authority-test--enter (child)
  "Record a genuine entered body and preserve every escaping serious condition."
  (let* ((state   *mission-authority-test-state*)
         (row     (mission-authority-test-state-row state))
         (outer   (task-child-agent-job child))
         (receipt *execution-lease-receipt*))
    (setf (mission-authority-test-state-child state) child
          (mission-authority-test-state-outer state) outer
          (mission-authority-test-state-receipt state) receipt
          (mission-authority-test-state-opening state) (mission-authority-test--opening receipt)
          (getf row :entered) t
          (getf row :lineage) (task-checkpoint-lineage-current-p (task-job-mission-context outer) outer nil)
          (getf row :current) (mission-lease--receipt-current-p receipt))
    (handler-bind ((serious-condition (lambda (condition) (setf (getf row :condition) condition))))
      (bt:signal-semaphore (mission-authority-test-state-entered state))
      (unless (bt:wait-on-semaphore (mission-authority-test-state-release state) :timeout 10)
        (error "Authority fixture body watchdog expired"))
      (when (eq (mission-authority-test-state-mode state) ':failed)
        (error (mission-authority-test-state-failure state)))
      (setf (getf row :completed) t (getf row :result) t)
      (agent-test-result "authority-inert-return" nil))))

(defclass mission-authority-test-leaf-tool (task-test-child-safe-tool) ()
  (:documentation "The entered leaf of the actual synchronous nested task."))

(defmethod tool-execute ((tool mission-authority-test-leaf-tool) (context tool-context) (arguments hash-table))
  "Hold the actual nested leaf until its inherited immutable cap cancels it."
  (declare (ignore tool arguments))
  (mission-authority-test--enter (tool-context-agent context)))

(-> mission-authority-test--assert-opening (mission-authority-test-state) null)
(defun mission-authority-test--assert-opening (state)
  "Check each real scope and its original nonempty intervening parent edges."
  (dolist (row (mission-authority-test-state-opening state))
    (let* ((scope  (getf row :receipt))
           (job    (getf row :job))
           (limits (remove nil (cons (getf row :cap)
                                     (mapcar #'execution-lease-receipt-end (getf row :parents))))))
      (test-assert (and (eq job (execution-lease-receipt-job scope))
                        (eq (getf row :pool) (job-pool job))
                        (equal (getf row :owners) (job-owner-identifiers job))
                        (equal (getf row :token) (job-run-token job)))
                   "Authority main-thread exact job/pool/owner/token associations")
      (test-assert (and (equal (getf row :parents) (execution-lease-receipt-parents scope))
                        (eql (getf row :cap) (job-deadline job))
                        (eql (getf row :end) (when limits (apply #'min limits))))
                   "Authority main-thread original EQ edges and minimum immutable end")))
  nil)

(-> mission-authority-test--run (keyword) null)
(defun mission-authority-test--run (mode)
  "Exercise a real source task and its actual own-inline or nested leaf lifecycle."
  (with-test-configuration (base root)
    (let* ((*task-default-maximum-runtime-milliseconds* (if (eq mode ':nested) 2000 10000))
           (original-operation-policy                   *mission-operation-milliseconds*)
           (configuration                               (configuration-copy base :working-directory root :mission-exhaustion-policy ':evaluate
                                                                             :mission-exhaustion-model (config :model base)))
           (provider                                    (make-instance 'task-test-provider :mode ':nested-cancel))
           (application                                 (mission-inference-test--application configuration provider))
           (registry                                    (application-tool-registry application))
           (state                                       (make-mission-authority-test-state :mode mode))
           (original-turn                               (symbol-function 'agent-run-user-turn))
           (runtime                                     nil))
      (setf *mission-authority-test-state* state
            (agent-provider (application-agent application)) provider)
      (tool-registry-register registry
                              (make-instance 'mission-authority-test-leaf-tool :namespace "test" :name "block"
                                             :description "Finite actual nested authority leaf."
                                             :parameters  (tool-object-schema (json-object) nil)))
      (task-augment-tool-registry registry)
      (let* ((run-tool     (tool-registry-find registry "child" "spawn"))
             (tool-context (make-instance 'tool-context :configuration configuration :worker nil
                                         :conversation (application-conversation application) :registry registry
                                         :agent (application-agent application) :call-id "authority-root")))
        (setf runtime (task-run-tool-orchestrator run-tool))
        (unwind-protect
             (test-call-with-function-replacements
              (list (list 'redact (lambda (text &key language model-directory runtime-library)
                                    (declare (ignore language model-directory runtime-library)) (values text ':ok)))
                    (list 'agent-run-user-turn
                          (lambda (agent text &rest arguments)
                            (if (and (typep agent 'task-child-agent) (not (eq mode ':nested)))
                                (mission-authority-test--enter agent)
                                (apply original-turn agent text arguments)))))
              (lambda ()
                (application-mission-start application (mission-test--specification))
                ;; Set fixture policy before minting scopes, including on task threads.
                (setf *mission-operation-milliseconds* (if (eq mode ':expired) 1000 10000))
                (test-assert (tool-result-success-p
                              (tool-execute run-tool tool-context
                                            (json-object "agent" "task" "task" "Finite local authority fixture."
                                                         "blocking" (json-false))))
                             "Authority main-thread actual task.run allocation")
                (test-assert (bt:wait-on-semaphore (mission-authority-test-state-entered state) :timeout 10)
                             "Authority main-thread genuine body entry")
                (let* ((outer   (mission-authority-test-state-outer state))
                       (receipt (mission-authority-test-state-receipt state))
                       (inline  (execution-lease-receipt-job receipt))
                       (context (task-job-mission-context outer))
                       (row     (mission-authority-test-state-row state))
                       (opening (mission-authority-test-state-opening state)))
                  (test-assert (and (getf row :entered) (getf row :lineage) (getf row :current))
                               "Authority main-thread production lineage positive before transition")
                  (mission-authority-test--assert-opening state)
                  (if (eq mode ':nested)
                      (let* ((tasks  (remove-if-not (lambda (entry) (typep (getf entry :job) 'task-job)) opening))
                             (parent (task-job-parent-agent outer)))
                        (test-assert (and (= 2 (length tasks)) (>= (length opening) 4)
                                          (typep parent 'task-child-agent)
                                          (find (task-child-agent-job parent) tasks :key (lambda (entry) (getf entry :job)) :test #'eq)
                                          (= 2 (task-test-provider-request-count provider)))
                                     "Authority main-thread actual nested task parent and finite intervening chain")
                        (test-assert (and (execution-lease-receipt-parents receipt)
                                          (< (execution-lease-receipt-end receipt) (execution-lease-receipt-cap receipt)))
                                     "Authority main-thread earlier actual ancestor cap limits entered innermost scope"))
                      (test-assert (and (typep inline 'execution-lease-inline-job)
                                        (eq outer (execution-lease-receipt-operation receipt))
                                        (eq outer (execution-lease-receipt-job (first (execution-lease-receipt-parents receipt))))
                                        (zerop (task-test-provider-request-count provider)))
                                   "Authority main-thread actual own-inline outer EQ associations and inert body"))
                  (when (eq mode ':expired)
                    (test-assert (and (eql (execution-lease-receipt-cap receipt) (execution-lease-receipt-end receipt))
                                      (< (execution-lease-receipt-cap receipt)
                                         (execution-lease-receipt-end (first (execution-lease-receipt-parents receipt)))))
                                 "Authority main-thread original own-inline cap precedes its outer cap"))
                  (when (eq mode ':cancelled)
                    (test-assert (job-cancel inline :reason ':user) "Authority main-thread cancellation accepted before publication"))
                  (when (eq mode ':failed)
                    (bt:signal-semaphore (mission-authority-test-state-release state)))
                  (multiple-value-bind (snapshot terminal) (job-await inline :timeout-seconds 10)
                    (test-assert terminal "Authority main-thread actual inline physical terminal")
                    (test-assert (if (eq mode ':failed)
                                     (eq (getf snapshot :state) ':failed)
                                     (and (eq (getf snapshot :state) ':aborted)
                                          (eq (getf snapshot :cancellation-reason) (if (eq mode ':cancelled) ':user ':timeout))))
                                 "Authority main-thread real first terminal reason at original cap")
                    (test-assert (and (not (getf row :completed)) (getf row :condition)
                                      (if (eq mode ':failed)
                                          (eq (getf row :condition) (mission-authority-test-state-failure state))
                                          (typep (getf row :condition) 'job-aborted)))
                                 "Authority main-thread raw serious condition retained without swallowing")
                    (job-await outer :timeout-seconds 10)
                    (let* ((terminal-row (execution-lease-receipt-terminal receipt))
                           (ended        (getf terminal-row :ended-at))
                           (source       (execution-lease-receipt-revocation-source receipt)))
                      (test-assert (and (integerp ended) (execution-lease-receipt-closed-p receipt)
                                        (if (member mode '(:nested :expired))
                                            (>= ended (execution-lease-receipt-end receipt))
                                            (< ended (execution-lease-receipt-end receipt))))
                                   "Authority main-thread actual physical ended-at and immutable-cap chronology")
                      (when (eq mode ':nested)
                        (test-assert (and (eq (execution-lease-receipt-reason receipt) ':timeout)
                                          (member source (rest (mission-authority-test--chain receipt)) :test #'eq)
                                          (eql (execution-lease-receipt-cap source) (execution-lease-receipt-end receipt)))
                                     "Authority main-thread actual earlier ancestor source cancels innermost entered scope")))
                    (test-assert (and (not (mission-lease--receipt-current-p receipt))
                                      (not (mission-lease--receipt-current-p receipt :unwind-p t))
                                      (not (task-checkpoint-lineage-current-p context outer nil)))
                                 "Authority main-thread production receipt and lineage refuse terminal own scope")
                    (test-assert (every (lambda (entry)
                                         (let ((scope (getf entry :receipt)))
                                           (and (eql (getf entry :cap) (execution-lease-receipt-cap scope))
                                                (eql (getf entry :end) (execution-lease-receipt-end scope))
                                                (equal (getf entry :parents) (execution-lease-receipt-parents scope))))) opening)
                                 "Authority main-thread exact original caps ends and parent objects retained")
                    (setf (getf row :terminal) snapshot
                          (getf row :receipt-terminal) (execution-lease-receipt-terminal receipt)
                          (getf row :reason) (execution-lease-receipt-reason receipt)
                          (getf row :source) (execution-lease-receipt-revocation-source receipt)
                          (getf row :closed) (execution-lease-receipt-closed-p receipt)
                          (getf row :outer-terminal) (job-snapshot outer))))))
          (bt:signal-semaphore (mission-authority-test-state-release state))
          (unwind-protect (when runtime (task-orchestrator-close runtime))
            (setf *mission-operation-milliseconds* original-operation-policy)))))
    nil))

(-> test-mission-lease-actual-nested-ancestor-cap () null)
(defun test-mission-lease-actual-nested-ancestor-cap ()
  "Cancel an actual entered nested task leaf at its real earlier ancestor cap."
  (mission-authority-test--run ':nested))

(-> test-mission-lease-own-inline-cancelled () null)
(defun test-mission-lease-own-inline-cancelled ()
  "Refuse cancelled own-inline authority after a genuine production positive."
  (mission-authority-test--run ':cancelled))

(-> test-mission-lease-own-inline-failed () null)
(defun test-mission-lease-own-inline-failed ()
  "Refuse failed own-inline authority after its exact inert condition escapes."
  (mission-authority-test--run ':failed))

(-> test-mission-lease-own-inline-expired () null)
(defun test-mission-lease-own-inline-expired ()
  "Refuse expired own-inline authority at its original assigned cap."
  (mission-authority-test--run ':expired))


;;;; -- Bounded Child Retained Settlement --

(defclass mission-accounting-test-child-provider (mission-inference-test-child-provider)
  ((outcome :initarg :outcome :reader mission-accounting-test-child-outcome
            :documentation "Bounded native provider observations.")
   (receipt :initform nil :accessor mission-accounting-test-child-receipt
            :documentation "Actual supervised provider receipt.")
   (job :initform nil :accessor mission-accounting-test-child-job
        :documentation "Actual admitted task at provider entry."))
  (:documentation "Local transport returning a genuine result with missing usage."))

(defmethod provider-stream-turn
    ((provider mission-accounting-test-child-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Return one local child response without a usage object."
  (declare (ignore conversation tool-namespaces event-callback goal-context compaction-p))
  (mission-accounting-test--observe
   (mission-accounting-test-child-outcome provider)
   (lambda ()
     (setf (mission-accounting-test-child-receipt provider) *execution-lease-receipt*
           (mission-accounting-test-child-job provider) *mission-admitted-operation*)
     (agent-test-result "bounded-missing-usage"
                        (list (agent-test-call :call-id "bounded-yield" :namespace "yield" :name "submit"
                                              :arguments (json-encode (json-object "status" "success" "text" "Local result."))))
                        :usage (json-object)))))

(-> test-mission-accounting-bounded-child-reload () null)
(defun test-mission-accounting-bounded-child-reload ()
  "Retain and reopen actual bounded-child UNKNOWN settlement after cutover."
  (with-test-configuration (base root)
    (let* ((configuration (configuration-copy base :working-directory root :mission-exhaustion-policy ':evaluate
                                             :mission-exhaustion-model (config :model base)))
           (main          sb-thread:*current-thread*)
           (outcomes      (loop for name in '(:provider :reserve :wrapper :usage :leaf :settle :policy :retry :load :map :record)
                                collect (cons name (vector 0 0 nil nil nil nil 0 main))))
           (provider      (make-instance 'mission-accounting-test-child-provider :outcome (rest (assoc :provider outcomes))))
           (application   (mission-inference-test--application configuration provider))
           (registry      (task-augment-tool-registry (application-tool-registry application)))
           (run-tool      (tool-registry-find registry "child" "spawn"))
           (orchestrator  (task-run-tool-orchestrator run-tool))
           (conversation  (application-conversation application))
           (path          (conversation-pathname conversation))
           (ready         (sb-thread:make-semaphore :count 0))
           (release       (sb-thread:make-semaphore :count 0))
           (cancelled     (sb-thread:make-semaphore :count 0))
           (canceller     (symbol-function 'job-cancel))
           (cancellation  (vector nil nil nil nil))
           (reserve       (symbol-function 'mission--reserve-inference))
           (account       (symbol-function 'mission--account-inference))
           (settle        (symbol-function 'mission--settle-inference-counters))
           (policy        (symbol-function 'mission--settlement-policy))
           (writer        (symbol-function 'log-append))
           (fault         (make-condition 'mission-error :reason ':fixture-writer :message "Immediate child audit fault"))
           (context nil) (goal nil) (request nil) (child nil) (successor nil)
           (before nil) (successor-values nil) (successor-opening nil) (payload nil)
           (missing-p nil) (released-p nil) (cutover-p nil) (armed-p t) (leaf-after-cutover-p nil)
           (child-conversation nil) (allocation nil) (receipt nil) (usage-returned-p nil)
           (reloaded nil) (records nil) (goals-before nil) (snapshot nil) (terminal nil))
      (labels ((observe (name function)
                 (mission-accounting-test--observe (rest (assoc name outcomes)) function))

               (check (name value)
                 (format t "~&G659-FACT ~s~%" (list :name name :value (not (null value))))
                 (test-assert value (symbol-name name)))

               (map-records (destination)
                 (let ((rows nil))
                   (observe :map (lambda () (conversation-map-records destination (lambda (row) (push row rows)))))
                   (nreverse rows))))
        (setf (agent-provider (application-agent application)) provider)
        (push (list :bounded-child t :target fault :outcomes outcomes) *mission-accounting-test-outcomes*)
        (unwind-protect
             (test-call-with-function-replacements
              (list
               (list 'job-cancel
                     (lambda (job &rest arguments &key reason &allow-other-keys)
                       (let ((values (multiple-value-list (apply canceller job arguments))))
                         (when (eq job child)
                           (setf (aref cancellation 0) job
                                 (aref cancellation 1) sb-thread:*current-thread*
                                 (aref cancellation 2) reason
                                 (aref cancellation 3) (first values))
                           (sb-thread:signal-semaphore cancelled))
                         (values-list values))))
               (list 'redact (lambda (text &key language model-directory runtime-library)
                               (declare (ignore language model-directory runtime-library)) (values text ':ok)))
               (list 'mission--reserve-inference
                     (lambda (target &key allowance)
                       (if (eq target context)
                           (observe :reserve
                                    (lambda ()
                                      (setf request (funcall reserve target :allowance allowance)
                                            child (mission-inference-request-child request)
                                            allocation (mission-inference-request-amount request)
                                            before (mission--accounting-values (mission--accounting-goal context)))
                                      request))
                           (funcall reserve target :allowance allowance))))
               (list 'mission--account-inference
                     (lambda (target function usage-function &key allowance)
                       (if (eq target context)
                           (observe :wrapper
                                    (lambda ()
                                      (funcall account target function
                                               (lambda (values)
                                                 (observe :usage
                                                          (lambda ()
                                                            (let ((usage (funcall usage-function values)))
                                                              (setf missing-p (null (rlm-usage-billable-tokens (provider-usage-normalize usage)))
                                                                    usage-returned-p t
                                                                    child-conversation (task-job-execution-metadata child)
                                                                    receipt *execution-lease-receipt*)
                                                              (sb-sys:without-interrupts
                                                                (sb-thread:signal-semaphore ready)
                                                                (unless (sb-thread:wait-on-semaphore release :timeout 10)
                                                                  (error "Owned usage release timed out."))
                                                                (setf released-p t))
                                                             (job-check-cancellation child)
                                                             usage))))
                                               :allowance allowance)))
                           (funcall account target function usage-function :allowance allowance))))
               (list 'mission--settle-inference-counters
                     (lambda (target handle tokens &rest arguments)
                       (if (eq target context)
                           (observe :settle (lambda () (apply settle target handle tokens arguments)))
                           (apply settle target handle tokens arguments))))
               (list 'mission--settlement-policy
                     (lambda (target &rest arguments)
                       (if (eq target context)
                           (observe :policy (lambda () (apply policy target arguments)))
                           (apply policy target arguments))))
               (list 'log-append
                     (lambda (target record &rest arguments)
                       (if (and request (equal target (conversation-log-pathname conversation))
                                (eq (first record) :mission-accounting-settlement)
                                (equal (getf (rest record) :request-id) (mission-inference-request-identifier request)))
                           (observe :leaf
                                    (lambda ()
                                      (when armed-p
                                        (setf leaf-after-cutover-p cutover-p
                                              payload (copy-tree (mission-inference-request-payload request)))
                                        (error fault))
                                      (apply writer target record arguments)))
                           (apply writer target record arguments)))))
              (lambda ()
                (application-mission-start application (mission-test--specification :turns 10 :tokens 100))
                (setf context (mission-context-find conversation) goal (application-goal application))
                (let* ((tool-context (make-instance 'tool-context :configuration configuration :worker nil
                                                    :conversation conversation :registry registry
                                                    :agent (application-agent application) :call-id "bounded-accounting"))
                       (result       (tool-execute run-tool tool-context
                                                   (json-object "agent" "task" "task" "Return one local result." "blocking" (json-false)))))
                  (check :actual-task-created (tool-result-success-p result)))
                (check :actual-usage-boundary (sb-thread:wait-on-semaphore ready :timeout 10))
                (check :real-child (typep child 'task-job))
                (check :positive-allocation (and (integerp allocation) (plusp allocation)))
                (check :provider-usage-absent (and missing-p usage-returned-p))
                (check :original-child-context (eq context (task-job-mission-context child)))
                (check :original-request-context (eq context (mission-inference-request-context request)))
                (check :original-request-child (eq child (mission-inference-request-child request)))
                (check :original-request-conversation (eq conversation (mission-inference-request-conversation request)))
                (check :actual-ordinary-allowance (null (mission-inference-request-allowance request)))
                (check :provider-actual-owned-job (eq child (mission-accounting-test-child-job provider)))
                (let ((body-receipt (mission-accounting-test-child-receipt provider)))
                  (format t "~&G659-RECEIPTS ~s~%"
                          (list :provider-present-p   (not (null body-receipt))
                                :provider-terminal-p  (and body-receipt (not (null (execution-lease-receipt-terminal body-receipt))))
                                :provider-settlements (and body-receipt (execution-lease-receipt-settlements body-receipt))
                                :usage-present-p      (not (null receipt))
                                :usage-terminal-p     (and receipt (not (null (execution-lease-receipt-terminal receipt))))
                                :usage-settlements    (and receipt (execution-lease-receipt-settlements receipt))))
                  (check :provider-supervision-closed
                         (and body-receipt (execution-lease-receipt-terminal body-receipt)))
                  (check :original-accounting-receipt-held
                         (and receipt (plusp (execution-lease-receipt-settlements receipt)))))
                (check :original-goal-eq (eq goal (mission-context-goal context)))
                (check :original-pending (eq request (gethash (mission-inference-request-identifier request)
                                                            (mission-context-accounting-requests context))))
                (setf successor (mission--attach application))
                (observe :record (lambda () (mission--record successor)))
                (setf successor-values (mission--accounting-values goal)
                      successor-opening (copy-tree (mission-context-accounting-opening successor))
                      goals-before (remove-if-not (lambda (row) (eq :goal (first row))) (map-records conversation))
                      cutover-p t)
                 ;; ATTACH only wakes the guardian. Observe cancellation before releasing usage.
                (check :actual-cancellation-observed (sb-thread:wait-on-semaphore cancelled :timeout 10))
                (format t "~&G667-CANCELLATION ~s~%"
                        (list :target-eq (eq child (aref cancellation 0))
                              :thread    (and (aref cancellation 1) (sb-thread:thread-name (aref cancellation 1)))
                              :reason (aref cancellation 2) :accepted (aref cancellation 3)))
                (check :actual-cancellation-target-eq (eq child (aref cancellation 0)))
                (check :actual-cancellation-off-main (and (aref cancellation 1) (not (eq main (aref cancellation 1)))))
                (check :actual-cancellation-replaced (eq :replaced (aref cancellation 2)))
                (check :actual-cancellation-accepted (aref cancellation 3))
                (sb-thread:signal-semaphore release)
                (multiple-value-setq (snapshot terminal) (job-await child :timeout-seconds 10))
                (format t "~&G659-OUTER ~s~%"
                        (list :terminal terminal :state (getf snapshot :state) :reason (getf snapshot :cancellation-reason)
                              :yielded-p (getf (getf snapshot :result) :yielded-p) :usage-released-p released-p
                              :child-conversation-present-p (not (null (getf child-conversation :conversation-file)))
                              :payload-snapshot (getf (rest payload) :snapshot) :payload-opening (getf (rest payload) :opening)))
                (dolist (entry outcomes) (mission-accounting-test--diagnostic ':bounded-child (first entry) (rest entry) :target fault))
                (check :child-terminal terminal)
                (check :cutover-reached-leaf leaf-after-cutover-p)
                (check :actual-leaf-fault-eq (eq fault (aref (rest (assoc :leaf outcomes)) 3)))
                (check :one-allocation (= 1 (aref (rest (assoc :reserve outcomes)) 0)))
                (check :one-provider (= 1 (aref (rest (assoc :provider outcomes)) 1)))
                (check :unknown-payload (eq :unknown (getf (rest payload) :classification)))
                (check :unknown-not-known-zero (null (getf (rest payload) :known-tokens)))
                (check :exact-child-id (equal (job-identifier child) (getf (rest payload) :child-id)))
                (check :exact-reservation-charge (= allocation (getf (rest payload) :reservation-charge)))
                (let* ((generation (mission-context-accounting-generation context))
                       (discovered (mission-inference-controller-find generation))
                       (reports    (getf (mission-inference-request-report discovered) :requests))
                       (identifier (getf (first reports) :request-id))
                       (native     (mission--accounting-values (mission--accounting-goal context)))
                       (pending    (getf successor-opening :old-pending)))
                  (format t "~&G659-RETAINED ~s~%" (list :reports reports :native native :reserved-before before :successor-pending pending))
                  (check :product-original-context (eq discovered context))
                  (check :one-report (= 1 (length reports)))
                  (check :retained-state (eq :unpersisted (getf (first reports) :state)))
                  (check :retained-phase (eq :audit-append (getf (first reports) :phase)))
                  (check :retained-uncertainty (getf (first reports) :append-uncertain-p))
                  (check :one-nonempty-child-attribution
                         (equal (list (cons (job-identifier child) 1)) (getf native :unknown-usage-by-child)))
                  (check :one-exact-child-reservation-charge
                         (equal (list (list (job-identifier child) :requests 1 :tokens allocation))
                                (getf native :reservation-charges-by-child)))
                  (check :bounded-not-unbounded-unknown (= (getf before :unknown-usage) (getf native :unknown-usage)))
                  (check :successor-distinct-generation
                         (not (equal generation (mission-context-accounting-generation successor))))
                  (check :wrapper-original-cancellation
                         (typep (aref (rest (assoc :wrapper outcomes)) 3) 'job-aborted))
                  (check :usage-wrapper-cancellation-eq
                         (eq (aref (rest (assoc :wrapper outcomes)) 3) (aref (rest (assoc :usage outcomes)) 3)))
                  (check :settler-separate-writer-transport
                         (typep (aref (rest (assoc :settle outcomes)) 3) 'conversation-invariant-error))
                  (check :physical-original-cancellation
                         (and (eq :aborted (getf snapshot :state)) (eq :replaced (getf snapshot :cancellation-reason))))
                  (check :report-identity (equal identifier (mission-inference-request-identifier request)))
                  (check :once-used (= (+ (getf before :tokens-used) allocation) (getf native :tokens-used)))
                  (check :once-reservation-charge (= (+ (getf before :reservation-charged-tokens) allocation)
                                                     (getf native :reservation-charged-tokens)))
                  (check :one-outstanding-release (= (1- (getf before :requests-outstanding)) (getf native :requests-outstanding)))
                  (check :one-reservation-release (= (- (getf before :tokens-reserved) allocation) (getf native :tokens-reserved)))
                  (check :no-second-turn (= (getf before :turns-used) (getf native :turns-used)))
                  (check :old-pending-positive (= 1 (getf pending :requests-outstanding)))
                  (check :old-pending-reservation (= allocation (getf pending :tokens-reserved)))
                  (setf armed-p nil)
                  (observe :retry (lambda () (mission-inference-request-retry discovered identifier)))
                  (check :immutable-payload (equal payload (mission-inference-request-payload request)))
                  (check :no-retry-arithmetic (equal native (mission--accounting-values (mission--accounting-goal context))))
                  (check :uncertainty-retained (mission-inference-request-append-uncertain-p request))
                  (check :controller-released (null (mission-inference-controller-find generation)))
                  (check :request-released (null (gethash identifier (mission-context-accounting-requests context))))
                  (check :successor-live-isolated (equal successor-values (mission--accounting-values goal)))
                  (check :successor-opening-isolated (equal successor-opening (mission-context-accounting-opening successor)))
                  (setf reloaded (observe :load (lambda () (conversation-load path)))
                        records (map-records reloaded))
                  (let ((audits (remove-if-not (lambda (row) (and (eq :mission-accounting-settlement (first row))
                                                                (equal identifier (getf (rest row) :request-id)))) records)))
                    (check :one-durable-audit (= 1 (length audits)))
                    (when audits
                      (let ((fields (copy-list (rest (first audits)))))
                        (remf fields :seq) (remf fields :time)
                        (check :complete-logical-payload (equal (cons :mission-accounting-settlement fields) payload))
                        (loop for (key value) on (rest payload) by (lambda (tail) (rest (rest tail)))
                              do (check key (equal value (getf fields key)))))))
                  (check :durable-successor-isolated
                         (equal goals-before (remove-if-not (lambda (row) (eq :goal (first row))) records)))
                 (dolist (entry outcomes) (mission-accounting-test--diagnostic ':bounded-child (first entry) (rest entry) :target fault))
                 (check :no-detached-policy (zerop (aref (rest (assoc :policy outcomes)) 0)))
                 (check :provider-not-replayed (= 1 (aref (rest (assoc :provider outcomes)) 0)))
                 (check :reserve-not-replayed (= 1 (aref (rest (assoc :reserve outcomes)) 0)))
                 (check :settlement-not-replayed (= 1 (aref (rest (assoc :settle outcomes)) 0)))
                 (check :actual-usage-cancelled-after-release
                        (let ((outcome (rest (assoc :usage outcomes))))
                          (and (= 1 (aref outcome 0)) (zerop (aref outcome 1)) (aref outcome 5)
                               (typep (aref outcome 3) 'job-aborted))))
                 (check :actual-usage-released released-p)
                 (check :actual-child-not-yielded (null (getf (getf snapshot :result) :yielded-p)))
                 (check :leaf-retry-completed-once (= 1 (aref (rest (assoc :leaf outcomes)) 1)))
                 (check :retry-returned-original-uncertainty (equal '(t t) (aref (rest (assoc :retry outcomes)) 2)))
                 (dolist (operation '(:retry :load :map))
                   (let ((outcome (rest (assoc operation outcomes))))
                     (check operation (and (plusp (aref outcome 0)) (= (aref outcome 0) (aref outcome 1))
                                           (aref outcome 4) (null (aref outcome 5)))))))))
          (sb-thread:signal-semaphore release)
          (when child (job-await child :timeout-seconds 10))
          (task-orchestrator-close orchestrator))
        (check :reserve-restored (eq reserve (symbol-function 'mission--reserve-inference)))
        (check :wrapper-restored (eq account (symbol-function 'mission--account-inference)))
        (check :settler-restored (eq settle (symbol-function 'mission--settle-inference-counters)))
        (check :policy-restored (eq policy (symbol-function 'mission--settlement-policy)))
        (check :writer-restored (eq writer (symbol-function 'log-append))))))
  nil)

(defclass mission-accounting-test-independent-provider
          (mission-accounting-test-child-provider)
          ((thread :initform nil :accessor
            mission-accounting-test-independent-thread :documentation
            "Actual provider callback thread, captured without control changes."))
          (:documentation
           "Observation-only transport subtype for the genuine independently budgeted child."))

(defmethod provider-stream-turn :around
           ((provider mission-accounting-test-independent-provider)
            (conversation conversation)
            &key tool-namespaces event-callback goal-context compaction-p)
  "Retain actual child callback thread and delegate the original provider path."
  (declare
   (ignore conversation tool-namespaces event-callback goal-context
    compaction-p))
  (setf (mission-accounting-test-independent-thread provider)
        sb-thread:*current-thread*)
  (call-next-method))

(-> test-mission-accounting-independent-child-reload nil null)
(defun test-mission-accounting-independent-child-reload ()
  "Retain and reopen actual bounded-child UNKNOWN settlement after cutover."
  (with-test-configuration (base root)
   (let* ((configuration
           (configuration-copy base :working-directory    root
                               :mission-exhaustion-policy ':evaluate
                               :mission-exhaustion-model  (config :model base)))
          (main                 sb-thread:*current-thread*)
          (outcomes
           (loop for name in '(:provider :reserve :wrapper :usage :leaf :settle
                               :policy :retry :load :map :record :trigger)
                 collect (cons name (vector 0 0 nil nil nil nil 0 main))))
          (provider
           (make-instance 'mission-accounting-test-independent-provider
                          :outcome (rest (assoc :provider outcomes))))
          (application
           (mission-inference-test--application configuration provider))
          (registry
           (task-augment-tool-registry
            (application-tool-registry application)))
          (run-tool             (tool-registry-find registry "child" "spawn"))
          (orchestrator         (task-run-tool-orchestrator run-tool))
          (conversation         (application-conversation application))
          (path                 (conversation-pathname conversation))
          (ready                (sb-thread:make-semaphore :count 0))
          (release              (sb-thread:make-semaphore :count 0))
          (cancelled            (sb-thread:make-semaphore :count 0))
          (canceller            (symbol-function 'job-cancel))
          (cancellation         (vector nil nil nil nil))
          (reserve              (symbol-function 'mission--reserve-inference))
          (account              (symbol-function 'mission--account-inference))
          (settle               (symbol-function 'mission--settle-inference-counters))
          (policy               (symbol-function 'mission--settlement-policy))
          (writer               (symbol-function 'log-append))
          (fault
           (make-condition 'mission-error :reason ':fixture-writer :message
                           "Immediate child audit fault"))
          (context              nil)
          (goal                 nil)
          (request              nil)
          (child                nil)
          (successor            nil)
          (before               nil)
          (successor-values     nil)
          (successor-opening    nil)
          (payload              nil)
          (missing-p            nil)
          (released-p           nil)
          (cutover-p            nil)
          (armed-p              t)
          (leaf-after-cutover-p nil)
          (child-conversation   nil)
          (allocation           nil)
          (receipt              nil)
          (usage-returned-p     nil)
          (reloaded             nil)
          (records              nil)
          (goals-before         nil)
          (snapshot             nil)
          (terminal             nil)
          (review-thread        nil)
          (allowance0           nil)
          (allowance-before     nil)
          (successor-allowance  nil)
          (detached-route       nil)
          (actual-threads
           (loop for entry in outcomes
                 collect (cons (first entry) nil))))
     (labels ((observe (name function)
                (setf (rest (assoc name actual-threads))
                      sb-thread:*current-thread*)
                (mission-accounting-test--observe (rest (assoc name outcomes))
                 function))

              (check (name value)
                (format t "~&G659-FACT ~s~%"
                        (list :name name :value (not (null value))))
                (test-assert value (symbol-name name)))

              (map-records (destination)
                (let ((rows nil))
                  (observe :map
                   (lambda ()
                     (conversation-map-records destination
                                               (lambda (row)
                                                 (push row rows)))))
                  (nreverse rows))))
       (setf (agent-provider (application-agent application)) provider)
       (push (list :bounded-child t :target fault :outcomes outcomes)
             *mission-accounting-test-outcomes*)
       (unwind-protect
           (test-call-with-function-replacements
            (list
              (list 'job-cancel
                    (lambda (job &rest arguments &key reason &allow-other-keys)
                      (let ((values (multiple-value-list (apply canceller job arguments))))
                        (when (eq job child)
                          (setf (aref cancellation 0) job
                                (aref cancellation 1) sb-thread:*current-thread*
                                (aref cancellation 2) reason
                                (aref cancellation 3) (first values))
                          (sb-thread:signal-semaphore cancelled))
                        (values-list values))))
             (list 'redact
                   (lambda (text &key language model-directory runtime-library)
                     (declare
                      (ignore language model-directory runtime-library))
                     (values text ':ok)))
             (list 'mission--reserve-inference
                   (lambda (target &key allowance)
                     (if (eq target context)
                         (observe :reserve
                          (lambda ()
                            (setf request
                                    (funcall reserve target :allowance
                                             allowance)
                                  child
                                    (mission-inference-request-child request)
                                  allocation
                                    (mission-inference-request-amount request)
                                  before
                                    (mission--accounting-values
                                     (mission--accounting-goal context))
                                  allowance0
                                    (mission-inference-request-allowance
                                     request)
                                  allowance-before (copy-tree allowance0))
                            request))
                         (funcall reserve target :allowance allowance))))
             (list 'mission--account-inference
                   (lambda (target function usage-function &key allowance)
                     (if (eq target context)
                         (observe :wrapper
                          (lambda ()
                            (funcall account target function
                                     (lambda (values)
                                       (observe :usage
                                        (lambda ()
                                          (let ((usage
                                                 (funcall usage-function
                                                          values)))
                                            (setf missing-p
                                                    (null
                                                     (rlm-usage-billable-tokens
                                                      (provider-usage-normalize
                                                       usage)))
                                                  usage-returned-p t
                                                  child-conversation
                                                    (task-job-execution-metadata
                                                     child)
                                                  receipt
                                                    *execution-lease-receipt*)
                                            (sb-sys:without-interrupts
                                              (sb-thread:signal-semaphore ready)
                                              (unless
                                                  (sb-thread:wait-on-semaphore
                                                   release :timeout 10)
                                                (error
                                                 "Owned usage release timed out."))
                                              (setf released-p t))
                                            (job-check-cancellation child)
                                            usage))))
                                     :allowance allowance)))
                         (funcall account target function usage-function
                                  :allowance allowance))))
             (list 'mission--settle-inference-counters
                   (lambda (target handle tokens &rest arguments)
                     (if (eq target context)
                         (observe :settle
                          (lambda ()
                            (apply settle target handle tokens arguments)))
                         (apply settle target handle tokens arguments))))
             (list 'mission--settlement-policy
                   (lambda (target &rest arguments)
                     (if (eq target context)
                         (observe :policy
                          (lambda () (apply policy target arguments)))
                         (apply policy target arguments))))
             (list 'log-append
                   (lambda (target record &rest arguments)
                     (if (and request
                              (equal target
                                     (conversation-log-pathname conversation))
                              (eq (first record)
                                  :mission-accounting-settlement)
                              (equal (getf (rest record) :request-id)
                                     (mission-inference-request-identifier
                                      request)))
                         (observe :leaf
                          (lambda ()
                            (when armed-p
                              (setf leaf-after-cutover-p cutover-p
                                    payload
                                      (copy-tree
                                       (mission-inference-request-payload
                                        request)))
                              (error fault))
                            (apply writer target record arguments)))
                         (apply writer target record arguments)))))
            (lambda ()
              (application-mission-start application
                                         (mission-test--specification :turns 10
                                          :tokens                            100))
              (setf context (mission-context-find conversation)
                    goal (application-goal application))
              (progn
               (with-open-file
                   (s (merge-pathnames "independent-review.txt" root)
                    :direction :output :if-exists :error)
                 (write-string "One local review input." s))
               (application-mission-review-configure application
                                                     (list :id
                                                           "independent-child"
                                                           :phase
                                                           ':after-edit-set
                                                           :inputs
                                                           '("independent-review.txt")
                                                           :context
                                                           "Read the local input."
                                                           :turn-limit  4
                                                           :token-limit 40
                                                           :run-limit 1 :models
                                                           nil))
               (let ((tool-context
                      (make-instance 'tool-context :configuration configuration
                                     :worker nil :conversation conversation
                                     :registry registry :execution-runtime
                                     orchestrator :agent
                                     (application-agent application) :call-id
                                     "independent-accounting")))
                 (setf review-thread
                         (sb-thread:make-thread
                          (lambda ()
                            (handler-case
                             (observe :trigger
                              (lambda ()
                                (application-mission-review-trigger application
                                                                    "independent-child"
                                                                    :tool-context
                                                                    tool-context)))
                             (serious-condition nil nil)))
                          :name "Independent child allowance trigger"))))
              (progn
               (check :actual-usage-boundary
                (let ((reached (sb-thread:wait-on-semaphore ready :timeout 10)))
                  (dolist (entry outcomes)
                    (mission-accounting-test--diagnostic ':independent-child
                     (first entry) (rest entry) :target fault))
                  (let ((condition (aref (rest (assoc :trigger outcomes)) 3)))
                    (format t "~&G661-TRIGGER ~s~%"
                            (list :reached reached :thread
                                  (and review-thread
                                       (sb-thread:thread-name review-thread))
                                  :condition-type
                                  (and condition (type-of condition))
                                  :condition
                                  (and condition
                                       (princ-to-string condition)))))
                  reached))
               (unless child
                 (return-from test-mission-accounting-independent-child-reload
                   nil)))
              (check :real-child (typep child 'task-job))
              (check :positive-allocation
               (and (integerp allocation) (plusp allocation)))
              (check :provider-usage-absent (and missing-p usage-returned-p))
              (check :original-child-context
               (eq context (task-job-mission-context child)))
              (check :original-request-context
               (eq context (mission-inference-request-context request)))
              (check :original-request-child
               (eq child (mission-inference-request-child request)))
              (check :original-request-conversation
               (eq conversation
                   (mission-inference-request-conversation request)))
              (progn
               (check :independent-real-policy allowance0)
               (check :independent-policy-id
                (equal "independent-child" (getf allowance0 :id)))
               (check :production-child-checkpoint
                (eq allowance0
                    (getf (task-job-review-checkpoint child) :policy)))
               (check :registered-policy-route
                (eq allowance0
                    (gethash allowance0
                             (mission-context-accounting-allowances context))))
               (check :one-independent-turn
                (= 1 (getf allowance0 :turns-used)))
               (check :one-independent-request
                (= 1 (getf allowance0 :requests-outstanding)))
               (check :independent-reservation
                (= allocation (getf allowance0 :tokens-reserved)))
               (check :actual-child-thread
                (and (mission-accounting-test-independent-thread provider)
                     (not
                      (eq main
                          (mission-accounting-test-independent-thread
                           provider)))))
               (check :actual-provider-usage-thread
                (eq (mission-accounting-test-independent-thread provider)
                    (rest (assoc :usage actual-threads)))))
              (check :provider-actual-owned-job
               (eq child (mission-accounting-test-child-job provider)))
              (let ((body-receipt
                     (mission-accounting-test-child-receipt provider)))
                (format t "~&G659-RECEIPTS ~s~%"
                        (list :provider-present-p (not (null body-receipt))
                              :provider-terminal-p
                              (and body-receipt
                                   (not
                                    (null
                                     (execution-lease-receipt-terminal
                                      body-receipt))))
                              :provider-settlements
                              (and body-receipt
                                   (execution-lease-receipt-settlements
                                    body-receipt))
                              :usage-present-p    (not (null receipt))
                              :usage-terminal-p
                              (and receipt
                                   (not
                                    (null
                                     (execution-lease-receipt-terminal
                                      receipt))))
                              :usage-settlements
                              (and receipt
                                   (execution-lease-receipt-settlements
                                    receipt))))
                (check :provider-supervision-closed
                 (and body-receipt
                      (execution-lease-receipt-terminal body-receipt)))
                (check :original-accounting-receipt-held
                 (and receipt
                      (plusp (execution-lease-receipt-settlements receipt)))))
              (check :original-goal-eq
               (eq goal (mission-context-goal context)))
              (check :original-pending
               (eq request
                   (gethash (mission-inference-request-identifier request)
                            (mission-context-accounting-requests context))))
              (setf successor (mission--attach application))
              (observe :record (lambda () (mission--record successor)))
              (setf successor-values (mission--accounting-values goal)
                    successor-opening
                      (copy-tree
                       (mission-context-accounting-opening successor))
                    goals-before
                      (remove-if-not (lambda (row) (eq :goal (first row)))
                                     (map-records conversation))
                    cutover-p t
                    successor-allowance (mission--accounting-values allowance0)
                    detached-route
                      (gethash allowance0
                               (mission-context-accounting-allowances
                                context)))
              ;; Observe actual cancellation outside every admission and lifecycle lock.
              (check :actual-cancellation-observed (sb-thread:wait-on-semaphore cancelled :timeout 10))
              (format t "~&G667-CANCELLATION ~s~%"
                      (list :target-eq (eq child (aref cancellation 0))
                            :thread    (and (aref cancellation 1) (sb-thread:thread-name (aref cancellation 1)))
                            :reason (aref cancellation 2) :accepted (aref cancellation 3)))
              (check :actual-cancellation-target-eq (eq child (aref cancellation 0)))
              (check :actual-cancellation-off-main (and (aref cancellation 1) (not (eq main (aref cancellation 1)))))
              (check :actual-cancellation-replaced (eq :replaced (aref cancellation 2)))
              (check :actual-cancellation-accepted (aref cancellation 3))
              (sb-thread:signal-semaphore release)
              (multiple-value-setq (snapshot terminal)
                (job-await child :timeout-seconds 10))
              (format t "~&G659-OUTER ~s~%"
                      (list :terminal terminal :state (getf snapshot :state)
                            :reason           (getf snapshot :cancellation-reason)
                            :yielded-p
                            (getf (getf snapshot :result) :yielded-p)
                            :usage-released-p released-p
                            :child-conversation-present-p
                            (not
                             (null
                              (getf child-conversation :conversation-file)))
                            :payload-snapshot (getf (rest payload) :snapshot)
                            :payload-opening  (getf (rest payload) :opening)))
              (dolist (entry outcomes)
                (mission-accounting-test--diagnostic ':bounded-child
                 (first entry) (rest entry) :target fault))
              (check :child-terminal terminal)
              (check :cutover-reached-leaf leaf-after-cutover-p)
              (check :actual-leaf-fault-eq
               (eq fault (aref (rest (assoc :leaf outcomes)) 3)))
              (check :one-allocation
               (= 1 (aref (rest (assoc :reserve outcomes)) 0)))
              (check :one-provider
               (= 1 (aref (rest (assoc :provider outcomes)) 1)))
              (check :unknown-payload
               (eq :unknown (getf (rest payload) :classification)))
              (check :unknown-not-known-zero
               (null (getf (rest payload) :known-tokens)))
              (check :exact-child-id
               (equal (job-identifier child) (getf (rest payload) :child-id)))
              (check :exact-reservation-charge
               (= allocation (getf (rest payload) :reservation-charge)))
              (let* ((generation
                      (mission-context-accounting-generation context))
                     (discovered
                      (mission-inference-controller-find generation))
                     (reports
                      (getf (mission-inference-request-report discovered)
                            :requests))
                     (identifier (getf (first reports) :request-id))
                     (native
                      (mission--accounting-values
                       (mission--accounting-goal context)))
                     (pending    (getf successor-opening :old-pending)))
                (format t "~&G659-RETAINED ~s~%"
                        (list :reports reports :native native :reserved-before
                              before :successor-pending pending))
                (check :product-original-context (eq discovered context))
                (check :one-report (= 1 (length reports)))
                (check :retained-state
                 (eq :unpersisted (getf (first reports) :state)))
                (check :retained-phase
                 (eq :audit-append (getf (first reports) :phase)))
                (check :retained-uncertainty
                 (getf (first reports) :append-uncertain-p))
                (check :one-nonempty-child-attribution
                 (equal (list (cons (job-identifier child) 1))
                        (getf native :unknown-usage-by-child)))
                (check :one-exact-child-reservation-charge
                 (equal
                  (list
                   (list (job-identifier child) :requests 1 :tokens
                         allocation))
                  (getf native :reservation-charges-by-child)))
                (check :bounded-not-unbounded-unknown
                 (= (getf before :unknown-usage) (getf native :unknown-usage)))
                (check :successor-distinct-generation
                 (not
                  (equal generation
                         (mission-context-accounting-generation successor))))
                (check :wrapper-original-cancellation
                 (typep (aref (rest (assoc :wrapper outcomes)) 3)
                        'job-aborted))
                (check :usage-wrapper-cancellation-eq
                 (eq (aref (rest (assoc :wrapper outcomes)) 3)
                     (aref (rest (assoc :usage outcomes)) 3)))
                (check :settler-separate-writer-transport
                 (typep (aref (rest (assoc :settle outcomes)) 3)
                        'conversation-invariant-error))
                (check :physical-original-cancellation
                 (and (eq :aborted (getf snapshot :state))
                      (eq :replaced (getf snapshot :cancellation-reason))))
                (check :report-identity
                 (equal identifier
                        (mission-inference-request-identifier request)))
                (progn
                 (check :once-used
                  (= (+ (getf before :tokens-used) allocation)
                     (getf native :tokens-used)))
                 (check :independent-route-detached
                  (and detached-route (not (eq allowance0 detached-route))))
                 (check :independent-request-identity
                  (eq allowance0
                      (mission-inference-request-allowance request)))
                 (check :independent-once-used
                  (= (+ (getf allowance-before :tokens-used) allocation)
                     (getf detached-route :tokens-used)))
                 (check :independent-once-charge
                  (=
                   (+ (getf allowance-before :reservation-charged-tokens)
                      allocation)
                   (getf detached-route :reservation-charged-tokens)))
                 (check :independent-once-release
                  (zerop (getf detached-route :requests-outstanding)))
                 (check :independent-once-reservation-release
                  (zerop (getf detached-route :tokens-reserved)))
                 (check :independent-no-second-turn
                  (= (getf allowance-before :turns-used)
                     (getf detached-route :turns-used)))
                 (check :successor-allowance-protected
                  (equal successor-allowance
                         (mission--accounting-values allowance0))))
                (check :once-reservation-charge
                 (= (+ (getf before :reservation-charged-tokens) allocation)
                    (getf native :reservation-charged-tokens)))
                (check :one-outstanding-release
                 (= (1- (getf before :requests-outstanding))
                    (getf native :requests-outstanding)))
                (check :one-reservation-release
                 (= (- (getf before :tokens-reserved) allocation)
                    (getf native :tokens-reserved)))
                (check :no-second-turn
                 (= (getf before :turns-used) (getf native :turns-used)))
                (check :old-pending-positive
                 (= 1 (getf pending :requests-outstanding)))
                (check :old-pending-reservation
                 (= allocation (getf pending :tokens-reserved)))
                (setf armed-p nil)
                (observe :retry
                 (lambda ()
                   (mission-inference-request-retry discovered identifier)))
                (check :immutable-payload
                 (equal payload (mission-inference-request-payload request)))
                (check :no-retry-arithmetic
                 (equal native
                        (mission--accounting-values
                         (mission--accounting-goal context))))
                (check :uncertainty-retained
                 (mission-inference-request-append-uncertain-p request))
                (check :controller-released
                 (null (mission-inference-controller-find generation)))
                (check :request-released
                 (null
                  (gethash identifier
                           (mission-context-accounting-requests context))))
                (check :successor-live-isolated
                 (equal successor-values (mission--accounting-values goal)))
                (check :successor-opening-isolated
                 (equal successor-opening
                        (mission-context-accounting-opening successor)))
                (setf reloaded
                        (observe :load (lambda () (conversation-load path)))
                      records (map-records reloaded))
                (let ((audits
                       (remove-if-not
                        (lambda (row)
                          (and (eq :mission-accounting-settlement (first row))
                               (equal identifier
                                      (getf (rest row) :request-id))))
                        records)))
                  (check :one-durable-audit (= 1 (length audits)))
                  (when audits
                    (let ((fields (copy-list (rest (first audits)))))
                      (remf fields :seq)
                      (remf fields :time)
                      (check :complete-logical-payload
                       (equal (cons :mission-accounting-settlement fields)
                              payload))
                      (loop for (key value) on (rest payload) by (lambda (tail)
                                                                   (rest
                                                                    (rest
                                                                     tail)))
                            do (check key (equal value (getf fields key)))))))
                (progn
                 (check :durable-successor-isolated
                  (equal goals-before
                         (remove-if-not (lambda (row) (eq :goal (first row)))
                                        records)))
                 (format t "~&G661-INDEPENDENT-DURABLE ~s~%"
                         (list :policy-id (getf allowance0 :id)
                               :detached-accounting
                               (mission--accounting-values detached-route)
                               :payload-has-allowance
                               (member :allowance (rest payload))
                               :provider-thread
                               (sb-thread:thread-name
                                (mission-accounting-test-independent-thread
                                 provider))
                               :actual-threads
                               (mapcar
                                (lambda (entry)
                                  (list (first entry)
                                        (and (rest entry)
                                             (sb-thread:thread-name
                                              (rest entry)))))
                                actual-threads)))
                 (let* ((audit
                         (find-if
                          (lambda (row)
                            (and
                             (eq :mission-accounting-settlement (first row))
                             (equal identifier (getf (rest row) :request-id))))
                          records))
                        (decoded (getf (rest audit) :allowance))
                        (expected
                         (list :id (getf allowance0 :id) :turn-limit
                               (getf allowance0 :turn-limit) :token-limit
                               (getf allowance0 :token-limit) :snapshot
                               (mission--accounting-values detached-route))))
                   (format t "~&G661-ALLOWANCE-READER ~s~%"
                           (list :decoded decoded :expected expected
                                 :new-load-p (not (eq reloaded conversation))))
                   (check :durable-independent-allowance-present decoded)
                   (check :durable-independent-allowance-id
                    (equal (getf allowance0 :id) (getf decoded :id)))
                   (check :durable-independent-allowance-values
                    (equal expected decoded)))
                 (check :independent-route-no-retry-arithmetic
                  (= (+ (getf allowance-before :tokens-used) allocation)
                     (getf detached-route :tokens-used))))
                (dolist (entry outcomes)
                  (mission-accounting-test--diagnostic ':bounded-child
                   (first entry) (rest entry) :target fault))
                (check :no-detached-policy
                 (zerop (aref (rest (assoc :policy outcomes)) 0)))
                (check :provider-not-replayed
                 (= 1 (aref (rest (assoc :provider outcomes)) 0)))
                (check :reserve-not-replayed
                 (= 1 (aref (rest (assoc :reserve outcomes)) 0)))
                (check :settlement-not-replayed
                 (= 1 (aref (rest (assoc :settle outcomes)) 0)))
                (check :actual-usage-cancelled-after-release
                 (let ((outcome (rest (assoc :usage outcomes))))
                   (and (= 1 (aref outcome 0)) (zerop (aref outcome 1))
                        (aref outcome 5)
                        (typep (aref outcome 3) 'job-aborted))))
                (check :actual-usage-released released-p)
                (check :actual-child-not-yielded
                 (null (getf (getf snapshot :result) :yielded-p)))
                (check :leaf-retry-completed-once
                 (= 1 (aref (rest (assoc :leaf outcomes)) 1)))
                (check :retry-returned-original-uncertainty
                 (equal '(t t) (aref (rest (assoc :retry outcomes)) 2)))
                (dolist (operation '(:retry :load :map))
                  (let ((outcome (rest (assoc operation outcomes))))
                    (check operation
                     (and (plusp (aref outcome 0))
                          (= (aref outcome 0) (aref outcome 1))
                          (aref outcome 4) (null (aref outcome 5)))))))))
         (sb-thread:signal-semaphore release)
         (progn
          (when child (job-await child :timeout-seconds 10))
          (when review-thread
            (sb-thread:join-thread review-thread :timeout 10 :default
                                   :unfinished)))
         (task-orchestrator-close orchestrator))
       (check :reserve-restored
        (eq reserve (symbol-function 'mission--reserve-inference)))
       (check :wrapper-restored
        (eq account (symbol-function 'mission--account-inference)))
       (check :settler-restored
        (eq settle (symbol-function 'mission--settle-inference-counters)))
       (check :policy-restored
        (eq policy (symbol-function 'mission--settlement-policy)))
       (check :writer-restored (eq writer (symbol-function 'log-append))))))
  nil)


;;;; -- Retired Reporting Owners --

(defparameter *mission-accounting-retirement-index*
  nil
  "Private callback index for the finite reporting-owner fixture.")

(-> test-mission-accounting-retired-retry-overlap nil null)
(defun test-mission-accounting-retired-retry-overlap ()
  "Retire actual scopes while two immutable reports survive serialized original-ID retries."
  (mission-exhaustion-test--run
   (lambda (application provider context goal)
     (declare (ignore provider))
     (let* ((conversation        (application-conversation application))
            (generation          (mission-context-accounting-generation context))
            (reserve             (symbol-function 'mission--reserve-inference))
            (writer              (symbol-function 'conversation-append-record))
            (guardian            (symbol-function 'execution-lease--guardian-run))
            (request-lock        (symbol-function 'mission-context-request-lock))
            (policy              (symbol-function 'mission--settlement-policy))
            (fault
             (make-condition 'mission-error :reason ':fixture-retained-write
                             :message               "Reached original retained write"))
            (requests            (make-array 3 :initial-element nil))
            (receipts            (make-array 3 :initial-element nil))
            (payloads            (make-array 2 :initial-element nil))
            (physical            (make-array 2 :initial-element nil))
            (allowances
             (vector
              (list :id "retired-known" :turn-limit 8 :token-limit 31
                    :turns-used 0 :tokens-used 0)
              (list :id "retired-unknown" :turn-limit 9 :token-limit 47
                    :turns-used 0 :tokens-used 0)))
            (entered             (bordeaux-threads:make-semaphore :count 0))
            (releases
             (vector (bordeaux-threads:make-semaphore :count 0)
                     (bordeaux-threads:make-semaphore :count 0)
                     (bordeaux-threads:make-semaphore :count 0)))
            (retired             (bordeaux-threads:make-semaphore :count 0))
            (write-entered       (bordeaux-threads:make-semaphore :count 0))
            (write-release       (bordeaux-threads:make-semaphore :count 0))
            (second-entered      (bordeaux-threads:make-semaphore :count 0))
            (threads             nil)
            (successor           nil)
            (successor-state     nil)
            (old-state           nil)
            (old-opening         nil)
            (policy-count        0)
            (reserve-count       0)
            (body-count          0)
            (attempts            0)
            (writes              0)
            (armed-p             t)
            (held-p              nil)
            (second-probed-p     nil)
            (first-id            nil)
            (second-id           nil)
            (discovered          nil)
            (retry-second-thread nil)
            (outcomes
             (loop for name in '(:driver :reserve :body0 :body1 :body2 :usage0
                                 :usage1 :usage2 :wrapper0 :wrapper1 :wrapper2
                                 :write0 :write1 :guardian :attach :record
                                 :find :report :retry0 :retry-duplicate :retry1
                                 :prelock :load :map :admit :policy
                                 :map-callback)
                   collect (cons name
                                 (vector 0 0 nil nil nil nil 0
                                         sb-thread:*current-thread* nil nil)))))
       (labels ((observe (name function)
                  (let ((cell (rest (assoc name outcomes))))
                    (setf (aref cell 8) sb-thread:*current-thread*)
                    (mission-accounting-test--observe cell function)))

                (wait-for (semaphore)
                  (unless
                      (bordeaux-threads:wait-on-semaphore semaphore :timeout
                                                          10)
                    (error 'mission-error :reason ':fixture-event :message
                           "Retained reporting event expired")))

                (start (name function)
                  (let ((thread
                         (make-thread
                          (lambda ()
                            (handler-case (observe name function)
                                          (serious-condition (condition)
                                           (setf (aref
                                                  (rest (assoc name outcomes))
                                                  9)
                                                 condition)))
                            t)
                          :name (symbol-name name))))
                    (push thread threads)
                    thread))

                (join (thread)
                  (test-assert
                   (eq t
                       (sb-thread:join-thread thread :timeout 10 :default
                                              ':unfinished))
                   "Exact owned reporting thread completed"))

                (facts (title values)
                  (format t "~&H663-FACTS ~s~%"
                          (list :title title :facts values))
                  (dolist (value values)
                    (test-assert (second value)
                     (format nil "~a: ~a" title (first value))))))
         (push
          (list :branch ':retired-overlap :target fault :outcomes outcomes
                :requests requests :receipts receipts)
          *mission-accounting-test-outcomes*)
         (test-call-with-function-replacements
          (list
           (list 'mission--reserve-inference
                 (lambda (owner &rest arguments)
                   (observe :reserve
                    (lambda ()
                      (let ((request (apply reserve owner arguments)))
                        (when *mission-accounting-retirement-index*
                          (setf (aref requests
                                      *mission-accounting-retirement-index*)
                                request)
                          (incf reserve-count))
                        request)))))
           (list 'mission--settlement-policy
                 (lambda (&rest arguments)
                   (observe :policy
                    (lambda () (incf policy-count) (apply policy arguments)))))
           (list 'execution-lease--guardian-run
                 (lambda (ledger)
                   (if (eq ledger (mission-context-lease-ledger context))
                       (progn
                        (observe :guardian
                         (lambda () (funcall guardian ledger)))
                        (bordeaux-threads:signal-semaphore retired))
                       (funcall guardian ledger))))
           (list 'mission-context-request-lock
                 (lambda (owner)
                   (let ((lock (funcall request-lock owner)))
                     (when
                         (and (eq owner context)
                              (eq sb-thread:*current-thread*
                                  retry-second-thread)
                              (not second-probed-p))
                       (setf second-probed-p t)
                       (observe :prelock (lambda () lock))
                       (bordeaux-threads:signal-semaphore second-entered))
                     lock)))
           (list 'conversation-append-record
                 (lambda (target record)
                   (if (and (eq target conversation)
                            (eq (first record) ':mission-accounting-settlement)
                            (equal generation
                                   (getf (rest record) :generation)))
                       (let* ((identifier (getf (rest record) :request-id))
                              (index
                               (position identifier requests :end 2 :test
                                         #'equal :key
                                         (lambda (request)
                                           (and request
                                                (mission-inference-request-identifier
                                                 request))))))
                         (incf attempts)
                         (observe
                          (if (eql index 0)
                              ':write0
                              ':write1)
                          (lambda ()
                            (when armed-p
                              (setf (aref payloads index) (copy-tree record))
                              (error fault))
                            (when held-p
                              (setf held-p nil)
                              (bordeaux-threads:signal-semaphore write-entered)
                              (wait-for write-release))
                            (let ((result (funcall writer target record)))
                              (incf writes)
                              (setf (aref physical index) (copy-tree result))
                              result))))
                       (funcall writer target record)))))
          (lambda ()
            (unwind-protect
                (observe :driver
                 (lambda ()
                   (dotimes (index 2)
                     (let ((index index))
                       (start
                        (if (zerop index)
                            ':wrapper0
                            ':wrapper1)
                        (lambda ()
                          (let ((*mission-accounting-retirement-index* index))
                            (mission--account-inference context
                                                        (lambda ()
                                                          (observe
                                                           (if (zerop index)
                                                               ':body0
                                                               ':body1)
                                                           (lambda ()
                                                             (incf body-count)
                                                             (setf (aref
                                                                    receipts
                                                                    index)
                                                                   *execution-lease-receipt*)
                                                             (mission-test--result
                                                              "Local retained reporting result"
                                                              :usage
                                                              (json-object
                                                               "input_tokens" 2
                                                               "output_tokens"
                                                               3)))))
                                                        (lambda (results)
                                                          (observe
                                                           (if (zerop index)
                                                               ':usage0
                                                               ':usage1)
                                                           (lambda ()
                                                             (bordeaux-threads:signal-semaphore
                                                              entered)
                                                             (wait-for
                                                              (aref releases
                                                                    index))
                                                             (when
                                                                 (zerop index)
                                                               (provider-result-usage
                                                                (first
                                                                 results))))))
                                                        :allowance
                                                        (aref allowances
                                                              index)))))))
                   (dotimes (index 2)
                     (declare (ignorable index))
                     (wait-for entered))
                   (facts "Original live settlement holds"
                    (list
                     (list "two real requests"
                           (= 2
                              (hash-table-count
                               (mission-context-accounting-requests context))))
                     (list "terminal scopes retain accounting"
                           (every
                            (lambda (receipt)
                              (and receipt
                                   (execution-lease-receipt-terminal receipt)
                                   (plusp
                                    (execution-lease-receipt-settlements
                                     receipt))))
                            (subseq receipts 0 2)))
                     (list "ordinary candidate pin absent"
                           (and (null (mission-context-origin-owner context))
                                (null
                                 (mission-context-candidate-ticket
                                  context))))))
                   (setf successor
                           (observe :attach
                            (lambda () (mission--attach application))))
                   (observe :record (lambda () (mission--record successor)))
                   (start :wrapper2
                    (lambda ()
                      (let ((*mission-accounting-retirement-index* 2))
                        (mission--account-inference successor
                                                    (lambda ()
                                                      (observe :body2
                                                       (lambda ()
                                                         (incf body-count)
                                                         (setf (aref receipts
                                                                     2)
                                                               *execution-lease-receipt*)
                                                         (mission-test--result
                                                          "Local retained reporting result"
                                                          :usage
                                                          (json-object
                                                           "input_tokens" 2
                                                           "output_tokens"
                                                           3)))))
                                                    (lambda (results)
                                                      (observe :usage2
                                                       (lambda ()
                                                         (bordeaux-threads:signal-semaphore
                                                          entered)
                                                         (wait-for
                                                          (aref releases 2))
                                                         (provider-result-usage
                                                          (first
                                                           results)))))))))
                   (wait-for entered)
                   (setf successor-state (copy-tree goal))
                   (bordeaux-threads:signal-semaphore (aref releases 0))
                   (bordeaux-threads:signal-semaphore (aref releases 1))
                   (dolist (thread (rest threads)) (join thread))
                   (wait-for retired)
                   (let ((ledger (mission-context-lease-ledger context)))
                     (facts "Actual guardian retirement before recovery"
                      (list
                       (list "no old receipts"
                             (null (execution-lease-receipts ledger)))
                       (list "no old guardian"
                             (null (execution-lease-guardian ledger)))
                       (list "no old listeners"
                             (null (execution-lease-listeners ledger)))
                       (list "all settlement references released"
                             (every
                              (lambda (receipt)
                                (zerop
                                 (execution-lease-receipt-settlements
                                  receipt)))
                              (subseq receipts 0 2)))
                       (list "successor context current"
                             (eq successor
                                 (mission-context-find conversation))))))
                   (let ((reason
                          (observe :admit
                           (lambda ()
                             (mission-exhaustion-test--admit context)))))
                     (facts "Retirement does not restore execution authority"
                      (list (list "old admission refused" reason))))
                   (setf discovered
                           (observe :find
                            (lambda ()
                              (mission-inference-controller-find generation))))
                   (let* ((report
                           (observe :report
                            (lambda ()
                              (mission-inference-request-report discovered))))
                          (rows (getf report :requests)))
                     (facts "Both original reports survive ordinary retirement"
                      (list (list "original owner" (eq context discovered))
                            (list "two reports"
                                  (= 2 (getf report :unresolved-count)))
                            (list "both uncertain"
                                  (every
                                   (lambda (row)
                                     (getf row :append-uncertain-p))
                                   rows))
                            (list "distinct first payloads"
                                  (not
                                   (equal (aref payloads 0)
                                          (aref payloads 1))))))
                     (setf first-id
                             (getf
                              (find ':known rows :key
                                    (lambda (row) (getf row :classification)))
                              :request-id)
                           second-id
                             (getf
                              (find ':unknown rows :key
                                    (lambda (row) (getf row :classification)))
                              :request-id)))
                   (progn
                    (progn
                     (facts
                      "Exact original identity and detached allowance evidence"
                      (list
                       (list "distinct original allocations"
                             (and
                              (plusp
                               (mission-inference-request-amount
                                (aref requests 0)))
                              (plusp
                               (mission-inference-request-amount
                                (aref requests 1)))
                              (/=
                               (mission-inference-request-amount
                                (aref requests 0))
                               (mission-inference-request-amount
                                (aref requests 1)))))
                       (list "same operational goal identity"
                             (eq goal (mission-context-goal context)))
                       (list "successor positive old-pending opening"
                             (= 2
                                (getf
                                 (getf
                                  (mission-context-accounting-opening
                                   successor)
                                  :old-pending)
                                 :requests-outstanding)))))
                     (dotimes (index 2)
                       (let* ((request  (aref requests index))
                              (payload  (aref payloads index))
                              (evidence (getf (rest payload) :allowance))
                              (route
                               (gethash (aref allowances index)
                                        (mission-context-accounting-allowances
                                         context)))
                              (name
                               (if (zerop index)
                                   ':wrapper0
                                   ':wrapper1))
                              (body
                               (rest
                                (assoc
                                 (if (zerop index)
                                     ':body0
                                     ':body1)
                                 outcomes)))
                              (usage
                               (rest
                                (assoc
                                 (if (zerop index)
                                     ':usage0
                                     ':usage1)
                                 outcomes)))
                              (wrapper  (rest (assoc name outcomes)))
                              (write
                               (rest
                                (assoc
                                 (if (zerop index)
                                     ':write0
                                     ':write1)
                                 outcomes))))
                         (facts "Actual original application and handoff"
                          (list
                           (list "original context EQ"
                                 (eq context
                                     (mission-inference-request-context
                                      request)))
                           (list "original generation"
                                 (equal generation
                                        (mission-inference-request-generation
                                         request)))
                           (list "original conversation EQ"
                                 (eq conversation
                                     (mission-inference-request-conversation
                                      request)))
                           (list "original allowance EQ"
                                 (eq (aref allowances index)
                                     (mission-inference-request-allowance
                                      request)))
                           (list "allowance route detached"
                                 (not (eq route (aref allowances index))))
                           (list "nonempty original allowance evidence"
                                 (and evidence
                                      (equal (getf evidence :id)
                                             (getf (aref allowances index)
                                                   :id))))
                           (list "real body completed once"
                                 (= 1 (aref body 0) (aref body 1)))
                           (list "real usage completed once"
                                 (= 1 (aref usage 0) (aref usage 1)))
                           (list "same actual wrapper body usage thread"
                                 (and (eq (aref wrapper 8) (aref body 8))
                                      (eq (aref wrapper 8) (aref usage 8))
                                      (not
                                       (eq (aref wrapper 8)
                                           sb-thread:*current-thread*))))
                           (list "exact reached writer fault"
                                 (eq fault (aref write 3)))
                           (list "wrapper exact escaping fault"
                                 (eq fault (aref wrapper 9)))
                           (list "one escaped wrapper and no normal return"
                                 (and (= 1 (aref wrapper 0))
                                      (zerop (aref wrapper 1))
                                      (aref wrapper 5)))
                           (list "unpersisted audit owner"
                                 (and
                                  (eq ':unpersisted
                                      (mission-inference-request-state
                                       request))
                                  (eq ':audit-append
                                      (mission-inference-request-persistence-phase
                                       request))
                                  (eq ':original-generation-controller
                                      (mission-inference-request-reporting-owner
                                       request)))))))))
                    (setf old-state
                            (copy-tree
                             (mission--accounting-values
                              (mission--accounting-goal context)))
                          old-opening
                            (copy-tree
                             (mission-context-accounting-opening context))
                          armed-p nil
                          held-p t))
                   (let* ((counts (list reserve-count body-count policy-count))
                          (first
                           (start :retry0
                            (lambda ()
                              (mission-inference-request-retry discovered
                               first-id)))))
                     (wait-for write-entered)
                     (let ((start-second
                            (bordeaux-threads:make-semaphore :count 0)))
                       (setf retry-second-thread
                               (start :retry-duplicate
                                (lambda ()
                                  (wait-for start-second)
                                  (mission-inference-request-retry discovered
                                   first-id))))
                       (bordeaux-threads:signal-semaphore start-second)
                       (wait-for second-entered)
                       (bordeaux-threads:signal-semaphore write-release)
                       (join first)
                       (join retry-second-thread))
                     (facts "Reached overlapping same-ID product retries"
                      (list (list "prelock reached" second-probed-p)
                            (list "one successful physical audit" (= 1 writes))
                            (list "first returned completion and uncertainty"
                                  (equal '(t t)
                                         (aref (rest (assoc ':retry0 outcomes))
                                               2)))
                            (list "duplicate exact typed late refusal"
                                  (let ((condition
                                         (aref
                                          (rest
                                           (assoc ':retry-duplicate outcomes))
                                          3)))
                                    (and (typep condition 'mission-error)
                                         (eq ':accounting-identity
                                             (mission-error-reason
                                              condition)))))
                            (list "no repeated reserve/provider/policy"
                                  (equal counts
                                         (list reserve-count body-count
                                               policy-count)))
                            (list "unchanged original arithmetic"
                                  (equal old-state
                                         (mission--accounting-values
                                          (mission--accounting-goal context))))
                            (list "unchanged successor"
                                  (equal successor-state goal))))
                     (let ((report
                            (mission-inference-request-report
                             (mission-inference-controller-find generation))))
                       (facts "First release preserves second reporting owner"
                        (list
                         (list "one unresolved"
                               (= 1 (getf report :unresolved-count)))
                         (list "exact remaining ID"
                               (equal second-id
                                      (getf (first (getf report :requests))
                                            :request-id)))
                         (list "same original owner"
                               (eq context
                                   (mission-inference-controller-find
                                    generation)))
                         (list "other payload unchanged"
                               (equal (aref payloads 1)
                                      (mission-inference-request-payload
                                       (aref requests 1)))))))
                     (observe :retry1
                      (lambda ()
                        (mission-inference-request-retry discovered
                         second-id)))
                     (facts
                      "Last payload releases only original reporting ownership"
                      (list (list "two writes" (= 2 writes))
                            (list "four reached write attempts" (= 4 attempts))
                            (list "registry released"
                                  (null
                                   (mission-inference-controller-find
                                    generation)))
                            (list "original report empty"
                                  (null
                                   (mission-inference-request-report context)))
                            (list "original request table empty"
                                  (zerop
                                   (hash-table-count
                                    (mission-context-accounting-requests
                                     context))))
                            (list "once-only accounting and policy"
                                  (and
                                   (equal counts
                                          (list reserve-count body-count
                                                policy-count))
                                   (equal old-state
                                          (mission--accounting-values
                                           (mission--accounting-goal
                                            context)))))
                            (list "opening immutable"
                                  (equal old-opening
                                         (mission-context-accounting-opening
                                          context)))
                            (list "successor pending request preserved"
                                  (and
                                   (eq successor
                                       (mission-context-find conversation))
                                   (equal successor-state goal)
                                   (eq ':pending
                                       (mission-inference-request-state
                                        (aref requests 2)))))
                            (list "no candidate pin manufactured"
                                  (and
                                   (null
                                    (mission-context-origin-owner context))
                                   (null
                                    (mission-context-candidate-ticket
                                     context)))))))
                   (let* ((loaded
                           (observe :load
                            (lambda ()
                              (conversation-load
                               (conversation-pathname conversation)))))
                          (records nil))
                     (observe :map
                      (lambda ()
                        (conversation-map-records loaded
                                                  (lambda (record)
                                                    (observe :map-callback
                                                     (lambda ()
                                                       (push record
                                                             records)))))))
                     (dotimes (index 2)
                       (let* ((payload    (aref payloads index))
                              (identifier (getf (rest payload) :request-id))
                              (audits
                               (remove-if-not
                                (lambda (record)
                                  (and
                                   (eq (first record)
                                       ':mission-accounting-settlement)
                                   (equal identifier
                                          (getf (rest record) :request-id))))
                                records))
                              (decoded    (copy-tree (first audits))))
                         (remf (rest decoded) :seq)
                         (remf (rest decoded) :time)
                         (facts
                          "Fresh reader retains immutable original payload"
                          (list
                           (list "one real record for original ID"
                                 (= 1 (length audits)))
                           (list "actual append return reproduced"
                                 (equal (first audits) (aref physical index)))
                           (list "full immutable payload"
                                 (equal payload decoded))
                           (list "allowance evidence retained"
                                 (equal (getf (rest decoded) :allowance)
                                        (getf (rest payload) :allowance)))
                           (list "payload unchanged after retry"
                                 (equal payload
                                        (mission-inference-request-payload
                                         (aref requests index))))
                           (list "uncertainty retained"
                                 (mission-inference-request-append-uncertain-p
                                  (aref requests index)))
                           (list "detached allowance arithmetic unchanged"
                                 (equal
                                  (getf (getf (rest payload) :allowance)
                                        :snapshot)
                                  (mission--accounting-values
                                   (gethash (aref allowances index)
                                            (mission-context-accounting-allowances
                                             context)))))))))
                     (let ((latest (find ':goal records :key #'first)))
                       (facts
                        "Fresh durable successor retains conservative opening"
                        (list (list "real successor goal record exists" latest)
                              (list "positive old-pending two"
                                    (= 2
                                       (getf
                                        (getf
                                         (getf (rest latest)
                                               :accounting-opening)
                                         :old-pending)
                                        :requests-outstanding)))
                              (list "successor opening exact"
                                    (equal
                                     (getf (rest latest) :accounting-opening)
                                     (mission-context-accounting-opening
                                      successor)))
                              (list "successor pending unchanged"
                                    (=
                                     (getf successor-state
                                           :requests-outstanding)
                                     (getf (rest latest)
                                           :requests-outstanding)))))))
                   (facts "Actual known and unbounded UNKNOWN semantics"
                    (list
                     (list "known five tokens"
                           (= 5
                              (getf (mission--accounting-goal context)
                                    :tokens-used)))
                     (list "one unbounded unknown"
                           (= 1
                              (getf (mission--accounting-goal context)
                                    :unknown-usage)))
                     (list "no original outstanding"
                           (zerop
                            (getf (mission--accounting-goal context)
                                  :requests-outstanding)))
                     (list "no original reserved"
                           (zerop
                            (getf (mission--accounting-goal context)
                                  :tokens-reserved)))))
                   (bordeaux-threads:signal-semaphore (aref releases 2))
                   (join (third (reverse threads)))))
              (setf armed-p nil
                    held-p nil)
              (bordeaux-threads:signal-semaphore write-release)
              (dotimes (index 3)
                (bordeaux-threads:signal-semaphore (aref releases index)))
              (dolist (thread threads)
                (sb-thread:join-thread thread :timeout 10 :default
                                       ':unfinished))
              (dolist (entry outcomes)
                (mission-accounting-test--diagnostic ':retired-overlap
                 (first entry) (rest entry) :target fault)))))
         (facts "Exact function restoration"
          (list
           (list "writer"
                 (eq writer (symbol-function 'conversation-append-record)))
           (list "request accessor"
                 (eq request-lock
                     (symbol-function 'mission-context-request-lock)))
           (list "guardian"
                 (eq guardian
                     (symbol-function 'execution-lease--guardian-run)))))
         (dolist
             (name
              '(:driver :body0 :body1 :body2 :usage0 :usage1 :usage2 :attach
                :record :find :report :retry0 :retry1 :prelock :load :map))
           (let ((cell (rest (assoc name outcomes))))
             (facts "Completed named operation on its actual thread"
              (list
               (list (symbol-name name)
                     (and (= 1 (aref cell 0) (aref cell 1))
                          (null (aref cell 5))
                          (typep (aref cell 8) 'sb-thread:thread)))))))))))
  nil)

(defparameter *mission-child-fault-test-evidence*
  nil
  "Private raw observations, never serialized condition objects.")

(-> mission-child-fault-test--run (keyword) null)
(defun mission-child-fault-test--run (branch)
  "Exercise a reached secondary writer through the actual built-in child fixture."
  (let* ((target-shape      nil)
         (fresh-count       0)
         (terminal-evidence nil)
         (transported       nil)
         (context           nil)
         (trace             nil)
         (root              nil)
         (child             nil)
         (refusal           nil)
         (fault
          (make-condition 'mission-error :reason ':fixture-secondary :message
                          "Controlled reporting fault"))
         (target-count      0)
         (disk              nil)
         (checkpoint        nil)
         (live-secondary    nil)
         (cells
          (loop for name in '(:driver :evaluate :request :provider :refuse
                              :checkpoint :child :boundary :trace :goal :notice
                              :settle :policy :close :load :map :boundary-body
                              :terminal)
                collect (cons name
                              (vector 0 0 nil nil nil nil 0
                                      sb-thread:*current-thread* nil nil))))
         (symbols
          '(mission-exhaustion--evaluate mission-exhaustion--request
                                         provider-stream-turn
                                         execution-lease-refuse
                                         mission-lease-checkpoint
                                         task-run-child
                                         task-call-with-review-boundary
                                         log-append
                                         mission--present-terminal-notice
                                         mission--settle-inference
                                         mission--settlement-policy
                                         task-orchestrator-close))
         (originals         (mapcar (lambda (s) (cons s (symbol-function s))) symbols)))
    (labels ((original (s)
               (rest (assoc s originals)))

             (observe (name function)
               (let ((cell (rest (assoc name cells))))
                 (setf (aref cell 8) sb-thread:*current-thread*)
                 (mission-accounting-test--observe cell function)))

             (read-disk ()
               (when (and context trace child (not disk))
                 (progn
                  (let* ((job
                          (mission-lease-test-state-child-job
                           *mission-lease-test-state*))
                         (snapshot
                          (observe :terminal
                           (lambda () (job-await job :timeout-seconds 0))))
                         (result (getf snapshot :result)))
                    (setf terminal-evidence
                            (list :physical (getf snapshot :state)
                                  :cancellation
                                  (getf snapshot :cancellation-reason) :status
                                  (getf result :status) :outcome
                                  (getf result :outcome) :execution-state
                                  (getf result :execution-state) :yielded-p
                                  (getf result :yielded-p)
                                  :administrative-checkpoint
                                  (copy-tree
                                   (getf result :administrative-checkpoint)))))
                  (setf live-secondary
                          (copy-list
                           (getf
                            (mission-context-evaluation-observation context)
                            :secondary-faults))))
                 (dolist
                     (entry
                      (list (cons :root root) (cons :trace trace)
                            (cons :child (agent-conversation child))))
                   (let ((records nil)
                         (loaded
                          (observe :load
                           (lambda ()
                             (conversation-load
                              (conversation-pathname (rest entry)))))))
                     (observe :map
                      (lambda ()
                        (conversation-map-records loaded
                                                  (lambda (record)
                                                    (push record records)))))
                     (progn
                      (unless (eq loaded (rest entry)) (incf fresh-count))
                      (push (cons (first entry) (nreverse records)) disk)))))))
      (unwind-protect
          (test-call-with-function-replacements
           (list
            (list 'mission-exhaustion--evaluate
                  (lambda (ctx)
                    (setf context ctx
                          root
                            (application-conversation
                             (mission-context-application ctx)))
                    (observe :evaluate
                     (lambda ()
                       (funcall (original 'mission-exhaustion--evaluate)
                                ctx)))))
            (list 'mission-exhaustion--request
                  (lambda (ctx conversation &rest keys)
                    (setf trace conversation)
                    (observe :request
                     (lambda ()
                       (apply (original 'mission-exhaustion--request) ctx
                              conversation keys)))))
            (list 'provider-stream-turn
                  (lambda (provider conversation &rest keys)
                    (if (and trace (eq trace conversation))
                        (observe :provider
                         (lambda ()
                           (apply (original 'provider-stream-turn) provider
                                  conversation keys)))
                        (apply (original 'provider-stream-turn) provider
                               conversation keys))))
            (list 'execution-lease-refuse
                  (lambda (&rest args)
                    (observe :refuse
                     (lambda ()
                       (apply (original 'execution-lease-refuse) args)))))
            (list 'mission-lease-checkpoint
                  (lambda (job conversation condition)
                    (setf refusal condition
                          child
                            (mission-lease-test-state-child
                             *mission-lease-test-state*)
                          checkpoint
                            (list :cause-eq
                                  (eq (mission-error-cause condition)
                                      (getf
                                       (mission-context-evaluation-observation
                                        context)
                                       :cause))
                                  :context-eq
                                  (eq context
                                      (mission-review-required-context
                                       condition))
                                  :job-context-eq
                                  (eq context (task-job-mission-context job))
                                  :generation
                                  (mission-review-required-generation
                                   condition)
                                  :attempt
                                  (mission-review-required-attempt condition)
                                  :current
                                  (task-checkpoint-current-p context
                                   (mission-review-required-generation
                                    condition)
                                   (mission-review-required-attempt condition))
                                  :lineage
                                  (task-checkpoint-lineage-current-p context
                                   job nil)
                                  :job-id         (session-job-identifier job)
                                  :execution-id
                                  (session-job-execution-identifier job) :cap
                                  (job-deadline job) :run-token
                                  (job-run-token job) :owners
                                  (copy-list (job-owner-identifiers job))
                                  :transported-eq (eq condition transported)))
                    (observe :checkpoint
                     (lambda ()
                       (funcall (original 'mission-lease-checkpoint) job
                                conversation condition)))))
            (list 'task-run-child
                  (lambda (&rest args)
                    (observe :child
                     (lambda () (apply (original 'task-run-child) args)))))
            (list 'task-call-with-review-boundary
                  (lambda (job conversation function)
                    (observe :boundary
                     (lambda ()
                       (funcall (original 'task-call-with-review-boundary) job
                                conversation
                                (lambda ()
                                  (handler-bind ((mission-review-required
                                                  (lambda (c)
                                                    (setf transported c))))
                                    (observe :boundary-body function))))))))
            (list 'log-append
                  (lambda (path record &rest keys)
                    (let ((target
                           (and context (zerop target-count)
                                (case branch
                                  (:evaluation-trace
                                   (and trace
                                        (equal path
                                               (conversation-log-pathname
                                                trace))
                                        (eq (first record)
                                            ':exhaustion-evaluation)))
                                  (:goal-record
                                   (and
                                    (equal path
                                           (conversation-log-pathname root))
                                    (eq (first record) ':goal)
                                    (getf (getf (rest record) :exhaustion)
                                          :review-required)))))))
                      (if target
                          (progn
                           (progn
                            (setf target-shape
                                    (if (eq branch :terminal-notice)
                                        (list :kind :terminal-notice :attempt
                                              (getf checkpoint :attempt))
                                        (list :kind (first record) :sequence
                                              (getf (rest record) :seq) :time
                                              (getf (rest record) :time)
                                              :latched-p
                                              (not
                                               (null
                                                (getf
                                                 (getf
                                                  (mission-context-goal
                                                   context)
                                                  :exhaustion)
                                                 :review-required)))
                                              :diagnostic
                                              (copy-tree
                                               (getf
                                                (getf (rest record)
                                                      :evaluation)
                                                :diagnostic)))))
                            (incf target-count))
                           (observe
                            (if (eq branch ':goal-record)
                                :goal
                                :trace)
                            (lambda () (error fault))))
                          (apply (original 'log-append) path record keys)))))
            (list 'mission--present-terminal-notice
                  (lambda (ctx status text)
                    (if (and (eq branch ':terminal-notice) (eq ctx context)
                             (eq status ':review-required)
                             (zerop target-count))
                        (progn
                         (progn
                           (setf target-shape
                                 (list :kind :terminal-notice :attempt
                                       (getf checkpoint :attempt)))
                          (incf target-count))
                         (observe :notice (lambda () (error fault))))
                        (funcall (original 'mission--present-terminal-notice)
                                 ctx status text))))
            (list 'mission--settle-inference
                  (lambda (&rest args)
                    (observe :settle
                     (lambda ()
                       (apply (original 'mission--settle-inference) args)))))
            (list 'mission--settlement-policy
                  (lambda (&rest args)
                    (observe :policy
                     (lambda ()
                       (apply (original 'mission--settlement-policy) args)))))
            (list 'task-orchestrator-close
                  (lambda (&rest args)
                    (read-disk)
                    (observe :close
                     (lambda ()
                       (apply (original 'task-orchestrator-close) args))))))
           (lambda ()
             (observe :driver
              (lambda () (mission-lease-test--real-child ':normal)))))
        (setf *mission-child-fault-test-evidence*
                (list :branch branch :cells cells :context context :refusal
                      refusal :fault fault :disk disk :checkpoint checkpoint))
        (let* ((observation
                (and context (mission-context-evaluation-observation context)))
               (diagnostic
                (and observation
                     (mission-exhaustion--diagnostic observation
                      (getf (getf (mission-context-goal context) :exhaustion)
                            :evaluator))))
               (categorical
                (list :branch branch :target-count target-count :checkpoint
                      checkpoint :diagnostic diagnostic :live-secondary
                      live-secondary :outcomes
                      (loop for (name . cell) in cells
                            collect (list :name name :entered (aref cell 0)
                                          :completed (aref cell 1) :signals
                                          (aref cell 6) :escaped (aref cell 5)
                                          :main-thread (aref cell 4) :condition
                                          (and (aref cell 3)
                                               (type-of (aref cell 3)))
                                          :fault-eq (eq fault (aref cell 3))))
                      :disk
                      (loop for (kind . records) in disk
                            collect (list :store kind :records
                                          (remove-if-not
                                           (lambda (r)
                                             (member (first r)
                                                     '(:exhaustion-evaluation
                                                       :mission-review-checkpoint
                                                       :mission-review-evidence
                                                       :goal)))
                                           records)))
                      :terminal terminal-evidence :fresh-count fresh-count
                      :target-shape target-shape)))
          (format t "~&E665 ~s~%" categorical)))
      (test-assert (= target-count 1) "Reached exactly one intended writer")
      (test-assert
       (every (lambda (key) (getf checkpoint key))
              '(:cause-eq :context-eq :job-context-eq :current :lineage))
       "Actual child received its real refusal with original evaluator cause and lineage")
      (test-assert (member branch live-secondary)
       "Secondary category retained live")
      (test-assert
       (some
        (lambda (entry)
          (some
           (lambda (record)
             (let* ((evidence
                     (case (first record)
                       (:mission-review-checkpoint
                        (getf (rest record) :evidence))
                       (:exhaustion-evaluation
                        (getf (getf (rest record) :evaluation) :diagnostic))))
                    (categories (getf evidence :secondary)))
               (member branch categories)))
           (rest entry)))
        disk)
       "Independent new disk load retains the reached secondary reporting category")
      (let* ((target-name
              (case branch
                (:evaluation-trace
                 :trace)
                (:goal-record
                 :goal)
                (t
                 :notice)))
             (target   (rest (assoc target-name cells)))
             (provider (rest (assoc :provider cells)))
             (records  (rest (assoc :child disk)))
             (record   (find :mission-review-checkpoint records :key #'first))
             (evidence (getf (rest record) :evidence))
             (expected
              (mission-exhaustion--diagnostic
               (mission-context-evaluation-observation context)
               (getf (getf (mission-context-goal context) :exhaustion)
                     :evaluator)))
             (data     (getf terminal-evidence :administrative-checkpoint)))
        (test-assert
         (and (= (aref target 0) 1) (zerop (aref target 1)) (aref target 5)
              (eq (aref target 3) fault))
         "Reached raw secondary writer fault is distinct and escaped its writer")
        (test-assert
         (and refusal (eq (mission-error-cause refusal) (aref provider 3))
              (not (eq fault refusal))
              (not (eq fault (mission-error-cause refusal))))
         "Live original evaluator Q is the actual transported refusal R cause, not S")
        (test-assert (getf checkpoint :transported-eq)
         "The real child boundary transported this exact refusal")
        (test-assert (= fresh-count 3)
         "All three mapped conversations are new disk loads")
        (test-assert
         (and (eq (getf terminal-evidence :physical) :aborted)
              (eq (getf terminal-evidence :status) :suspended)
              (eq (getf terminal-evidence :outcome) :review-required)
              (eq (getf terminal-evidence :execution-state) :aborted)
              (not (getf terminal-evidence :yielded-p)))
         "Actual physical abort and administrative suspension do not imply yield")
        (test-assert
         (and record (equal evidence expected)
              (member branch (getf evidence :secondary)))
         "Child checkpoint retains immutable original categorical evidence including S")
        (dolist (key '(:job-id :generation :attempt))
          (test-assert (equal (getf (rest record) key) (getf checkpoint key))
           (format nil "Disk checkpoint preserves original ~a" key)))
        (test-assert
         (and data (equal (getf data :cursor) (getf (rest record) :cursor))
              (eql (getf (getf data :cursor) :sequence)
                   (getf (rest record) :seq)))
         "Terminal administrative cursor addresses the actual immutable evidence record")
        (test-assert
         (every
          (lambda (entry) (eq (symbol-function (first entry)) (rest entry)))
          originals)
         "Every callable is restored exactly")
        (test-assert
         (and (eq (getf evidence :condition-category) :serious-failure)
              (eq (getf evidence :policy) :evaluate)
              (= (getf evidence :attempt) 1))
         "Disk primary evidence describes the actual original evaluator failure")
        (let ((allowance (getf evidence :allowance-after)))
          (test-assert
           (and (= (getf allowance :turns-used) 1)
                (= (getf allowance :unknown-usage) 1)
                (zerop (getf allowance :tokens-used))
                (zerop (getf allowance :requests-outstanding))
                (zerop (getf allowance :tokens-reserved)))
           "Evaluator dispatch has honest UNKNOWN usage and no retained reservation"))
        (test-assert (null (getf terminal-evidence :cancellation))
         "Administrative abort does not invent an accepted cancellation")
        (dolist (key '(:mission-id :generation :attempt :job-id))
          (test-assert (equal (getf data key) (getf (rest record) key))
           (format nil "Terminal and independently reloaded evidence share ~a"
                   key)))
        (test-assert
         (and (equal (getf data :run-token) (getf checkpoint :run-token))
              (eql (getf data :original-cap) (getf checkpoint :cap))
              (equal (getf data :owners) (getf checkpoint :owners)))
         "Original token, cap and owner lineage survive actual terminal publication")
        (progn))))
  nil)

(-> test-mission-child-trace-fault-evidence nil null)
(defun test-mission-child-trace-fault-evidence ()
  "Retain original child refusal across a reached EVALUATION-TRACE fault and new disk reads."
  (mission-child-fault-test--run ':evaluation-trace))

(-> test-mission-child-goal-fault-evidence nil null)
(defun test-mission-child-goal-fault-evidence ()
  "Retain original child refusal across a reached GOAL-RECORD fault and new disk reads."
  (mission-child-fault-test--run ':goal-record))

(-> test-mission-child-notice-fault-evidence nil null)
(defun test-mission-child-notice-fault-evidence ()
  "Retain original child refusal across a reached TERMINAL-NOTICE fault and new disk reads."
  (mission-child-fault-test--run ':terminal-notice))


;;;; -- Descriptive Acceptance Coverage --

(-> mission-exhaustion-test--coverage-goal () list)
(defun mission-exhaustion-test--coverage-goal ()
  "Supply already-normalized policy data, without claiming normalization ran."
  (list :criteria (list (list :id (copy-seq "criterion-A") :description (copy-seq "First criterion")
                              :gates (list (copy-seq "gate-A")) :evidence nil)
                        (list :id (copy-seq "criterion-B") :description (copy-seq "Second criterion")
                              :gates (list (copy-seq "gate-A") (copy-seq "gate-B")) :evidence nil)
                        (list :id (copy-seq "criterion-manual") :description (copy-seq "Manual criterion")
                              :gates nil :evidence (copy-seq "User evidence")))
        :gates (list (list :id (copy-seq "gate-A") :status :failed :attempts 2 :attempt-limit 3
                          :requires-tool (copy-seq "compiler") :requires-criterion (copy-seq "criterion-manual"))
                     (list :id (copy-seq "gate-B") :status :pending :attempts 0 :attempt-limit 4))
        :evidence (list (list :kind :gate :text (copy-seq "Retained evidence")))
        :tokens-used 7 :turns-used 2 :token-limit 100 :turn-limit 20))

(-> test-mission-exhaustion-coverage-identities () t)
(defun test-mission-exhaustion-coverage-identities ()
  "Keep shared edges, recorded state and mutable ownership separate from the goal."
  (let* ((goal (mission-exhaustion-test--coverage-goal))
         (before (task--character-strings goal))
         (coverage (mission-exhaustion--coverage goal))
         (criteria (getf coverage :criteria))
         (gates (getf coverage :gates)))
    (test-assert (getf coverage :complete-p) "Small coverage is complete")
    (test-assert (and (getf coverage :descriptive-only) (getf coverage :recorded-state-only))
                 "Coverage describes recorded state, not fresh proof")
    (test-assert (zerop (+ (getf coverage :omitted-criteria) (getf coverage :omitted-gates)))
                 "Complete coverage omits no records")
    (loop for original in (getf goal :criteria) for projected in criteria
          do (dolist (key '(:id :description :gates))
               (test-assert (equal (getf original key) (getf projected key)) "Criterion identity and edges survive"))
             (test-assert (eq (getf projected :manual-evidence-present-p)
                              (not (null (getf original :evidence)))) "Only evidence presence is projected")
             (test-assert (not (member :accepted-p projected)) "No accepted proof field is invented"))
    (loop for original in (getf goal :gates) for projected in gates
          do (dolist (key '(:id :attempts :attempt-limit :requires-tool :requires-criterion))
               (test-assert (equal (getf original key) (getf projected key)) "Gate identity and prerequisites survive"))
             (test-assert (eq (getf original :status) (getf projected :recorded-status)) "State is recorded status"))
    (test-assert (equal (mapcar (lambda (gate) (getf gate :recorded-status)) gates) '(:failed :pending))
                 "Failed and pending gates are represented")
    (test-assert (equal before goal) "Projection does not change policy, evidence, accounting or limits")
    (setf (char (getf (first criteria) :id) 0) #\X
          (char (getf (first criteria) :description) 0) #\X
          (char (first (getf (second criteria) :gates)) 0) #\X
          (char (getf (first gates) :id) 0) #\X
          (char (getf (first gates) :requires-tool) 0) #\X
          (char (getf (first gates) :requires-criterion) 0) #\X
          (getf (second gates) :attempt-limit) 99)
    (push "foreign" (getf (first criteria) :gates))
    (test-assert (equal before goal) "Returned strings and lists do not alias goal data")
    (setf (getf goal :criteria) (reverse (getf goal :criteria))
          (getf (first (getf goal :criteria)) :description) "Changed description")
    (let ((reordered (mission-exhaustion--coverage goal)))
      (test-assert (equal (mapcar (lambda (item) (getf item :id)) (getf reordered :criteria))
                          '("criterion-manual" "criterion-B" "criterion-A")) "Order is retained without rebinding IDs")
      (test-assert (equal (getf (second (getf reordered :criteria)) :gates) '("gate-A" "gate-B"))
                   "Description and order changes do not change edges"))))

(-> test-mission-exhaustion-coverage-proof () t)
(defun test-mission-exhaustion-coverage-proof ()
  "Exercise the genuine criterion predicate without claiming full freshness proof."
  (let ((goal (mission-exhaustion-test--coverage-goal)))
    (test-assert (not (mission--criteria-proven-p goal)) "Failed and pending references prevent proof")
    (setf (getf (first (getf goal :gates)) :status) :passed)
    (test-assert (not (mission--criteria-proven-p goal)) "An uncovered referenced gate prevents whole proof")
    (setf (getf (second (getf goal :gates)) :status) :passed)
    (test-assert (mission--criteria-proven-p goal) "Passed gate references prove gated criteria with NIL criterion evidence")
    (test-assert (not (getf (first (getf (mission-exhaustion--coverage goal) :criteria)) :manual-evidence-present-p))
                 "Projection does not fabricate criterion evidence")
    (setf (getf (third (getf goal :criteria)) :evidence) nil)
    (test-assert (not (mission--criteria-proven-p goal)) "Ungated criterion needs explicit evidence")
    (setf (getf (third (getf goal :criteria)) :evidence) "   ")
    (test-assert (not (mission--criteria-proven-p goal)) "Whitespace is not manual proof")
    (setf (getf (third (getf goal :criteria)) :evidence) "User evidence")
    (test-assert (mission--criteria-proven-p goal) "Nonempty manual evidence restores the necessary predicate")))

(-> test-mission-exhaustion-coverage-bounds () t)
(defun test-mission-exhaustion-coverage-bounds ()
  "Fit whole records at exact bounds and disclose huge or numerous omitted records."
  (let* ((goal (mission-exhaustion-test--coverage-goal))
         (coverage (mission-exhaustion--coverage goal))
         (size (length (task--write-readable-sexp coverage))))
    (let ((*mission-evidence-limit* size))
      (test-assert (equal coverage (mission-exhaustion--coverage goal)) "Exact complete character bound fits"))
    (let* ((*mission-evidence-limit* (1- size))
           (partial (mission-exhaustion--coverage goal)))
      (test-assert (not (getf partial :complete-p)) "One character over bound is incomplete")
      (test-assert (plusp (+ (getf partial :omitted-criteria) (getf partial :omitted-gates))) "Omissions are explicit")
      (test-assert (<= (length (task--write-readable-sexp partial)) *mission-evidence-limit*) "Partial envelope fits"))
    (dolist (key '(:id :description))
      (let ((large (mission-exhaustion-test--coverage-goal)))
        (setf (getf (first (getf large :criteria)) key) (make-string 100000 :initial-element #\Q))
        (let ((partial (mission-exhaustion--coverage large)))
          (test-assert (not (getf partial :complete-p)) "Huge source string is omitted, not clipped")
          (test-assert (plusp (getf partial :omitted-criteria)) "Huge record omission is counted")
          (test-assert (not (find "criterion-A" (getf partial :criteria) :test #'equal
                                  :key (lambda (item) (getf item :id)))) "Oversized criterion is not partly represented"))))
    (let* ((unicode (mission-exhaustion-test--coverage-goal))
           (identity (format nil "λ~Cquote~Cslash~C終" #\" #\\ #\Newline)))
      (setf (getf (first (getf unicode :gates)) :id) identity
            (first (getf (first (getf unicode :criteria)) :gates)) identity
            (first (getf (second (getf unicode :criteria)) :gates)) identity)
      (let* ((projected (mission-exhaustion--coverage unicode))
             (text (task--write-readable-sexp projected))
             (roundtrip (let ((*read-eval* nil)) (read-from-string text))))
        (test-assert (equal projected roundtrip) "Unicode and escaping roundtrip through real readable serialization")
        (test-assert (equal identity (getf (first (getf roundtrip :gates)) :id)) "Included Unicode identity is exact")
        (test-assert (= (length text) (task--readable-sexp-character-count projected)) "Real character measurement matches output")))
    (let ((large (mission-exhaustion-test--coverage-goal)))
      (setf (getf large :criteria)
            (loop for index below 1000 collect (list :id (format nil "criterion-~D" index)
                                                     :description "Shared gate"
                                                     :gates (if (zerop index) (list "gate-0")
                                                                (list "gate-0" (format nil "gate-~D" index)))
                                                     :evidence nil)))
      (setf (getf large :gates)
            (loop for index below 1000 collect (list :id (format nil "gate-~D" index)
                                                     :status :pending :attempts 0 :attempt-limit 3)))
      (let ((partial (mission-exhaustion--coverage large)))
        (test-assert (not (getf partial :complete-p)) "Many records cannot claim complete coverage")
        (test-assert (= 1000 (+ (length (getf partial :criteria)) (getf partial :omitted-criteria))) "Every criterion is represented or omitted")
        (test-assert (= 1000 (+ (length (getf partial :gates)) (getf partial :omitted-gates))) "Every gate is represented or omitted")
        (test-assert (<= (length (task--write-readable-sexp partial)) *mission-evidence-limit*) "Many-record output is bounded")
        (dolist (record (getf partial :criteria))
          (test-assert (find (getf record :id) (getf large :criteria) :test #'equal :key (lambda (item) (getf item :id)))
                       "Included identities are original identities")
          (test-assert (equal (getf record :gates)
                              (getf (find (getf record :id) (getf large :criteria) :test #'equal
                                          :key (lambda (item) (getf item :id))) :gates))
                       "Included shared edges are not abbreviated"))))
    (let* ((edges (loop for index below 80 collect (format nil "gate-~D" index)))
           (wide (list :criteria (list (list :id "wide" :description "All configured edges" :gates edges :evidence nil))
                       :gates (loop for id in edges collect (list :id id :status :pending :attempts 0 :attempt-limit 3))))
           (partial (mission-exhaustion--coverage wide)))
      (test-assert (equal edges (getf (first (getf partial :criteria)) :gates)) "More than 64 included edges remain exact")
      (test-assert (not (getf partial :complete-p)) "Missing gate records are disclosed despite retained edges")
      (test-assert (plusp (getf partial :omitted-gates)) "Wide graph omissions are counted"))
    (let* ((empty (mission-exhaustion--coverage nil))
           (*mission-evidence-limit* (length (task--write-readable-sexp empty))))
      (test-assert (getf (mission-exhaustion--coverage nil) :complete-p) "Empty complete envelope fits its exact bound"))
    (let ((*mission-evidence-limit* 1))
      (test-assert (handler-case (progn (mission-exhaustion--coverage goal) nil)
                     (mission-error (condition) (eq (mission-error-reason condition) :evaluator-coverage)))
                   "An envelope that cannot fit refuses truthfully"))))


(-> test-mission-exhaustion-coverage-inputs () t)
(defun test-mission-exhaustion-coverage-inputs ()
  "Inspect actual input data and supported redaction refusal with the local fixture."
  (mission-exhaustion-test--run
   (lambda (application provider context goal)
     (declare (ignore application provider))
     (let* ((policy (mission-exhaustion-test--coverage-goal))
            (progress (getf (getf goal :exhaustion) :progress)))
       (setf (getf goal :criteria) (getf policy :criteria)
             (getf goal :gates) (getf policy :gates))
       (let* ((before (task--character-strings goal))
              (inputs (mission-exhaustion--inputs context))
              (entry (find "COVERAGE" inputs :test #'equal :key (lambda (item) (getf item :id))))
              (coverage (let ((*read-eval* nil)) (read-from-string (getf entry :text)))))
         (test-assert (equal coverage (mission-exhaustion--coverage goal)) "Consumer delivers the descriptive projection")
         (dolist (id '("E1" "E2" "E3" "E4"))
           (test-assert (find id inputs :test #'equal :key (lambda (item) (getf item :id))) "Prior input identities are retained"))
         (test-assert (equal before goal) "Input construction does not mutate the goal")
         (test-assert (eq progress (getf (getf goal :exhaustion) :progress)) "Prior extension history is not replaced"))
       (test-call-with-function-replacements
        (list (list 'redact (lambda (text &key language model-directory runtime-library)
                             (declare (ignore language model-directory runtime-library))
                             (values (if (search "criterion-A" text) "masked" text) :ok))))
        (lambda ()
          (let* ((text (mission-exhaustion--coverage-text goal))
                 (coverage (let ((*read-eval* nil)) (read-from-string text))))
            (test-assert (and (not (getf coverage :complete-p)) (getf coverage :redacted-p))
                         "Changed redaction cannot become complete coverage")
            (test-assert (and (null (getf coverage :criteria)) (null (getf coverage :gates)))
                         "Changed identities are discarded, not advertised as exact")
            (test-assert (= 3 (getf coverage :omitted-criteria)) "Redaction omissions include every criterion"))))
       (test-call-with-function-replacements
        (list (list 'redact (lambda (text &key language model-directory runtime-library)
                             (declare (ignore text language model-directory runtime-library))
                             (values nil :unavailable))))
        (lambda ()
          (test-assert (handler-case (progn (mission-exhaustion--coverage-text goal) nil)
                         (mission-error (condition) (eq (mission-error-reason condition) :evaluator-redaction)))
                       "Local redaction refusal is retained")))))))
