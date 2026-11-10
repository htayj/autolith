(in-package #:autolith)

;;;; -- Next-Request Context Admission --

(defclass context-admission-test-provider (codex-subscription-provider)
  ((last-ceiling
    :initform      nil
    :accessor      context-admission-test-provider-last-ceiling
    :documentation "Effective local output ceiling at the transport boundary.")
   (last-request
    :initform      nil
    :accessor      context-admission-test-provider-last-request
    :documentation "The actual final request seen by the inert transport.")
   (network-calls
    :initform      0
    :accessor      context-admission-test-provider-network-calls
    :documentation "Number of admitted requests reaching the fake transport."))
  (:documentation "A real product projection with an inert observable transport."))

(defmethod provider-open-response-stream
    ((provider context-admission-test-provider) (request hash-table)
     &key credentials conversation)
  "Record an admitted request without credentials or network activity."
  (declare (ignore credentials conversation))
  (setf (context-admission-test-provider-last-request provider) request
        (context-admission-test-provider-last-ceiling provider) *provider-maximum-output-tokens*)
  (incf (context-admission-test-provider-network-calls provider))
  (values nil 200 nil))

(-> test-context-admission-boundary () null)
(defun test-context-admission-boundary ()
  "Reserve explicit output allowances and stop rejected requests before transport."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((provider (make-instance 'context-admission-test-provider
                                     :configuration configuration))
             (request  (json-object "input" (make-string 1000 :initial-element #\x)))
             (window   (config :context-window configuration)))
        (test-assert (= window 5000) "the test controls the shared context window")
        (provider-open-response-stream provider request)
        (test-assert (= 1 (context-admission-test-provider-network-calls provider))
                     "an admitted request reaches transport once")
        (setf (gethash "max_output_tokens" request) 4900)
        (test-assert
         (handler-case
             (progn (provider-open-response-stream provider request) nil)
           (provider-context-budget-error (condition)
             (and (= 4900 (provider-context-budget-error-output-reserve condition))
                  (= window (provider-context-budget-error-context-window condition))
                  (plusp (provider-context-budget-error-estimated-input-tokens condition)))))
         "an explicit generation cap takes precedence over minimum headroom")
        (test-assert (= 1 (context-admission-test-provider-network-calls provider))
                     "rejection occurs before network I/O")
        (dolist (field '("max_tokens" "max_completion_tokens"))
          (let ((capped (json-object field 6000)))
            (test-assert (= 6000 (provider-request-output-reserve provider capped))
                         "Messages and Chat output caps reserve the requested allowance")))
        (test-assert
         (= 7000 (provider-request-output-reserve
                  provider (json-object "generationConfig"
                                        (json-object "maxOutputTokens" 7000))))
         "GenerateContent output caps participate in admission")
        (test-assert
         (null (provider-check-request-context provider (json-object)))
         "a rejected request does not poison a later small request"))))
  nil)

(-> test-context-admission-fresh-projection () null)
(defun test-context-admission-fresh-projection ()
  "New input and tool evidence trigger compaction independently of stale reported usage."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000"))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (provider     (provider-create configuration))
             (agent        (agent-create :configuration configuration :conversation conversation
                                         :provider provider :tool-registry (make-instance 'tool-registry))))
        (test-call-with-function-replacements
         (list (list 'system-prompt (lambda (configuration)
                                     (declare (ignore configuration)) "small prompt"))
               (list 'request-context-session-state
                     (lambda (configuration &key hurry-up-p)
                       (declare (ignore configuration hurry-up-p)) ""))
               (list 'tool-registry-provider-schemas
                     (lambda (registry &key canonical-names)
                       (declare (ignore registry canonical-names)) #())))
         (lambda ()
           (setf (conversation-last-total-tokens conversation) 100000)
           (test-assert (not (agent-should-compact-p agent))
                        "high stale usage does not force compaction of a small current projection")
           (setf (conversation-last-total-tokens conversation) 1)
           (conversation-append-user-message conversation (make-string 20000 :initial-element #\u))
           (test-assert (agent-should-compact-p agent)
                        "new user input crosses the soft threshold before any usage response")
           (conversation-append-summary conversation "small checkpoint")
           (test-assert (not (agent-should-compact-p agent))
                        "the replacement is measured instead of trusting the reset usage counter")
           (conversation-append-provider-item
            conversation (json-object "type" "function_call" "call_id" "fresh-evidence"
                                      "name" "test" "arguments" "{}"))
           (conversation-append-tool-result
            conversation "fresh-evidence" :tool-name "test"
            :output (make-string 20000 :initial-element #\e) :success-p nil)
           (test-assert (agent-should-compact-p agent)
                        "new tool results contribute before the next provider request"))))))
  nil)

(-> test-context-admission-final-request () null)
(defun test-context-admission-final-request ()
  "Measure newly projected instructions, tools and temporary context without rewriting history."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((provider     (make-instance 'context-admission-test-provider
                                         :configuration configuration))
             (conversation (conversation-create configuration))
             (large        (make-string 22000 :initial-element #\c))
             (baseline     (mapcar #'json-encode (conversation-input-items conversation))))
        (dolist (request
                  (list (json-object "instructions" large)
                        (json-object "input" (vector (json-object "role" "developer" "content" large)))
                        (json-object "tools" (vector (json-object "description" large)))))
          (test-assert
           (handler-case
               (progn (provider-open-response-stream provider request :conversation conversation) nil)
             (provider-context-budget-error () t))
           "every prompt-bearing component in the final request participates in admission"))
        (test-assert (zerop (context-admission-test-provider-network-calls provider))
                     "oversized temporary context is never sent")
        (test-assert (equal baseline (mapcar #'json-encode (conversation-input-items conversation)))
                     "admission rejection preserves the original conversation projection")
        (test-assert (zerop (conversation-last-total-tokens conversation))
                     "local estimates are not recorded as paid usage")
        (conversation-append-summary conversation large)
        (test-assert
         (handler-case
             (progn
               (provider-open-response-stream
                provider (json-object "input" (coerce (conversation-input-items conversation) 'vector)))
               nil)
           (provider-context-budget-error () t))
         "an oversized replacement must pass admission again before continuing"))))
  nil)

(-> test-context-admission-images () null)
(defun test-context-admission-images ()
  "Use the image allowance rather than base64 size or short URL length."
  (with-test-configuration (configuration)
    (let ((provider (make-instance 'context-admission-test-provider :configuration configuration)))
      (multiple-value-bind (short uncertain-p)
          (provider-request-input-token-estimate
           provider (json-object "type" "input_image" "image_url" "https://example.invalid/image"))
        (test-assert (and uncertain-p (= short *provider-image-token-estimate*))
                     "image token accounting is explicitly approximate")
        (multiple-value-bind (encoded encoded-uncertain-p)
            (provider-request-input-token-estimate
             provider (json-object "type" "input_image" "image_url"
                                   (concatenate 'string "data:image/png;base64,"
                                                (make-string 100000 :initial-element #\a))))
          (test-assert (and encoded-uncertain-p (= short encoded))
                       "base64 wire size is not interpreted as text tokens")))))
  nil)

(-> test-context-admission-checkpoint-transaction () null)
(defun test-context-admission-checkpoint-transaction ()
  "Reject oversized final checkpoints before changing durable or projected history."
  (dolist (native-p '(nil t))
    (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                            ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
      (with-test-configuration (configuration)
        (let* ((conversation (conversation-create configuration))
               (provider     (make-instance 'context-admission-test-provider
                                            :configuration configuration))
               (window       (list (json-object "type" "compaction"
                                                "encrypted_content" "private-checkpoint"))))
          (conversation-append-user-message conversation "Original exact request.")
          (let ((capture (conversation-compaction-capture conversation)))
            (conversation-append-user-message conversation
                                              (make-string 20000 :initial-element #\a))
            (let ((before    (copy-list (conversation-input-items conversation)))
                  (sequence  (conversation-next-sequence conversation))
                  (checked-p nil))
              (flet ((validate (candidate)
                       "Admit the full candidate, including arrivals, before publication."
                       (setf checked-p t)
                       (test-assert (find (second before)
                                          (conversation-input-items candidate) :test #'eq)
                                    "the validator sees the exact chronological arrival")
                       (provider-check-request-context
                        provider (json-object "input"
                                              (coerce (conversation-input-items candidate) 'vector)))))
                (test-assert
                 (handler-case
                     (progn
                       (if native-p
                           (conversation-append-native-compaction
                            conversation window :family ':responses :summary "Small handoff."
                            :compaction capture :validate-projection #'validate)
                           (conversation-append-summary
                            conversation "Small handoff." :compaction capture
                            :validate-projection                      #'validate))
                       nil)
                   (provider-context-budget-error () t))
                 "oversized candidate admission aborts publication"))
              (test-assert checked-p "publication invoked the admission callback")
              (test-assert (and (= sequence (conversation-next-sequence conversation))
                                (equal before (conversation-input-items conversation)))
                           "rejection preserves the full projection and durable sequence")
              (let ((loaded (conversation-load-by-id
                             configuration (conversation-identifier conversation))))
                (test-assert
                 (equal (mapcar #'json-encode before)
                        (mapcar #'json-encode (conversation-input-items loaded)))
                 "reload sees no rejected checkpoint"))))))))
  nil)

(-> test-context-admission-mission-large-budget () null)
(defun test-context-admission-mission-large-budget ()
  "Bound a real mission allocation at final projection without shrinking its ledger."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "272000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((provider     (make-instance 'context-admission-test-provider
                                          :configuration configuration))
             (application  (mission-inference-test--application configuration provider))
             (conversation (application-conversation application)))
        (application-mission-start
         application (mission-test--specification :turns 5 :tokens most-positive-fixnum))
        (unwind-protect
             (let* ((context              (mission-context-find conversation))
                    (goal                 (mission-context-goal context))
                    (original-reservation (floor most-positive-fixnum 5)))
               (mission--account-inference
                context
                (lambda ()
                  (test-assert (= original-reservation (getf goal :tokens-reserved))
                               "the original aggregate reservation is retained through dispatch")
                  (provider-open-response-stream
                   provider (provider-request-object provider conversation #())
                   :conversation conversation))
                (lambda (results)
                  (declare (ignore results))
                  (json-object "input_tokens" 7 "output_tokens" 3)))
               (test-assert (= 1 (context-admission-test-provider-network-calls provider))
                            "real projection and admission dispatch exactly once")
               (let ((ceiling (context-admission-test-provider-last-ceiling provider))
                     (request (context-admission-test-provider-last-request provider)))
                 (test-assert (and (plusp ceiling) (<= ceiling original-reservation))
                              "only the internal output allowance is reduced")
                 (test-assert (<= (+ (provider-request-input-token-estimate provider request)
                                     (max 256 ceiling))
                                  272000)
                              "the actual final request fits its complete context envelope")
                 (test-assert (null (json-get request "max_output_tokens"))
                              "the unsupported Codex wire ceiling is omitted"))
               (test-assert (= most-positive-fixnum (getf goal :token-limit))
                            "the finite aggregate budget is unchanged")
               (test-assert (and (= 10 (getf goal :tokens-used))
                                 (zerop (getf goal :tokens-reserved))
                                 (zerop (getf goal :requests-outstanding)))
                            "reported usage is charged once and the original reservation released"))
          (mission-detach application)))))
  (context-admission-test--bounded-child)
  nil)


;;;; -- Mission Ceiling Transport Fixtures --

(defclass context-admission-capped-test-provider (context-admission-test-provider)
  ()
  (:documentation "An inert Responses transport that accepts output ceilings."))

(defmethod provider-output-ceiling-p ((provider context-admission-capped-test-provider))
  "Enable the standard Responses wire ceiling for this inert transport."
  (declare (ignore provider))
  t)

(defmethod provider-open-native-compaction
    ((provider context-admission-test-provider) (request hash-table)
     &key credentials conversation)
  "Capture admitted native compaction without opening a transport."
  (declare (ignore credentials conversation))
  (setf (context-admission-test-provider-last-request provider) request
        (context-admission-test-provider-last-ceiling provider) *provider-maximum-output-tokens*)
  (incf (context-admission-test-provider-network-calls provider))
  (values nil 200 nil))

(-> test-context-admission-mission-wire-ceiling () null)
(defun test-context-admission-mission-wire-ceiling ()
  "Measure serialized cap overhead and finalize ordinary and compaction requests."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (dolist (native-p '(nil t))
        (let* ((provider                            (make-instance 'context-admission-capped-test-provider
                                                                   :configuration configuration))
               (request                             (json-object "input" (make-string 1000 :initial-element #\x)))
               (original                            (json-encode request))
               (*provider-mission-output-allowance* most-positive-fixnum)
               (observed                            nil)
               (*provider-cache-request-prefix*     '(:fixture t))
               (*provider-cache-prefix-function*
                 (lambda (prefix) (setf observed prefix))))
          (funcall (if native-p #'provider-open-native-compaction #'provider-open-response-stream)
                   provider request)
          (let* ((final-request (context-admission-test-provider-last-request provider))
                 (ceiling       (context-admission-test-provider-last-ceiling provider))
                 (wire          (json-get final-request "max_output_tokens"))
                 (input         (provider-request-input-token-estimate provider final-request)))
            (test-assert (= 1 (context-admission-test-provider-network-calls provider))
                         "exactly one finalized request reaches the inert transport")
            (test-assert (and (plusp ceiling) (= wire ceiling)
                              (<= (+ input (max 256 ceiling)) 5000))
                         "the actual wire ceiling fits including its own serialized overhead")
            (test-assert (> input (provider-request-input-token-estimate provider request))
                         "the cap field is included in the input estimate")
            (test-assert (equal original (json-encode request))
                         "the caller's original request is not changed")
            (unless native-p
              (test-assert (equal (getf observed :wire)
                                  (provider-cache-wire-plan
                                   final-request
                                   :volatile-items (getf observed :volatile-items 0)))
                           "cache observation fingerprints the finalized wire request")))))))
  nil)

(-> test-context-admission-mission-explicit-policy () null)
(defun test-context-admission-mission-explicit-policy ()
  "Never hide oversized caller ceilings or prompt content behind a mission allowance."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((provider                            (make-instance 'context-admission-capped-test-provider
                                                                 :configuration configuration))
             (*provider-mission-output-allowance* 10))
        (dolist (request
                  (list (json-object "max_output_tokens" 6000)
                        (json-object "max_completion_tokens" 6000)
                        (json-object "max_tokens" 6000)
                        (json-object "generationConfig" (json-object "maxOutputTokens" 6000))
                        (json-object "input" (make-string 22000 :initial-element #\x))
                        (json-object "instructions" (make-string 22000 :initial-element #\x))
                        (json-object "tools" (vector (json-object "description"
                                                                   (make-string 22000 :initial-element #\x))))))
          (test-assert
           (handler-case (progn (provider-open-response-stream provider request) nil)
             (provider-context-budget-error () t))
           "original explicit wire policy and all final prompt components retain refusal"))
        (let ((*provider-maximum-output-tokens* 6000))
          (test-assert
           (handler-case (progn (provider-open-response-stream provider (json-object)) nil)
             (provider-context-budget-error () t))
           "an oversized explicit dynamic cap is not silently reduced"))
        (test-assert (zerop (context-admission-test-provider-network-calls provider))
                     "all explicit policy refusals precede transport")
        (let ((*provider-maximum-output-tokens* 8))
          (provider-open-response-stream provider (json-object "max_output_tokens" 6))
          (test-assert (= 6 (context-admission-test-provider-last-ceiling provider))
                       "the effective ceiling is the conjunction of mission, caller and wire limits"))
        (let ((*provider-mission-output-allowance* nil))
          (provider-open-response-stream provider (json-object))
          (test-assert (null (context-admission-test-provider-last-ceiling provider))
                       "ordinary nonmission requests acquire no dynamic output ceiling")
          (test-assert (null (json-get (context-admission-test-provider-last-request provider)
                                     "max_output_tokens"))
                       "ordinary nonmission requests acquire no wire ceiling")))))
  nil)

(-> test-context-admission-mission-copy-protocol () null)
(defun test-context-admission-mission-copy-protocol ()
  "Copy each accepted ceiling field without mutating the original wire policy."
  (with-test-configuration (configuration)
    (dolist (entry
              (list (list (make-instance 'openai-api-provider :configuration configuration)
                          "max_output_tokens")
                    (list (make-instance 'openai-compatible-provider :configuration configuration
                                          :display-name "Inert" :family ':openai-compatible)
                          "max_completion_tokens")
                    (list (make-instance 'anthropic-api-key-provider :configuration configuration)
                          "max_tokens")
                    (list (make-instance 'cl-llm-provider-api:gemini-generate-content-provider)
                          "maxOutputTokens")))
      (destructuring-bind (provider field) entry
        (let* ((nested-p   (string= field "maxOutputTokens"))
               (generation (json-object "temperature" 0.5 "maxOutputTokens" 20))
               (request    (if nested-p
                               (json-object "generationConfig" generation "model" "inert")
                               (json-object field 20 "model" "inert")))
               (before     (json-encode request))
               (copy       (provider-request-with-output-ceiling provider request 10)))
          (test-assert (= 10 (json-get (if nested-p (json-get copy "generationConfig") copy)
                                      field))
                       "the accepted provider field carries the reduced ceiling")
          (test-assert (and (not (eq request copy))
                            (equal before (json-encode request))
                            (equal "inert" (json-get copy "model")))
                       "copying preserves the caller's explicit policy and unrelated fields")
          (when nested-p
            (test-assert (and (not (eq generation (json-get copy "generationConfig")))
                              (= 0.5 (json-get (json-get copy "generationConfig") "temperature")))
                         "nested generation settings are copied and preserved"))))))
  nil)

(-> test-context-admission-mission-reviewer () null)
(defun test-context-admission-mission-reviewer ()
  "Conjoin finite mission, reviewer, caller and wire limits with original settlement."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (dolist (caller '(nil 8))
        (let* ((provider                         (make-instance 'context-admission-capped-test-provider
                                                                :configuration configuration))
               (application                      (mission-inference-test--application configuration provider))
               (conversation                     (application-conversation application))
               (*provider-maximum-output-tokens* caller))
          (application-mission-start application (mission-test--specification :turns 4 :tokens 100))
          (unwind-protect
               (let* ((context     (mission-context-find conversation))
                      (goal        (mission-context-goal context))
                      (reviewer    (list :turn-limit 3 :token-limit 30 :turns-used 0 :tokens-used 0))
                      (reservation (or caller 10)))
                 (mission--account-inference
                  context
                  (lambda ()
                    (test-assert (and (= reservation (getf goal :tokens-reserved))
                                      (= reservation (getf reviewer :tokens-reserved)))
                                 "both ledgers retain their original minimum reservation")
                    (provider-open-response-stream
                     provider (if caller (json-object "max_output_tokens" 6) (json-object))))
                  (lambda (results)
                    (declare (ignore results))
                    (json-object "input_tokens" 2 "output_tokens" 3))
                  :allowance reviewer)
                 (test-assert (= (if caller 6 10)
                                 (context-admission-test-provider-last-ceiling provider))
                              "small finite allowances and explicit caller policy are conjoined")
                 (dolist (limit (list goal reviewer))
                   (test-assert (and (= 5 (getf limit :tokens-used))
                                     (= 1 (getf limit :turns-used))
                                     (zerop (getf limit :requests-outstanding))
                                     (zerop (getf limit :tokens-reserved)))
                                "each ledger charges usage once and releases its original reservation")))
            (mission-detach application))))))
  nil)

(-> test-context-admission-mission-refusal-settlement () null)
(defun test-context-admission-mission-refusal-settlement ()
  "Settle locally refused stream, compaction and reviewer requests as known zero."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (dolist (native-p '(nil t))
      (dolist (reviewer-p '(nil t))
        (with-test-configuration (configuration)
          (let* ((provider     (make-instance 'context-admission-test-provider :configuration configuration))
                 (application  (mission-inference-test--application configuration provider))
                 (conversation (application-conversation application))
                 (reviewer     (when reviewer-p
                                 (list :turn-limit 4 :token-limit 100 :turns-used 0 :tokens-used 0))))
            (application-mission-start application (mission-test--specification :turns 4 :tokens 100))
            (unwind-protect
                 (let* ((context          (mission-context-find conversation))
                        (goal             (mission-context-goal context))
                        (original-refusal nil))
                   (test-assert
                    (handler-case
                        (mission--account-inference
                         context
                         (lambda ()
                           (handler-bind ((provider-context-budget-error
                                            (lambda (condition) (setf original-refusal condition))))
                             (funcall (if native-p #'provider-open-native-compaction #'provider-open-response-stream)
                                      provider (json-object "max_output_tokens" 6000))))
                         (lambda (results) (declare (ignore results)) nil)
                         :allowance reviewer)
                      (error (condition)
                        (and (typep condition 'provider-context-budget-error)
                             (eq condition original-refusal)
                             (= 6000 (provider-context-budget-error-output-reserve condition))
                             (= 5000 (provider-context-budget-error-context-window condition))
                             (plusp (provider-context-budget-error-estimated-input-tokens condition)))))
                    "the accounted request is refused by ordinary admission")
                   (test-assert (zerop (context-admission-test-provider-network-calls provider))
                                "local refusal never enters the transport")
                   (dolist (limit (remove nil (list goal reviewer)))
                     (test-assert (zerop (getf limit :unknown-usage 0))
                                  "the local refusal has known zero usage")
                     (test-assert (and (zerop (getf limit :tokens-used))
                                       (= 1 (getf limit :turns-used))
                                       (zerop (getf limit :tokens-reserved))
                                       (zerop (getf limit :requests-outstanding)))
                                  "settlement releases once without refunding the admitted turn"))
                   (test-assert (eq ':active (getf goal :status))
                                "known zero usage does not quarantine the mission"))
              (mission-detach application)))))))
  nil)

(-> test-context-admission-mission-refusal-proof () null)
(defun test-context-admission-mission-refusal-proof ()
  "Require admission origin and no earlier admitted request before certifying zero."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (dolist (native-p '(nil t))
      (dolist (mode '(:synthetic :outside-boundary :after-admission :second-refusal :caught-refusal))
        (with-test-configuration (configuration)
          (let* ((provider     (make-instance 'context-admission-test-provider :configuration configuration))
                 (application  (mission-inference-test--application configuration provider))
                 (conversation (application-conversation application))
                 (open         (if native-p #'provider-open-native-compaction #'provider-open-response-stream))
                 (large        (json-object "max_output_tokens" 6000))
                 (entered-p    (member mode '(:after-admission :second-refusal))))
            (application-mission-start application (mission-test--specification :turns 4 :tokens 100))
            (unwind-protect
                 (let* ((context (mission-context-find conversation))
                        (goal    (mission-context-goal context)))
                   (test-assert
                    (handler-case
                        (mission--account-inference
                         context
                         (lambda ()
                           (when entered-p (funcall open provider (json-object)))
                           (case mode
                             (:outside-boundary
                              (provider-check-request-context provider large))
                             (:second-refusal
                              (funcall open provider large))
                             (:caught-refusal
                              (handler-case (funcall open provider large)
                                (provider-context-budget-error () nil))
                              (error "An unrelated later failure."))
                             (t
                              (error 'provider-context-budget-error
                                     :message "Synthetic admission-like failure."
                                     :estimated-input-tokens 6000 :output-reserve 256
                                     :context-window 5000 :non-text-estimate-p nil))))
                         (lambda (results) (declare (ignore results)) nil))
                      (mission-error () t))
                    "the supervised failure propagates")
                   (test-assert (= (if entered-p 1 0)
                                   (context-admission-test-provider-network-calls provider))
                                "the negative control records its actual transport entries")
                   (test-assert (and (= 1 (getf goal :unknown-usage))
                                     (eq ':blocked (getf goal :status)))
                                "a condition name or later local refusal cannot prove zero usage")
                   (test-assert (and (zerop (getf goal :tokens-used))
                                     (= 1 (getf goal :turns-used))
                                     (zerop (getf goal :tokens-reserved))
                                     (zerop (getf goal :requests-outstanding)))
                                "uncertain settlement releases its reservation exactly once"))
              (mission-detach application)))))))
  nil)

(-> test-context-admission-mission-code-assist-envelope () null)
(defun test-context-admission-mission-code-assist-envelope ()
  "Preserve Code Assist's routing envelope and its original inner output policy."
  (with-test-configuration (configuration)
    (let* ((provider   (make-instance 'gemini-code-assist-provider :configuration configuration))
           (inner      (json-object "generationConfig" (json-object "maxOutputTokens" 20
                                                                       "temperature" 0.5)))
           (request    (json-object "request" inner "project" "inert"))
           (before     (json-encode request))
           (copy       (provider-request-with-output-ceiling provider request 10))
           (generation (json-get (json-get copy "request") "generationConfig")))
      (test-assert (and (= 10 (json-get generation "maxOutputTokens"))
                        (= 0.5 (json-get generation "temperature"))
                        (equal "inert" (json-get copy "project")))
                   "the ceiling is encoded inside the copied GenerateContent request")
      (test-assert (and (equal before (json-encode request))
                        (not (eq copy request))
                        (not (eq inner (json-get copy "request"))))
                   "both the routing envelope and inner generation policy are request-local")
      (test-assert (member 20 (provider-request-output-ceilings request :enclosed-p t))
                   "original inner ceilings participate in ordinary admission and conjunction")))
  nil)

(-> test-context-admission-code-assist-first-setup () null)
(defun test-context-admission-code-assist-first-setup ()
  "Admit and observe the actual serialized Code Assist envelope after initial setup."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (dolist (allowance (list most-positive-fixnum nil))
        (let* ((provider                            (make-instance 'gemini-code-assist-provider :configuration configuration))
               (request                             (json-object "model" "inert" "project" nil
                                                                 "request" (json-object "contents" #())))
               (before                              (json-encode request))
               (credentials                         (make-instance 'oauth-credentials :access-token "inert"))
               (*provider-mission-output-allowance* allowance)
               (setup-calls                         0)
               (observations                        nil)
               (sent                                nil))
          (test-call-with-function-replacements
           (list
            (list 'gemini-code-assist-ensure-setup
                  (lambda (provider credentials)
                    (declare (ignore credentials))
                    (incf setup-calls)
                    (setf (gemini-code-assist-provider-project provider)
                          "this-is-a-project-id-discovered-during-first-setup"
                          (gemini-code-assist-provider-setup-complete-p provider) t)
                    provider))
            (list 'provider-cache-prefix-observe
                  (lambda (request)
                    (push (json-encode request) observations)))
            (list 'provider-post-event-stream
                  (lambda (url body &rest options)
                    (declare (ignore url options))
                    (setf sent (json-decode (babel:octets-to-string body :encoding :utf-8)))
                    (values nil 200 nil))))
           (lambda ()
             (provider-open-response-stream provider request :credentials credentials)))
          (test-assert (json-object-p sent) "the actual primary method serialized one request")
          (let* ((input      (provider-request-input-token-estimate provider sent))
                 (generation (json-get (json-get sent "request") "generationConfig"))
                 (cap        (and generation (json-get generation "maxOutputTokens"))))
            (format t "~&Code Assist first setup: input=~D cap=~S total=~D window=5000~%"
                    input cap (+ input (max 256 (or cap 0))))
            (test-assert (<= (+ input (max 256 (or cap 0))) 5000)
                         "the actual serialized input and output headroom fit the window")
            (test-assert (if allowance (and cap (plusp cap)) (null cap))
                         "only a mission allowance adds a wire ceiling"))
          (test-assert (equal (json-get sent "project")
                              "this-is-a-project-id-discovered-during-first-setup")
                       "the initial nil project is finalized before transport")
          (test-assert (and (= 1 setup-calls) (= 1 (length observations)))
                       "setup and cache observation each run exactly once")
          (test-assert (equal (first observations) (json-encode sent))
                       "cache observation sees exactly the serialized final envelope")
          (test-assert (equal before (json-encode request))
                       "neither routing setup nor mission headroom mutates the original request")))))
  nil)

(defclass context-admission-nonmonotonic-test-provider (context-admission-capped-test-provider)
  ()
  (:documentation "An inert transport with deliberately nonmonotonic cap-sensitive estimates."))

(defmethod provider-request-input-token-estimate
    ((provider context-admission-nonmonotonic-test-provider) (request hash-table))
  "Make a smaller serialized ceiling increase estimated input beyond the window."
  (declare (ignore provider))
  (let ((ceiling (json-get request "max_output_tokens")))
    (values (cond ((null ceiling)
                   100)
                  ((> ceiling 1000)
                   4500)
                  (t
                   6000))
            nil)))

(-> test-context-admission-mission-final-recheck () null)
(defun test-context-admission-mission-final-recheck ()
  "Fail closed when a custom estimator invalidates a reduced or provisional candidate."
  (dolist (window '("5000" "1000"))
    (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" window)
                            ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
      (with-test-configuration (configuration)
        (let ((provider                            (make-instance 'context-admission-nonmonotonic-test-provider
                                                                 :configuration configuration))
              (*provider-mission-output-allowance* most-positive-fixnum))
          (test-assert
           (handler-case (progn (provider-open-response-stream provider (json-object)) nil)
             (provider-context-budget-error () t))
           "a nonpositive envelope or an oversized final copy is refused")
          (test-assert (zerop (context-admission-test-provider-network-calls provider))
                       "no candidate bypasses final context admission")))))
  nil)

(defclass context-admission-child-test-provider (context-admission-capped-test-provider)
  ((allocation :initform nil :accessor context-admission-child-test-allocation
               :documentation "Full mission admission before final request projection.")
   (caller-cap :initform nil :accessor context-admission-child-test-caller-cap
               :documentation "Explicit caller policy before transport admission."))
  (:documentation "Fail a real child after observing its final encoded request."))

(defmethod provider-with-configuration
    ((provider context-admission-child-test-provider) (configuration configuration))
  "Retain the inert transport and observations in the native child's provider."
  (declare (ignore configuration))
  provider)

(defmethod provider-stream-turn
    ((provider context-admission-child-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Admit the real request into an inert transport, then fail without usage."
  (declare (ignore tool-namespaces event-callback goal-context compaction-p))
  (setf (context-admission-child-test-allocation provider) *provider-mission-output-allowance*
        (context-admission-child-test-caller-cap provider) *provider-maximum-output-tokens*)
  (provider-open-response-stream provider (provider-request-object provider conversation #())
                                 :conversation conversation)
  (error "Synthetic failure after final encoded admission without usage."))

(-> context-admission-test--bounded-child () null)
(defun context-admission-test--bounded-child ()
  "Distinguish full child allocation, lowered wire cap and durable measured totals."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "272000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration root)
      (setf configuration (configuration-copy configuration :working-directory root))
      (let* ((provider     (make-instance 'context-admission-child-test-provider
                                          :configuration configuration))
             (application  (mission-inference-test--application configuration provider))
             (conversation (application-conversation application))
             (registry     (task-augment-tool-registry (application-tool-registry application)))
             (orchestrator (task-run-tool-orchestrator (tool-registry-find registry "task" "run")))
             (definition   (task-agent-definition-create
                            :name "admission-child" :description "Exercise admission and settlement"
                            :instructions "Attempt one request." :tools ':all :source ':test))
             (entry        (list :definition definition :detached t
                                 :item (list :agent "admission-child" :task "Attempt one request.")))
             (job          nil))
        (setf (agent-provider (application-agent application)) provider)
        (application-mission-start application
                                   (mission-test--specification :tokens most-positive-fixnum))
        (let* ((goal    (application-goal application))
               (context (mission-context-find conversation)))
          (unwind-protect
               (progn
                 (setf job (first (task-orchestrator-start-jobs
                                   orchestrator (application-agent application) (list entry))))
                 (task-job-await job nil)
                 (let* ((allocation (context-admission-child-test-allocation provider))
                        (ceiling    (context-admission-test-provider-last-ceiling provider))
                        (request    (context-admission-test-provider-last-request provider))
                        (result     (getf (task-job-snapshot job) :result))
                        (summary    (list :unknown-usage 1 :reservation-charged-tokens allocation
                                          :reservation-charge-requests 1)))
                   (test-assert (and (null (context-admission-child-test-caller-cap provider))
                                     (typep allocation '(integer 1))
                                     (typep ceiling '(integer 1)) (< ceiling allocation)
                                     (= ceiling (json-get request "max_output_tokens"))
                                     (= 1 (context-admission-test-provider-network-calls provider)))
                                "no caller ceiling: one actual final encoded request has 0<W<R")
                   (test-assert (<= (+ (provider-request-input-token-estimate provider request) ceiling) 272000)
                                "the final encoded request fits the window, independently of full allocation")
                   (test-assert (and (eq ':failed (getf result :status)) (getf result :error)
                                     (null (getf result :usage)) (= 1 (getf result :unknown-usage 0))
                                     (= allocation (getf result :reservation-charged-tokens 0))
                                     (= 1 (getf result :reservation-charge-requests 0)))
                                "the actual child retains failure and UNKNOWN audit with one full-R charge")
                   (test-assert (and (eq goal (application-goal application))
                                     (eq ':active (getf goal :status))
                                     (= allocation (getf goal :tokens-used))
                                     (= allocation (getf goal :reservation-charged-tokens 0))
                                     (zerop (getf goal :unknown-usage))
                                     (zerop (getf goal :requests-outstanding))
                                     (zerop (getf goal :tokens-reserved)))
                                "settlement releases R once without shared quarantine or replacing the goal")
                   (test-assert (equal summary (mission-task-job-accounting-summary job))
                                "the locked child summary separates allocation charge from measured usage")
                   (test-assert (every #'zerop (subseq (conversation-usage conversation) 0 5))
                                "an allocation charge is absent from running measured totals")
                   (mission--account-inference
                    context (lambda () (json-object "input_tokens" 2 "output_tokens" 3)) #'first)
                   (conversation-append-provider-metadata
                    conversation '(:usage (("input_tokens" 2) ("output_tokens" 3) ("total_tokens" 5))))
                   (test-assert (and (eq goal (application-goal application))
                                     (eq ':active (getf goal :status))
                                     (= (+ allocation 5) (getf goal :tokens-used))
                                     (= allocation (getf goal :reservation-charged-tokens))
                                     (equal '(2 3 5 0 0 nil) (conversation-usage conversation)))
                                "subsequent known-usage work is admitted and only measured usage enters totals")
                   (task-job-await job nil)
                   (test-assert (and (= 1 (context-admission-test-provider-network-calls provider))
                                     (= (+ allocation 5) (getf goal :tokens-used))
                                     (equal summary (mission-task-job-accounting-summary job)))
                                "duplicate completion observation makes no request or second charge")
                   (application--record-goal application)
                   (conversation-append-summary conversation "Synthetic compaction after bounded failure.")
                   (let* ((loaded   (conversation-load (conversation-pathname conversation)))
                          (restored (progn (application--load-goal application)
                                           (application-goal application))))
                     (test-assert (equal '(2 3 5 0 0 nil) (conversation-usage loaded))
                                  "compaction and reload preserve measured-only running totals")
                     (test-assert (and (= (+ allocation 5) (getf restored :tokens-used))
                                       (= allocation (getf restored :reservation-charged-tokens))
                                       (= 1 (rest (assoc (job-identifier job)
                                                        (getf restored :unknown-usage-by-child) :test #'equal)))
                                       (equal (getf goal :reservation-charges-by-child)
                                              (getf restored :reservation-charges-by-child))
                                       (zerop (getf restored :unknown-usage))
                                       (eq ':active (getf restored :status)))
                                  "goal append, compaction and reload retain full allocation and child audit"))))
            (when job (task-job-await job nil))
            (task-orchestrator-close orchestrator)
            (mission-detach application))))))
  nil)

(-> test-context-admission-mission-handoff-reasons () null)
(defun test-context-admission-mission-handoff-reasons ()
  "Keep cancellation and deadline supervisor failures despite retained refusal proof."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (dolist (reason '(:aborted :exhausted :gate-timeout))
      (with-test-configuration (configuration)
        (let* ((provider     (make-instance 'context-admission-test-provider :configuration configuration))
               (application  (mission-inference-test--application configuration provider))
               (conversation (application-conversation application))
               (failure      (make-condition 'mission-error :reason reason :message "Supervisor stopped.")))
          (application-mission-start application (mission-test--specification :turns 4 :tokens 100))
          (unwind-protect
               (test-call-with-function-replacements
                (list (list 'mission--supervise
                            (lambda (context label function)
                              (declare (ignore context label))
                              (handler-case (funcall function)
                                (provider-context-budget-error () (error failure))))))
                (lambda ()
                  (let ((context (mission-context-find conversation)))
                    (test-assert
                     (handler-case
                         (mission--account-inference
                          context
                          (lambda () (provider-open-response-stream
                                      provider (json-object "max_output_tokens" 6000)))
                          (lambda (results) (declare (ignore results)) nil))
                       (mission-error (condition) (eq condition failure)))
                     "nonfailure supervisor reasons preserve their original condition")
                    (test-assert
                     (and (zerop (context-admission-test-provider-network-calls provider))
                          (zerop (getf (mission-context-goal context) :unknown-usage 0))
                          (zerop (getf (mission-context-goal context) :requests-outstanding)))
                     "the handoff guard does not change d0 settlement"))))
            (mission-detach application))))))
  nil)


;;;; -- Measured Ordinary Context --

(-> context-admission-test--with-measured-context (function) t)
(defun context-admission-test--with-measured-context (function)
  "Run FUNCTION with real request encoding, small policy and an inert transport."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (provider     (make-instance 'context-admission-test-provider :configuration configuration))
             (agent        (agent-create :configuration configuration :conversation conversation
                                         :provider provider :tool-registry (make-instance 'tool-registry))))
        (test-call-with-function-replacements
         (list (list 'system-prompt
                     (lambda (configuration) (declare (ignore configuration)) "small policy"))
               (list 'request-context-session-state
                     (lambda (configuration &key hurry-up-p)
                       (declare (ignore configuration hurry-up-p)) ""))
               (list 'context-resolve-request
                     (lambda (configuration conversation tools &key goal-context compaction-p)
                       (declare (ignore configuration conversation tools goal-context compaction-p)) nil)))
         (lambda ()
          (let* ((*provider-input-measurement-credentials*
                   (context-route-tests--credentials configuration "measured-test-account" "measured-test-bearer"))
                 (*provider-input-measurement-route*
                   (provider-input-measurement--route provider *provider-input-measurement-credentials*)))
            (funcall function agent))))))))

(-> context-admission-test--measure
    (agent &key (:usage t) (:envelope-length (integer 1)) (:during-request (option function))) null)
(defun context-admission-test--measure
    (agent &key (usage (json-object "input_tokens" 1000 "output_tokens" 200
                                   "total_tokens" 1200 "cached_input_tokens" 900))
                (envelope-length 30000) during-request)
  "Admit a real request, persist opaque reasoning and anchor its ordinary usage."
  (let* ((provider                   (agent-provider agent))
         (conversation               (agent-conversation agent))
         (context                    (provider-context-request-create provider conversation #()))
         (*provider-context-request* context)
         (request                    (provider-request-object provider conversation #()))
         (result                     (make-instance 'provider-result
                                                    :output-items
                                                    (list (json-object "type" "reasoning" "summary" #()
                                                                       "encrypted_content"
                                                                       (make-string envelope-length :initial-element #\x)))
                                                    :usage usage)))
    (provider-open-response-stream provider request :credentials *provider-input-measurement-credentials* :conversation conversation)
    (when during-request (funcall during-request conversation))
    (agent--persist-provider-result agent result :request-number 1)
    (provider-context-note-response context result))
  nil)

(-> test-context-admission-usage-ownership () null)
(defun test-context-admission-usage-ownership ()
  "Keep ordinary upstream usage scoped to its newest exact owner and generation."
  (context-admission-test--with-measured-context
   (lambda (agent)
     (let* ((owner    (agent-conversation agent))
            (provider (agent-provider agent))
            (stale    (provider-context-request-create provider owner #()))
            (result   (make-instance 'provider-result)))
       (conversation-append-user-message owner "Measure this ordinary request.")
       (context-admission-test--measure agent :envelope-length 100)
       (let ((newest (copy-tree (conversation-context-usage-sample owner))))
         (test-assert newest "The newer completed request earns ordinary usage.")
         (provider-context-note-response stale result)
         (test-assert (equal newest (conversation-context-usage-sample owner))
                      "An older publication cannot erase the newer sample.")
         (let* ((*provider-context-request* (provider-context-request-create provider owner #()))
                (foreign                    (conversation-create (agent-configuration agent))))
           (provider-request-object provider foreign #())
           (test-assert (not (getf *provider-context-request* :eligible-p))
                        "Encoding another conversation cannot earn or consume this owner's usage."))
         (let ((pending (provider-context-request-create provider owner #())))
           (setf (getf pending :wire-provider) provider)
           (setf (conversation-input-items owner) (conversation-input-items owner))
           (provider-context-note-response pending result)
           (test-assert (equal newest (conversation-context-usage-sample owner))
                        "Projection replacement invalidates a pending publication even when items compare equal."))))))
  nil)

(-> test-context-admission-measured-reasoning () null)
(defun test-context-admission-measured-reasoning ()
  "Use full ordinary usage for retained reasoning, charging output once and new input now."
  (context-admission-test--with-measured-context
   (lambda (agent)
     (let* ((conversation (agent-conversation agent))
            (provider     (agent-provider agent)))
       (conversation-append-user-message conversation "Before the measured request.")
       (context-admission-test--measure agent)
       (test-assert (conversation-context-usage-sample conversation) "ordinary usage has a projection anchor")
       (test-assert (not (agent-should-compact-p agent)) "opaque replay does not inflate the soft estimate")
       (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
              (request                    (provider-request-object provider conversation #())))
         (test-assert (= 1200 (provider-request-input-token-estimate provider request))
                      "input includes cached tokens and output is carried exactly once")
         (provider-open-response-stream provider request :credentials *provider-input-measurement-credentials* :conversation conversation)
         (let ((*provider-context-request* nil))
           (test-assert (> (provider-request-input-token-estimate provider request) 5000)
                        "unmeasured opaque history keeps its uncertain fallback"))
         (test-assert
          (handler-case
              (progn
                (setf (gethash "max_output_tokens" request) 3900)
                (provider-open-response-stream provider request :credentials *provider-input-measurement-credentials* :conversation conversation) nil)
            (provider-context-budget-error (condition)
              (= 3900 (provider-context-budget-error-output-reserve condition))))
          "measured input plus an explicit output cap is refused before transport")
         (test-assert (= 2 (context-admission-test-provider-network-calls provider))
                      "only the two admitted requests reached transport"))
       (conversation-append-provider-item
        conversation (json-object "type" "function_call" "name" "test" "call_id" "new-local-evidence"
                                  "arguments" "{}"))
       (conversation-append-tool-result conversation "new-local-evidence" :tool-name "test"
                                        :output (make-string 20000 :initial-element #\e) :success-p t)
       (test-assert (agent-should-compact-p agent) "new tool evidence crosses the soft limit without new usage")
       (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
              (request                    (provider-request-object provider conversation #())))
         (test-assert
          (handler-case (progn (provider-open-response-stream provider request :credentials *provider-input-measurement-credentials* :conversation conversation) nil)
            (provider-context-budget-error () t))
          "new tool evidence also crosses hard admission")))))
  nil)

(-> test-context-admission-measured-request-tails () null)
(defun test-context-admission-measured-request-tails ()
  "Include new temporary context, locally expanded input and image allowances."
  (context-admission-test--with-measured-context
   (lambda (agent)
     (let* ((conversation (agent-conversation agent))
            (provider     (agent-provider agent)))
       (context-admission-test--measure agent)
       (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
              (request                    (provider-request-object provider conversation #()
                                                                   :goal-context (make-string 20000 :initial-element #\g))))
         (test-assert (> (provider-request-input-token-estimate provider request) 5000)
                      "the current temporary tail is not covered by previous usage")
         (test-assert
          (handler-case (progn (provider-open-response-stream provider request :credentials *provider-input-measurement-credentials* :conversation conversation) nil)
            (provider-context-budget-error () t))
          "temporary context can refuse the next request"))
       (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
              (request                    (provider-request-object provider conversation #())))
         (setf (gethash "input" request)
               (concatenate 'vector (json-get request "input")
                            (vector (json-object "type" "message" "role" "user" "content"
                                                 (vector (json-object "type" "input_image" "image_url" "https://example.invalid/image"))))))
         (multiple-value-bind (tokens uncertain-p) (provider-request-input-token-estimate provider request)
           (test-assert (and uncertain-p (> tokens *provider-image-token-estimate*))
                        "new media retains its bounded allowance alongside measured input"))))))
  nil)

(-> test-context-admission-measured-invalidation () null)
(defun test-context-admission-measured-invalidation ()
  "Reject malformed samples and invalidate measurements at context ownership boundaries."
  (context-admission-test--with-measured-context
   (lambda (agent)
     (let* ((conversation (agent-conversation agent))
            (provider     (agent-provider agent)))
       (dolist (usage (list nil (json-object "input_tokens" 1000)
                           (json-object "input_tokens" -1 "output_tokens" 20)
                           (json-object "input_tokens" "1000" "output_tokens" 20)
                           (json-object "input_tokens" 1000 "output_tokens" 0)
                           (json-object "input_tokens" 1000 "output_tokens" 200 "total_tokens" 2)
                           (json-object "input_tokens" 1000 "output_tokens" 200 "cached_input_tokens" 2000)))
         (context-admission-test--measure agent :envelope-length 64)
         (context-admission-test--measure agent :usage usage :envelope-length 64)
         (test-assert (null (conversation-context-usage-sample conversation))
                      "an incomplete or inconsistent response cannot establish a sample"))
       (context-admission-test--measure agent :envelope-length 64)
       (let* ((unconfigured (make-instance 'model-provider))
              (context      (provider-context-request-create unconfigured conversation #())))
         (test-assert (null (getf context :eligible-p))
                      "providers without a configuration method use unmeasured admission"))
       (dolist (field '("instructions" "model" "tools" "input"))
         (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
                (request                    (provider-request-object provider conversation #())))
           (setf (gethash field request)
                 (if (equal field "input")
                     (vector (json-object "type" "message" "role" "user" "content" "A changed covered prefix."))
                     "A changed request field."))
           (multiple-value-bind (tokens matched-p) (provider-context--matched-input provider request)
             (declare (ignore tokens))
             (test-assert (not matched-p) "changed encoded headers or covered items reject the baseline"))))
       (let* ((other                      (make-instance 'context-admission-test-provider
                                                         :configuration (agent-configuration agent)))
              (*provider-context-request* (provider-context-request-create other conversation #()))
              (request                    (provider-request-object other conversation #())))
         (multiple-value-bind (tokens matched-p) (provider-context--matched-input other request)
           (declare (ignore tokens))
           (test-assert matched-p "an identically configured encoder revalidates portable coverage")))
       (let* ((other                      (conversation-create (agent-configuration agent)))
              (*provider-context-request* (provider-context-request-create provider other #()))
              (request                    (provider-request-object provider other #())))
         (multiple-value-bind (tokens matched-p) (provider-context--matched-input provider request)
           (declare (ignore tokens))
           (test-assert (not matched-p) "conversations do not share usage anchors")))
       (context-admission-test--measure
        agent :envelope-length 64
        :during-request        (lambda (conversation)
                                 (conversation-append-user-message conversation "Arrived while awaiting a reply.")))
       (test-assert (null (conversation-context-usage-sample conversation))
                    "input arriving in flight is not charged to the provider output allowance")
       (context-admission-test--measure agent :envelope-length 64)
       (conversation-append-provider-metadata conversation '(:compaction :native :usage (("input_tokens" 0) ("output_tokens" 0))))
       (test-assert (null (conversation-context-usage-sample conversation))
                    "native compaction usage cannot replace the ordinary baseline")
       (context-admission-test--measure agent :envelope-length 64)
       (conversation-append-summary conversation "A replacement context.")
       (test-assert (null (conversation-context-usage-sample conversation)) "summary publication invalidates the sample")
       (context-admission-test--measure agent :envelope-length 64)
       (test-assert (conversation-context-usage-sample
                     (conversation-load (conversation-pathname conversation)))
                    "disk resume restores fingerprint evidence for revalidation"))))
  nil)

(-> test-context-admission-measured-high-usage () null)
(defun test-context-admission-measured-high-usage ()
  "A genuinely full measured context crosses both limits despite a small serialization."
  (context-admission-test--with-measured-context
   (lambda (agent)
     (let* ((conversation (agent-conversation agent))
            (provider     (agent-provider agent)))
       (context-admission-test--measure
        agent :envelope-length 64
        :usage                 (json-object "input_tokens" 4800 "output_tokens" 200
                                            "total_tokens" 5000 "cached_input_tokens" 4700))
       (test-assert (agent-should-compact-p agent) "full measured occupancy crosses the soft limit")
       (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
              (request                    (provider-request-object provider conversation #())))
         (test-assert
          (handler-case (progn (provider-open-response-stream provider request :credentials *provider-input-measurement-credentials* :conversation conversation) nil)
            (provider-context-budget-error (condition)
              (= 5000 (provider-context-budget-error-estimated-input-tokens condition))))
          "a small envelope cannot erase authoritative measured occupancy")))))
  nil)

(-> test-context-admission-measured-resume () null)
(defun test-context-admission-measured-resume ()
  "Revalidate persisted measurements after restart and charge newly retained evidence."
  (context-admission-test--with-measured-context
   (lambda (original)
     (context-admission-test--measure original)
     (let* ((configuration (agent-configuration original))
            (conversation  (conversation-load (conversation-pathname (agent-conversation original))))
            (provider      (make-instance 'context-admission-test-provider :configuration configuration))
            (registry      (make-instance 'tool-registry))
            (agent         (agent-create :configuration configuration :conversation conversation
                                         :provider provider :tool-registry registry))
            (application   (make-instance 'application :configuration configuration
                                                      :conversation conversation :provider provider
                                                      :agent agent :tool-registry registry)))
       (test-assert (conversation-context-usage-sample-p (conversation-context-usage-sample conversation))
                    "restart restores portable coverage evidence")
       (test-assert (not (agent-should-compact-p agent))
                    "retained opaque replay does not cause startup compaction")
       (let* ((*provider-context-request* (provider-context-request-create provider conversation #()))
              (request                    (provider-request-object provider conversation #())))
         (test-assert (= 1200 (provider-request-input-token-estimate provider request))
                      "the final encoded prefix revalidates the historical measurement")
         (setf (gethash "instructions" request) "Different wire policy.")
         (test-assert (not (nth-value 1 (provider-context--matched-input provider request)))
                      "restart does not bypass final wire validation"))
       (let ((before (application-context-usage application)))
         (conversation-append-provider-item
          conversation (json-object "type" "function_call" "name" "test" "call_id" "resume-output"
                                    "arguments" "{}"))
         (conversation-append-tool-result conversation "resume-output" :tool-name "test"
                                          :output (make-string 20000 :initial-element #\e) :success-p t)
         (test-assert (> (application-context-usage application) before)
                      "the meter includes tool output not covered by the last response")
         (test-assert (= (application-context-usage application)
                         (context-estimate-known-tokens configuration conversation
                                                       :provider provider :tool-registry registry))
                      "the meter uses the same current estimate as soft compaction")
         (test-assert (agent-should-compact-p agent)
                      "new retained evidence can cross the soft limit after restart"))
       (conversation-append-provider-metadata
        conversation '(:usage (("input_tokens" 1000) ("output_tokens" 200) ("total_tokens" 1200))))
       (test-assert (null (conversation-context-usage-sample
                           (conversation-load (conversation-pathname conversation))))
                    "a response interrupted before publishing coverage invalidates the older anchor")
       (conversation-append-record conversation '(:context-usage :sample (:format 1)))
       (test-assert (null (conversation-context-usage-sample
                           (conversation-load (conversation-pathname conversation))))
                    "malformed coverage is discarded during durable replay"))))
  nil)


;;;; -- Measured Ordinary Input Regression --

(defclass context-measured-test-provider (context-admission-test-provider)
  ((requests
    :initform      nil
    :accessor      context-measured-test-requests
    :documentation "Final admitted requests in newest-first order."))
  (:documentation "Real Responses execution with synthetic final usage and no network."))

(defparameter *context-measured-test-refusal-function* nil
  "Request-local fixture observer for the first refusal before agent recovery.")

(defmethod provider-open-response-stream :around
    ((provider context-measured-test-provider) (request hash-table)
     &key credentials conversation)
  "Observe actual admission inside the ordinary loop's recovery handler."
  (declare (ignore credentials conversation))
  (handler-bind ((provider-context-budget-error
                   (lambda (condition)
                     (when *context-measured-test-refusal-function*
                       (funcall *context-measured-test-refusal-function* condition)))))
    (call-next-method)))

(defmethod provider-open-response-stream
    ((provider context-measured-test-provider) (request hash-table)
     &key credentials conversation)
  "Return a complete inert Responses stream after ordinary admission."
  (declare (ignore credentials conversation))
  (push request (context-measured-test-requests provider))
  (setf (context-admission-test-provider-last-request provider) request
        (context-admission-test-provider-last-ceiling provider) *provider-maximum-output-tokens*)
  (incf (context-admission-test-provider-network-calls provider))
  (let* ((number (context-admission-test-provider-network-calls provider))
         (id     (format nil "measured-response-~D" number))
         (item   (json-object "type" "message" "id" (format nil "measured-output-~D" number)
                              "role" "assistant" "status" "completed"
                              "content" (vector (json-object "type" "output_text" "text" "Inspected synthetic images."))))
         (source
           (concatenate
            'string
            (test-sse-event-string (json-object "type" "response.created" "response" (json-object "id" id)))
            (test-sse-event-string (json-object "type" "response.output_item.done" "item" item))
            (test-sse-event-string
             (json-object "type" "response.completed" "response"
                          (json-object "id" id "status" "completed" "output" (vector item)
                                       "usage" (json-object "input_tokens" 160000 "output_tokens" 30
                                                            "total_tokens" 160030
                                                            "input_tokens_details" (json-object "cached_tokens" 120000))))))))
    (values (make-instance 'test-character-input-stream :source source) 200 nil)))

(-> context-measured-tests--append-images (conversation integer integer &key (:separate-p boolean)) null)
(defun context-measured-tests--append-images (conversation start count &key separate-p)
  "Append correlated fs.view-image-shaped outputs without opening any image."
  (dotimes (offset count)
    (let* ((number (+ start offset))
           (id     (format nil "measured-image-~D" number)))
      (when separate-p
        (conversation-append-user-message conversation (format nil "Inspect synthetic image ~D" number)))
      (conversation-append-provider-item
       conversation (json-object "type" "function_call" "call_id" id "name" "fs__view_image"
                                 "arguments" "{\"path\":\"synthetic.png\"}"))
      (conversation-append-provider-item
       conversation (json-object "type" "function_call_output" "call_id" id
                                 "output" (vector (json-object "type" "input_text" "text" "Viewed synthetic PNG")
                                                  (json-object "type" "input_image" "detail" "high"
                                                               "image_url" (format nil "data:image/png;base64,c3ludGhldGlj~D" number)))))))
  nil)

(-> context-measured-tests--image-count (t) (integer 0))
(defun context-measured-tests--image-count (value)
  "Count structured image nodes, not image-looking text."
  (typecase value
    (hash-table
     (if (json-string= (json-get value "type") "input_image")
         1
         (loop for child being the hash-values of value sum (context-measured-tests--image-count child))))
    (string
     0)
    (vector
     (loop for child across value sum (context-measured-tests--image-count child)))
    (cons
     (loop for child in value sum (context-measured-tests--image-count child)))
    (t
     0)))

(-> context-measured-tests--run (&key (:separate-p boolean) (:headroom-p boolean)) null)
(defun context-measured-tests--run (&key separate-p headroom-p)
  "Earn a genuine ordinary input measurement, then admit appended evidence at 600K."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "900000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "8192"))
    (with-test-configuration (configuration)
      (let* ((configuration                       (configuration-copy configuration :model "gpt-6.1-sol" :provider-validation-p nil))
             (*context-contributors*              nil)
             (*context-resolver*                  (cl-llm-provider-api:make-context-resolver))
             (*context-last-deliveries*           (make-hash-table :test #'equal))
             (*context-last-delivery-order*       nil)
             (*provider-mission-output-allowance* (when headroom-p 8192))
             (provider                            (make-instance 'context-measured-test-provider :configuration configuration
                                                                :credential-manager                             (credential-manager-create configuration)))
             (conversation                        (conversation-create configuration))
             (agent                               (agent-create :configuration configuration :conversation conversation :provider provider
                                                                :tool-registry (make-instance 'tool-registry)))
             (completions                         nil)
             (observer                            (make-instance 'callback-agent-observer
                                                                 :status-callback (lambda (status details)
                                                                                    (when (eq status :provider-request-completed)
                                                                                      (push details completions))
                                                                                    nil)))
             (prior-count                         (if separate-p 45 35))
             (first-result                        nil)
             (target-result                       nil)
             (refusal                             nil))
        (conversation-append-user-message conversation (make-string 32000 :initial-element #\u))
        (context-measured-tests--append-images conversation 0 prior-count :separate-p separate-p)
        (test-call-with-function-replacements
         (list (list 'credential-manager-credentials
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (provider-tests--credentials configuration))))
         (lambda ()
           (test-assert (not (agent-should-compact-p agent)) "Small synthetic payloads do not trigger the soft compaction check.")
           (setf first-result (agent--run-provider-loop agent observer :tools-p nil))
           (let* ((request       (first (context-measured-test-requests provider)))
                  (prefix        (getf (first completions) :cache-prefix))
                  (wire          (getf prefix :wire))
                  (stable-count  (getf wire :prefix-items 0))
                  (input         (json-get request "input"))
                  (stable-images (context-measured-tests--image-count (subseq input 0 stable-count)))
                  (usage         (provider-usage-normalize (provider-result-usage first-result))))
             (test-assert (= 160000 (json-get usage "input_tokens")) "A completed inert ordinary response supplies the full input measurement.")
             (test-assert (= 120000 (json-get usage "cached_input_tokens")) "Cached input is a subset of the measured full input.")
             (test-assert (= 1 (context-admission-test-provider-network-calls provider)) "Only the earned first success reached transport.")
             (test-assert (= prior-count (context-measured-tests--image-count request)) "The measured prior request contains every prior structured image.")
             (test-assert (= prior-count stable-images) "The actual final stable prefix covers images in the newest user exchange.")
             (test-assert (equal wire (provider-cache-wire-plan request :volatile-items (getf prefix :volatile-items 0)))
                          "The ordinary completion fingerprint describes the actual final wire request.")
             (format t "~&C-PREFIX separate=~S headroom=~S input=160000 cached=120000 images=~D stable-images=~D wire-items=~D stable-items=~D volatile=~D raw=~D reserve=~D~%"
                     separate-p headroom-p prior-count stable-images (length input) stable-count
                     (getf prefix :volatile-items 0) (provider-request-input-token-estimate provider request)
                     (provider-request-output-reserve provider request)))
           (context-measured-tests--append-images conversation prior-count 2 :separate-p separate-p)
           (conversation-append-provider-item conversation
                                               (json-object "type" "message" "role" "assistant" "content" "New generated reasoning text is delta."))
           (platform-setenv "AUTOLITH_CONTEXT_WINDOW" "600000")
           (test-assert (= 600000 (provider-effective-context-window provider)) "Only this private fixture changes from its earning window to the target window.")
           (test-assert (not (agent-should-compact-p agent)) "The actual ordinary loop need not compact the measured recent images.")
           (let ((before (mapcar #'json-encode (conversation-input-items conversation))))
             ;; Observe the original typed refusal before the ordinary recovery handler.
             ;; This prevents paid summaries from obscuring the causal admission failure.
             (block target
               (let ((*context-measured-test-refusal-function*
                       (lambda (condition)
                         (setf refusal condition)
                         (return-from target nil))))
                 (setf target-result (agent--run-provider-loop agent observer :tools-p nil))))
             (when refusal
               (format t "~&C-REFUSAL separate=~S headroom=~S images=~D input=~D reserve=~D window=~D transports=~D measured=160000~%"
                       separate-p headroom-p (+ prior-count 2)
                       (provider-context-budget-error-estimated-input-tokens refusal)
                       (provider-context-budget-error-output-reserve refusal)
                       (provider-context-budget-error-context-window refusal)
                       (context-admission-test-provider-network-calls provider))
               (test-assert (and (= 8192 (provider-context-budget-error-output-reserve refusal))
                                 (> (provider-context-budget-error-estimated-input-tokens refusal) 600000))
                            "The base failure is the conservative ordinary image estimate, not fixture setup.")
               (test-assert (equal before (mapcar #'json-encode (conversation-input-items conversation)))
                            "A local refusal preserves canonical image evidence."))
             (test-assert (and (null refusal) target-result
                               (= 2 (context-admission-test-provider-network-calls provider)))
                          "Prior measured input plus appended evidence must reach ordinary transport at 600K.")
             (when target-result
               (let ((request (first (context-measured-test-requests provider))))
                 (test-assert (= (+ prior-count 2) (context-measured-tests--image-count request))
                              "Admission preserves all structured images on the actual ordinary wire.")
                 (test-assert (every (lambda (item) (member item (mapcar #'json-encode (conversation-input-items conversation)) :test #'equal)) before)
                              "Successful admission preserves canonical image evidence and surrounding text.")))))))))
  nil)

(-> test-context-measured-current-images () null)
(defun test-context-measured-current-images ()
  "Admit a many-image current exchange after an earned ordinary input measurement."
  (context-measured-tests--run)
  nil)

(-> test-context-measured-headroom-images () null)
(defun test-context-measured-headroom-images ()
  "Reuse the same measurement at early output headroom and final admission."
  (context-measured-tests--run :headroom-p t)
  nil)

(-> test-context-measured-distributed-images () null)
(defun test-context-measured-distributed-images ()
  "Admit recent images distributed across exchanges using their own prior measurement."
  (context-measured-tests--run :separate-p t :headroom-p t)
  nil)


;;;; -- Earned Measurement Controls --

(-> context-measured-tests--earned (function) null)
(defun context-measured-tests--earned (continuation)
  "Run CONTINUATION with a real ordinary receipt earned on synthetic current images."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "900000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "8192"))
    (with-test-configuration (configuration)
      (let* ((configuration                 (configuration-copy configuration :model "gpt-6.1-sol" :provider-validation-p nil))
             (*context-contributors*        nil)
             (*context-resolver*            (cl-llm-provider-api:make-context-resolver))
             (*context-last-deliveries*     (make-hash-table :test #'equal))
             (*context-last-delivery-order* nil)
             (provider                      (make-instance 'context-measured-test-provider :configuration configuration
                                                           :credential-manager                            (credential-manager-create configuration)))
             (conversation                  (conversation-create configuration))
             (agent                         (agent-create :configuration configuration :conversation conversation :provider provider
                                                          :tool-registry (make-instance 'tool-registry)))
             (observer                      (make-instance 'callback-agent-observer :status-callback (lambda (&rest values) (declare (ignore values)) nil))))
        (conversation-append-user-message conversation (make-string 32000 :initial-element #\u))
        (context-measured-tests--append-images conversation 0 35)
        (test-call-with-function-replacements
         (list (list 'credential-manager-credentials
                     (lambda (&rest arguments) (declare (ignore arguments)) (provider-tests--credentials configuration))))
         (lambda ()
           (agent--run-provider-loop agent observer :tools-p nil)
           (let ((*provider-input-measurement-credentials* (provider-tests--credentials configuration))
                 (*provider-input-measurement-route*
                   (provider-input-measurement--route provider (provider-tests--credentials configuration))))
             (funcall continuation (list :provider provider :conversation conversation :agent agent :observer observer
                                         :configuration configuration))))))))
  nil)

(-> context-measured-tests--estimate (model-provider conversation hash-table) (values integer boolean))
(defun context-measured-tests--estimate (provider conversation request)
  "Estimate REQUEST in the exact ordinary owner scope after an earned success."
  (let ((*provider-input-measurement-context* (list :owner conversation :adapter provider)))
    (provider-request-input-token-estimate provider request)))

(-> test-context-measured-estimate-controls () null)
(defun test-context-measured-estimate-controls ()
  "Check measured whole input, fresh delta, payload bounds, overhead and exact mutations."
  (context-measured-tests--earned
   (lambda (fixture)
     (let* ((provider     (getf fixture :provider))
            (conversation (getf fixture :conversation))
            (receipt      (conversation-input-measurement conversation))
            (request      (provider-request-object provider conversation #()))
            (count        (getf receipt :count))
            (unaccounted  (json-object-copy request)))
       (test-assert (conversation-input-measurement-valid-p receipt) "Only a completed ordinary request earns a bounded receipt.")
       (test-assert (= 160000 (getf receipt :input)) "The 120K cache subset neither discounts nor surcharges full160K input.")
       (setf (gethash "input" unaccounted) (subseq (json-get request "input") count))
       (multiple-value-bind (estimate uncertain-p) (context-measured-tests--estimate provider conversation request)
         (test-assert (= estimate (+ 160000 (ceiling (provider-context--request-characters unaccounted) 4))) "Measured input adds the entire generated-output tail and full current overhead.")
         (test-assert uncertain-p "A mixed measured/local estimate retains uncertainty.")
         (let ((overhead (json-decode (json-encode request))))
           (setf (gethash "instructions" overhead) (make-string 40000 :initial-element #\i)
                 (gethash "tools" overhead) (vector (json-object "type" "function" "name" "new_tool" "description" (make-string 40000 :initial-element #\t))))
           (let ((current-cost (json-object-copy overhead)))
             (setf (gethash "input" current-cost) (subseq (json-get overhead "input") count))
             (test-assert (= (context-measured-tests--estimate provider conversation overhead)
                             (+ 160000 (ceiling (provider-context--request-characters current-cost) 4)))
                          "All current instructions and tool definitions are charged without subtracting old overhead.")
             (test-assert (> (context-measured-tests--estimate provider conversation overhead) estimate)
                          "Adding substantial current overhead raises the admission estimate."))
           (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "180000"))
             (let ((*provider-input-measurement-context* (list :owner conversation :adapter provider)))
               (test-assert (handler-case (progn (provider-check-request-context provider overhead) nil)
                              (provider-context-budget-error () t)) "Large current overhead can refuse even with a valid measured prefix.")))))
       (let* ((new-image (json-object "type" "input_image" "image_url" "data:image/png;base64,AA"))
              (tail      (concatenate 'vector (json-get request "input") (vector new-image)))
              (candidate (json-object-copy request)))
         (setf (gethash "input" candidate) tail)
         (let ((small (context-measured-tests--estimate provider conversation candidate)))
           (test-assert (>= (- small (context-measured-tests--estimate provider conversation request)) 16384) "Every unanchored structured image retains the conservative bound.")
           (setf (gethash "image_url" new-image) (make-string 100000 :initial-element #\a))
           (test-assert (= small (context-measured-tests--estimate provider conversation candidate)) "New image payload bytes do not replace the image allowance.")
           (setf (gethash "input" candidate) (concatenate 'vector tail (vector (json-object "type" "input_text" "text" (make-string 100000 :initial-element #\a)))))
           (test-assert (> (context-measured-tests--estimate provider conversation candidate) (+ small 24000)) "Long ordinary text is charged even beside structured images.")))
       (dolist (mutation '(:text :image :role :order :delete :call-id :model))
         (let* ((candidate (json-decode (json-encode request)))
                (items     (json-get candidate "input")))
           (ecase mutation
             (:text
              (setf (gethash "text" (aref (json-get (aref items 0) "content") 0)) (make-string 32000 :initial-element #\v)))
             (:image
              (setf (gethash "image_url" (aref (json-get (aref items 2) "output") 1)) "data:image/png;base64,c3ludGhldGljX"))
             (:role
              (setf (gethash "role" (aref items 0)) "xxxx"))
             (:order
              (rotatef (aref items 1) (aref items 2)))
             (:delete
              (setf (gethash "input" candidate) (subseq items 1)))
             (:call-id
              (setf (gethash "call_id" (aref items 1)) "measured-image-X"))
             (:model
              (setf (gethash "model" candidate) "gpt-6.1-alt")))
           (let ((raw (provider-request-input-token-estimate provider candidate)))
             (test-assert (= raw (context-measured-tests--estimate provider conversation candidate)) "Changed earlier content/order/correlation/model falls back to the full raw estimate.")))))))
  nil)

(-> test-context-measured-durable-controls () null)
(defun test-context-measured-durable-controls ()
  "Reload an earned receipt and reject invalid metadata and unmeasured legacy usage."
  (context-measured-tests--earned
   (lambda (fixture)
     (let* ((provider     (getf fixture :provider))
            (conversation (getf fixture :conversation))
            (receipt      (conversation-input-measurement conversation))
            (loaded       (conversation-load (conversation-pathname conversation)))
            (request      (provider-request-object provider loaded #())))
       (test-assert (equal receipt (conversation-input-measurement loaded)) "Ordinary receipt metadata survives durable reload.")
       (test-assert (equal receipt (getf (rest (conversation--header-record conversation)) :input-measurement)) "Chunk snapshots carry only the bounded receipt.")
       (test-assert (< (context-measured-tests--estimate provider loaded request) 200000) "Reloaded exact-prefix proof enables measured admission.")
       (dolist (bad (list nil '(:version 2) (list :version 99 :input 160000 :count 71 :root (getf receipt :root) :identity (getf receipt :identity) :issued 2 :layout :responses)
                          (list :version 1 :input -1 :count 71 :root "bad" :identity "bad" :issued 2 :layout :responses)))
         (test-assert (not (conversation-input-measurement-valid-p bad)) "Missing, future and malformed metadata is not an anchor."))
       (let ((legacy (conversation-create (getf fixture :configuration) :identifier "old55-usage-no-receipt")))
         (conversation-append-user-message legacy (make-string 32000 :initial-element #\u))
         (context-measured-tests--append-images legacy 0 35)
         (conversation-append-provider-metadata legacy '(:request-number 1 :response-id "old55-success" :usage (("input_tokens" 160000) ("output_tokens" 30) ("cached_input_tokens" 120000)) :request-identity (:family :codex :model "gpt-6.1-sol")))
         (let* ((active (conversation-log-pathname legacy))
                (forms  (conversation--read-records active)))
           (remf (rest (first forms)) :input-measurement)
           (conversation-identifier-migration--write-forms active forms)
           (test-assert (not (member :input-measurement (rest (first (conversation--read-records active)))))
                        "The OLD55-format journal fixture has no new receipt header field."))
         (let ((reloaded (conversation-load (conversation-pathname legacy))))
           (test-assert (null (conversation-input-measurement reloaded)) "Pre-receipt usage alone cannot earn a measured prefix.")
           (test-assert (> (context-measured-tests--estimate provider reloaded (provider-request-object provider reloaded #())) 590000) "Legacy usage without a wire receipt retains conservative image admission."))))))
  nil)

(-> test-context-measured-owner-controls () null)
(defun test-context-measured-owner-controls ()
  "Detached helpers sharing identity/path/cache keys cannot consume an owner's receipt."
  (context-measured-tests--earned
   (lambda (fixture)
     (let* ((provider     (getf fixture :provider))
            (conversation (getf fixture :conversation))
            (request      (provider-request-object provider conversation #()))
            (view         (conversation-compaction-summary-view conversation (conversation-input-items conversation)))
            (calls        (context-admission-test-provider-network-calls provider)))
       (test-assert (null (conversation-input-measurement view)) "Detached views start without the parent's receipt.")
       (test-assert (> (context-measured-tests--estimate provider view request) 590000) "Shared external identity does not imply measured input ownership.")
       (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "600000"))
         (let ((*provider-input-measurement-context* (list :owner conversation :adapter provider)))
           (test-assert (handler-case (progn (provider-open-response-stream provider request :conversation view) nil)
                          (provider-context-budget-error () t)) "Even direct detached transport admission cannot consume the active parent's receipt.")))
       (test-assert (= calls (context-admission-test-provider-network-calls provider)) "Refused helper requests do not reach transport."))))
  nil)


;;;; -- Ordinary Measurement Lifecycle Controls --

(defclass context-measured-lifecycle-provider (context-measured-test-provider)
  ()
  (:documentation "Synthetic failures at the real Responses transport boundary."))

(defclass context-measured-deadline-stream (test-character-input-stream)
  ()
  (:documentation "A response stream whose real close method signals a cleanup deadline."))

(defmethod close ((stream context-measured-deadline-stream) &key abort)
  "Inject SBCL's deadline condition after a complete ordinary response."
  (declare (ignore abort))
  (error 'sb-sys:deadline-timeout :seconds 0.001))

(defparameter *context-measured-transport-hook* nil
  "An owned test barrier called inside the inert transport after admission.")

(defparameter *context-measured-lifecycle-mode* nil
  "Transport fault keyword or final usage object for the current test.")

(defmethod provider-open-response-stream
    ((provider context-measured-lifecycle-provider) (request hash-table)
     &key credentials conversation)
  "Inject failures below admission while retaining the ordinary parser."
  (declare (ignore credentials conversation))
  (when *context-measured-transport-hook* (funcall *context-measured-transport-hook*))
  (if (null *context-measured-lifecycle-mode*)
      (call-next-method)
      (progn
        (incf (context-admission-test-provider-network-calls provider))
        (case *context-measured-lifecycle-mode*
          (:transport
           (error 'simple-error :format-control "Synthetic transport failure"))
          (:retry
           (setf *context-measured-lifecycle-mode* (json-object "input_tokens" 170000 "output_tokens" 30 "total_tokens" 170030))
           (values (make-instance 'test-character-input-stream :source "Retryable HTTP failure") 500 nil))
          (:summary
           (let* ((item   (json-object "type" "message" "role" "assistant" "status" "completed"
                                       "content" (vector (json-object "type" "output_text" "text"
                                                                      (format nil "[Progress]~%Paid synthetic summary.~%[Decisions]~%Continue.~%[Context]~%Owned fixture.~%[Constraints]~%Synthetic only.~%[Next steps]~%Resume.~%[References]~%Fixture.")))))
                  (source (concatenate 'string
                                       (test-sse-event-string (json-object "type" "response.output_item.done" "item" item))
                                       (test-sse-event-string (json-object "type" "response.completed" "response"
                                                                          (json-object "id" "paid-summary" "output" (vector item) "usage"
                                                                                       (json-object "input_tokens" 170000 "output_tokens" 30 "total_tokens" 170030)))))))
             (values (make-instance 'test-character-input-stream :source source) 200 nil)))
          (:http
           (values (make-instance 'test-character-input-stream :source "Synthetic HTTP failure") 500 nil))
          (otherwise
           (let* ((mode   *context-measured-lifecycle-mode*)
                  (usage  (if (hash-table-p mode) mode (json-object "input_tokens" 170000 "output_tokens" 30 "total_tokens" 170030)))
                  (source (test-sse-event-string
                           (json-object "type" (if (eq mode :partial) "response.incomplete" "response.completed")
                                        "response" (json-object "id" "lifecycle-response"
                                                                "output" #()
                                                                "usage" usage)))))
             (values (make-instance (case mode
                                             (:cleanup
                                              'test-failing-close-stream)
                                             (:cleanup-deadline
                                              'context-measured-deadline-stream)
                                             (otherwise
                                              'test-character-input-stream)) :source source)
                     200 nil)))))))

(-> context-measured-tests--event (t) null)
(defun context-measured-tests--event (event)
  "Ignore fixture events without omitting the required callback."
  (declare (ignore event))
  nil)

(-> context-measured-tests--attempt (list &key (:callback (option function))) (values provider-result list))
(defun context-measured-tests--attempt (fixture &key callback)
  "Use runtime's ordinary wrapper and transport, returning its transient ticket."
  (let ((provider (getf fixture :provider))
        (owner    (getf fixture :conversation)))
    (provider-call-with-input-measurement
     owner (lambda () (provider-stream-turn provider owner :tool-namespaces #()
                                             :event-callback                (or callback #'context-measured-tests--event))))))

(-> test-context-measured-failure-lifecycle () null)
(defun test-context-measured-failure-lifecycle ()
  "Only successful final usage after cleanup may replace an earned measurement."
  (context-measured-tests--earned
   (lambda (fixture)
     (let* ((owner                                (getf fixture :conversation))
            (provider                             (getf fixture :provider))
            (*provider-maximum-transient-retries* 0)
            (receipt                              (copy-tree (conversation-input-measurement owner))))
       (change-class provider 'context-measured-lifecycle-provider)
       (dolist (usage (list (json-object) (json-object "input_tokens" 0)
                           (json-object "input_tokens" "170000")
                           (json-object "input_tokens" 170000.5)
                           (json-object "output_tokens" 30 "total_tokens" 170030)
                           (json-object "input_tokens" 170000 "cached_input_tokens" 170001)))
         (let ((*context-measured-lifecycle-mode* usage))
           (multiple-value-bind (result ticket) (context-measured-tests--attempt fixture)
             (test-assert (null ticket) "Missing or malformed final input cannot earn a ticket.")
             (agent--persist-provider-result (getf fixture :agent) result :request-number 2 :input-measurement ticket)))
         (test-assert (equal receipt (conversation-input-measurement owner)) "Invalid usage does not overwrite the receipt."))
       (let ((before (copy-tree (conversation-usage owner))))
         (dolist (mode '(:http :transport :partial :cleanup :cleanup-deadline :downstream :cancel))
           (let ((*context-measured-lifecycle-mode* (unless (member mode '(:downstream :cancel)) mode))
                 (ticket                            nil)
                 (completion-seen                   nil)
                 (calls                             (context-admission-test-provider-network-calls provider)))
             (test-assert
              (handler-case
                  (progn
                    (setf ticket
                          (nth-value 1
                            (if (eq mode :cancel)
                                (block cancelled
                                  (provider-call-with-input-measurement
                                   owner (lambda ()
                                           (provider-stream-turn provider owner :tool-namespaces #()
                                                                 :event-callback                 #'context-measured-tests--event)
                                           (setf completion-seen t)
                                           (return-from cancelled (values nil nil)))))
                                (context-measured-tests--attempt
                                 fixture :callback (when (eq mode :downstream)
                                                     (lambda (event)
                                                       (when (typep event 'provider-completed-event)
                                                        (setf completion-seen t)
                                                        (error 'simple-error :format-control "Synthetic downstream failure"))))))))
                    (not (null (member mode '(:cancel :cleanup :cleanup-deadline)))))
                (error () t))
              "Failed attempts unwind, except best-effort cleanup which may return a paid result.")
             (format t "~&LIFECYCLE mode=~S ticket=~S~%" mode (not (null ticket)))
             (test-assert (null ticket) "No post-callback failure can publish an anchor.")
             (test-assert (equal receipt (conversation-input-measurement owner)) "Failed attempts preserve the prior receipt.")
             (test-assert (= (1+ calls) (context-admission-test-provider-network-calls provider)) "Every fault control reaches its actual transport exactly once.")
             (when (member mode '(:cancel :downstream))
               (test-assert completion-seen "Cancellation/downstream failure occurs after actual completion."))))
         (test-assert (equal before (conversation-usage owner)) "Failed attempts append no paid usage metadata."))
       (let ((*context-measured-lifecycle-mode* (json-object "input_tokens" 170000 "output_tokens" 30 "total_tokens" 170030)))
         (multiple-value-bind (result ticket) (context-measured-tests--attempt fixture)
           (test-assert (not (null ticket)) "A later successful retry earns its own ticket.")
           (agent--persist-provider-result (getf fixture :agent) result :request-number 3 :input-measurement ticket)))
       (test-assert (= 170000 (getf (conversation-input-measurement owner) :input)) "Only the successful retry replaces measured input."))))
  nil)

(-> test-context-measured-publication-lineage () null)
(defun test-context-measured-publication-lineage ()
  "Reject reverse completion and edits between ordinary return and persistence."
  (context-measured-tests--earned
   (lambda (fixture)
     (let ((owner (getf fixture :conversation))
           (agent (getf fixture :agent)))
       (multiple-value-bind (older-result older-ticket) (context-measured-tests--attempt fixture)
         (declare (ignore older-result))
         (multiple-value-bind (result newer-ticket) (context-measured-tests--attempt fixture)
           (agent--persist-provider-result agent result :request-number 3 :input-measurement newer-ticket)
           (test-assert (null (provider-input-measurement-ticket-receipt owner older-ticket)) "An earlier ticket cannot overwrite a newer issuance.")
           (test-assert (= (getf (getf newer-ticket :receipt) :issued) (getf (conversation-input-measurement owner) :issued)) "The newest successful issuance is retained.")))
       (multiple-value-bind (result ticket) (context-measured-tests--attempt fixture)
         (declare (ignore result))
         (setf (conversation-input-items owner) (conversation-input-items owner))
         (test-assert (null (provider-input-measurement-ticket-receipt owner ticket)) "Even a no-op reset revokes in-flight publication."))
       (multiple-value-bind (result ticket) (context-measured-tests--attempt fixture)
         (declare (ignore result))
         (let ((item (first (conversation-input-items owner))))
           (setf (gethash "role" item) "assistant"))
         (test-assert (null (provider-input-measurement-ticket-receipt owner ticket)) "An in-place role edit after return invalidates the ticket.")))))
  nil)

(-> test-context-measured-effective-identity () null)
(defun test-context-measured-effective-identity ()
  "An earned prefix cannot cross effective adapter, endpoint or input interpretation."
  (context-measured-tests--earned
   (lambda (fixture)
     (let* ((owner         (getf fixture :conversation))
            (provider      (getf fixture :provider))
            (request       (provider-request-object provider owner #()))
            (configuration (getf fixture :configuration)))
       (let ((candidate (make-instance 'context-measured-lifecycle-provider :configuration configuration)))
         (multiple-value-bind (estimate reason)
             (let ((*provider-input-measurement-context* (list :owner owner :adapter candidate)))
               (provider-input-measurement--estimate candidate request))
           (test-assert (and (null estimate) (eq reason :identity-changed)) "A changed effective adapter class falls back.")))
       (with-test-environment (("AUTOLITH_PROVIDER_ENDPOINT" "https://synthetic.invalid/responses"))
         (multiple-value-bind (estimate reason)
             (let ((*provider-input-measurement-context* (list :owner owner :adapter provider)))
               (provider-input-measurement--estimate provider request))
           (test-assert (and (null estimate) (eq reason :identity-changed)) "A changed effective endpoint falls back.")))
       (dolist (field '("reasoning" "request"))
         (let ((changed (json-object-copy request)))
           (setf (gethash field changed) (json-object "effort" "changed"))
           (test-assert (null (let ((*provider-input-measurement-context* (list :owner owner :adapter provider)))
                                (provider-input-measurement--estimate provider changed))) "Changed interpretation or unsupported envelope cannot consume the anchor."))))))
  nil)

(-> test-context-measured-volatile-projection () null)
(defun test-context-measured-volatile-projection ()
  "Charge changed one-request context, instructions and tools through product projection."
  (context-measured-tests--earned
   (lambda (fixture)
     (let* ((owner                            (getf fixture :conversation))
            (provider                         (getf fixture :provider))
            (volatile-text                    "Initial one-request context")
            (instruction                      "Small initial instructions.")
            (schemas                          (provider-tests--search-filter-schemas))
            (prefix                           nil)
            (*provider-cache-prefix-function* (lambda (value) (setf prefix value))))
       (test-call-with-function-replacements
        (list (list 'system-prompt (lambda (configuration) (declare (ignore configuration)) instruction))
              (list 'context-tests--next-request
                    (lambda (context)
                      (declare (ignore context))
                      (make-context-contribution :identifier (if (> (length volatile-text) 100) "changed-volatile" "initial-volatile")
                                                 :instruction volatile-text :lifetime ':next-request))))
        (lambda ()
          (register-context-contributor "measured-volatile" 'context-tests--next-request)
          (multiple-value-bind (result ticket) (context-measured-tests--attempt fixture)
            (agent--persist-provider-result (getf fixture :agent) result :request-number 2 :input-measurement ticket))
          (test-assert (plusp (getf prefix :volatile-items 0)) "Actual encoder output contains an excluded volatile suffix.")
          (setf instruction (make-string 40000 :initial-element #\i)
                volatile-text (make-string 4000 :initial-element #\v))
          (let* ((request     (provider-request-object provider owner schemas))
                 (receipt     (conversation-input-measurement owner))
                 (unaccounted (json-object-copy request)))
            (setf (gethash "input" unaccounted) (subseq (json-get request "input") (getf receipt :count)))
            (multiple-value-bind (estimate reason)
                (let ((*provider-input-measurement-context* (list :owner owner :adapter provider)))
                  (provider-input-measurement--estimate provider request))
              (test-assert (eq reason :measured-prefix) "Changing a one-request suffix preserves only the attested stable prefix.")
              (test-assert (= estimate (+ 160000 (ceiling (provider-context--request-characters unaccounted) 4))) "Current projected tools/instructions/tail are all charged.")
              (test-assert (>= (length (json-get request "instructions" "")) 40000) "Current large system instructions are present in the actual request.")
              (test-assert (> (provider-context--request-characters (gethash "input" unaccounted)) 4000) "Current volatile contribution is admitted within the advisory budget and charged in the suffix."))))))))
  nil)

(-> test-context-measured-storage-lifecycle () null)
(defun test-context-measured-storage-lifecycle ()
  "Restore bounded headers and invalidate compaction, including paid rejected publication."
  (context-measured-tests--earned
   (lambda (fixture)
     (let* ((owner                           (getf fixture :conversation))
            (receipt                         (copy-tree (conversation-input-measurement owner)))
            (header                          (conversation--header-record owner))
            (restored                        (conversation--from-header (conversation-pathname owner) (conversation-log-pathname owner) header))
            (history                         (mapcar #'json-encode (conversation-input-items owner)))
            (*provider-cache-request-prefix* '(:volatile-items 1)))
       (test-assert (equal receipt (conversation-input-measurement restored)) "A header-only snapshot restores its validated bounded receipt.")
       (dolist (bad (list '(:version 99) '(:version 1 :input 0)))
         (let ((changed (copy-tree header)))
           (setf (getf (rest changed) :input-measurement) bad)
           (test-assert (null (conversation-input-measurement
                               (conversation--from-header (conversation-pathname owner) (conversation-log-pathname owner) changed))) "Malformed/future persisted headers load without credit.")))
       (conversation-append-provider-metadata owner '(:request-number 2 :usage (("input_tokens" 100) ("output_tokens" 50) ("total_tokens" 150))))
       (let ((paid (copy-tree (conversation-usage owner))))
         (test-assert (handler-case
                         (progn (conversation-append-summary owner "Rejected paid summary"
                                                             :validate-projection (lambda (candidate)
                                                                                    (declare (ignore candidate))
                                                                                    (error 'simple-error :format-control "Synthetic candidate refusal"))) nil)
                       (error () t)) "Failed ordinary candidate validation does not publish a summary.")
         (test-assert (equal history (mapcar #'json-encode (conversation-input-items owner))) "Rejected publication preserves canonical history.")
         (test-assert (equal paid (conversation-usage owner)) "Rejected publication preserves known paid summary usage.")
         (test-assert (equal receipt (conversation-input-measurement owner)) "Rejected replacement does not invalidate the canonical owner's receipt."))
       (multiple-value-bind (result ticket) (context-measured-tests--attempt fixture)
         (declare (ignore result))
         (conversation-append-summary owner "Portable compacted context")
         (test-assert (null (conversation-input-measurement owner)) "Portable compaction invalidates the old measured prefix.")
         (test-assert (null (provider-input-measurement-ticket-receipt owner ticket)) "A precompaction completion cannot publish after chunk rollover."))
       (multiple-value-bind (result ticket) (context-measured-tests--attempt fixture)
         (test-assert ticket "A postcompaction success captures a fresh stable boundary despite inherited volatile counts.")
         (agent--persist-provider-result (getf fixture :agent) result :request-number 3 :input-measurement ticket))
       (let* ((loaded  (conversation-load (conversation-pathname owner)))
              (request (provider-request-object (getf fixture :provider) loaded #())))
         (test-assert (conversation-input-measurement-valid-p (conversation-input-measurement loaded)) "New success in a rotated chunk survives normal journal replay.")
         (test-assert (< (context-measured-tests--estimate (getf fixture :provider) loaded request) 200000) "The replayed new receipt revalidates its own postcompaction wire prefix.")))))
  nil)

(-> test-context-measured-legacy-refusal () null)
(defun test-context-measured-legacy-refusal ()
  "OLD55 usage without a receipt cannot cross actual ordinary admission."
  (context-measured-tests--earned
   (lambda (fixture)
     (let* ((provider (getf fixture :provider))
            (legacy   (conversation-create (getf fixture :configuration) :identifier "old55-real-refusal")))
       (conversation-append-user-message legacy (make-string 32000 :initial-element #\u))
       (context-measured-tests--append-images legacy 0 35)
       (conversation-append-provider-metadata legacy '(:request-number 1 :usage (("input_tokens" 160000) ("output_tokens" 30) ("cached_input_tokens" 120000))))
       (let* ((active (conversation-log-pathname legacy))
              (forms  (conversation--read-records active)))
         (remf (rest (first forms)) :input-measurement)
         (conversation-identifier-migration--write-forms active forms))
       (let* ((loaded  (conversation-load (conversation-pathname legacy)))
              (calls   (context-admission-test-provider-network-calls provider))
              (attempt (copy-list fixture)))
         (setf (getf attempt :conversation) loaded)
         (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "600000"))
           (test-assert (handler-case (progn (context-measured-tests--attempt attempt) nil)
                          (provider-context-budget-error () t)) "Usage-only legacy journal is actually refused, not seeded from status counters."))
         (test-assert (= calls (context-admission-test-provider-network-calls provider)) "Legacy refusal occurs before any target transport.")))))
  nil)

(-> test-context-measured-native-mechanical () null)
(defun test-context-measured-native-mechanical ()
  "Reject receipts across native checkpoints, mechanical bounding and projection rewind."
  (context-measured-tests--earned
   (lambda (fixture)
     (let ((owner (getf fixture :conversation)))
       (multiple-value-bind (result ticket) (context-measured-tests--attempt fixture)
         (declare (ignore result))
         (conversation-append-native-compaction
          owner (list (json-object "type" "context_compaction" "encrypted_content" "synthetic-native-checkpoint"))
          :family ':codex :summary "Synthetic native handoff")
         (test-assert (null (conversation-input-measurement owner)) "Native compaction invalidates ordinary input evidence.")
         (test-assert (null (provider-input-measurement-ticket-receipt owner ticket)) "Native publication revokes the outstanding ticket.")
         (test-assert (null (conversation-input-measurement (conversation-load (conversation-pathname owner)))) "Native invalidation survives chunk reload."))
       (conversation-append-user-message owner "Inspect old evidence")
       (conversation-append-provider-item owner (precompaction-tests--call "measured-large-output"))
       (conversation-append-tool-result owner "measured-large-output" :tool-name "shell.run"
                                        :output (make-string 12000 :initial-element #\e) :success-p t)
       (conversation-append-user-message owner "Next user turn")
       (multiple-value-bind (result ticket) (context-measured-tests--attempt fixture)
         (agent--persist-provider-result (getf fixture :agent) result :request-number 4 :input-measurement ticket))
       (test-assert (conversation-input-measurement-valid-p (conversation-input-measurement owner)) "Post-native ordinary success earns its own receipt.")
       (multiple-value-bind (result ticket) (context-measured-tests--attempt fixture)
         (declare (ignore result))
         (let ((paid (copy-tree (conversation-usage owner))))
           (test-assert (plusp (conversation-precompact owner)) "Mechanical bounding actually changes an old oversized output.")
           (test-assert (null (conversation-input-measurement owner)) "Mechanical replacement clears the old receipt.")
           (test-assert (null (provider-input-measurement-ticket-receipt owner ticket)) "Mechanical replacement revokes in-flight publication.")
           (test-assert (equal paid (conversation-usage owner)) "Mechanical projection changes do not rewrite paid usage.")))
       (multiple-value-bind (result ticket) (context-measured-tests--attempt fixture)
         (agent--persist-provider-result (getf fixture :agent) result :request-number 5 :input-measurement ticket))
       (setf (conversation-input-items owner) (butlast (conversation-input-items owner)))
       (test-assert (null (conversation-input-measurement owner)) "Projection rewind cannot inherit evidence from discarded input."))))
  nil)

(-> test-context-measured-child-ownership () null)
(defun test-context-measured-child-ownership ()
  "A child with inherited history earns only its own ordinary input measurement."
  (context-measured-tests--earned
   (lambda (fixture)
     (let* ((parent         (getf fixture :conversation))
            (provider       (getf fixture :provider))
            (parent-receipt (copy-tree (conversation-input-measurement parent)))
            (child          (conversation-create (getf fixture :configuration) :identifier "measured-child-owner"))
            (child-agent    (agent-create :configuration (getf fixture :configuration) :conversation child :provider provider
                                          :tool-registry (make-instance 'tool-registry)))
            (calls          (context-admission-test-provider-network-calls provider)))
       (setf (conversation-input-items child) (mapcar (lambda (item) (json-decode (json-encode item))) (conversation-input-items parent)))
       (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "600000"))
         (let ((*provider-input-measurement-context* (list :owner parent :adapter provider)))
           (test-assert (handler-case
                           (progn (provider-stream-turn provider child :tool-namespaces #() :goal-context "Parent-owned advisory context"
                                                       :event-callback #'context-measured-tests--event) nil)
                         (provider-context-budget-error () t)) "Goal context and inherited history do not authorize the parent's receipt.")))
       (test-assert (= calls (context-admission-test-provider-network-calls provider)) "The unmeasured child is refused before transport.")
       (let ((attempt (copy-list fixture)))
         (setf (getf attempt :conversation) child)
         (multiple-value-bind (result ticket) (context-measured-tests--attempt attempt)
           (agent--persist-provider-result child-agent result :request-number 1 :input-measurement ticket))
         (test-assert (conversation-input-measurement-valid-p (conversation-input-measurement child)) "A successful ordinary child request earns a child-local receipt.")
         (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "600000"))
           (multiple-value-bind (result ticket) (context-measured-tests--attempt attempt)
             (declare (ignore result))
             (test-assert (not (null ticket)) "Only its own success admits the child's later request."))))
       (test-assert (= (+ calls 2) (context-admission-test-provider-network-calls provider)) "Exactly two child transports follow its own earning request.")
       (test-assert (equal parent-receipt (conversation-input-measurement parent)) "Child completion cannot replace the parent's receipt."))))
  nil)


;;;; -- Retry and Replay Admission Controls --

(-> test-context-measured-automatic-retry () null)
(defun test-context-measured-automatic-retry ()
  "Earn one ticket only after an automatic failed attempt then success in one call."
  (context-measured-tests--earned
   (lambda (fixture)
     (let* ((provider                             (getf fixture :provider))
            (owner                                (getf fixture :conversation))
            (calls                                (context-admission-test-provider-network-calls provider))
            (before                               (copy-list (conversation-usage owner)))
            (receipt                              (copy-tree (conversation-input-measurement owner)))
            (failures                             0)
            (completions                          0)
            (*provider-maximum-transient-retries* 1)
            (*context-measured-lifecycle-mode*    :retry)
            (cl-llm-provider-api:*bounded-retry-sleep-function*
              (lambda (seconds) (declare (ignore seconds)) nil)))
       (change-class provider 'context-measured-lifecycle-provider)
       (multiple-value-bind (result ticket)
           (context-measured-tests--attempt
            fixture :callback (lambda (event)
                                (when (typep event 'provider-attempt-failed-event)
                                  (incf failures)
                                  (test-assert (equal receipt (conversation-input-measurement owner)) "The failed automatic attempt cannot replace its predecessor."))
                                (when (typep event 'provider-completed-event) (incf completions))))
         (test-assert (= failures 1) "Exactly one failed automatic attempt is observed.")
         (test-assert (= completions 1) "Only the successful retry completes.")
         (test-assert (= (+ calls 2) (context-admission-test-provider-network-calls provider)) "Both automatic attempts reach transport in this one ordinary call.")
         (test-assert (equal before (conversation-usage owner)) "No result is published before the ordinary call returns.")
         (test-assert (= 170000 (getf (getf ticket :receipt) :input)) "Only the final retry's complete usage earns a ticket.")
         (agent--persist-provider-result (getf fixture :agent) result :request-number 2 :input-measurement ticket)
         (test-assert (= (+ 170000 (first before)) (first (conversation-usage owner))) "Only the successful paid input is added.")
         (test-assert (= (+ 30 (second before)) (second (conversation-usage owner))) "Only the successful paid output is added.")
         (test-assert (= 170000 (getf (conversation-input-measurement owner) :input)) "The final successful retry replaces the receipt.")))))
  nil)

(-> context-measured-tests--ordinary-refusal (model-provider conversation) null)
(defun context-measured-tests--ordinary-refusal (provider owner)
  "Check conservative fallback at real ordinary admission with no transport."
  (let ((calls (context-admission-test-provider-network-calls provider)))
    (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "600000"))
      (test-assert
       (handler-case
           (progn
             (provider-call-with-input-measurement
              owner (lambda () (provider-stream-turn provider owner :tool-namespaces #()
                                                     :event-callback                 #'context-measured-tests--event)))
             nil)
         (provider-context-budget-error (condition)
           (> (+ (provider-context-budget-error-estimated-input-tokens condition)
                 (provider-context-budget-error-output-reserve condition)) 600000)))
       "Invalid evidence falls back and refuses at actual ordinary admission."))
    (test-assert (= calls (context-admission-test-provider-network-calls provider)) "Fallback refuses before any transport."))
  nil)

(-> test-context-measured-replay-admission () null)
(defun test-context-measured-replay-admission ()
  "Exercise malformed/future metadata replay and changed loaded-owner admission."
  (context-measured-tests--earned
   (lambda (fixture)
     (let* ((provider (getf fixture :provider))
            (owner    (getf fixture :conversation))
            (receipt  (copy-tree (conversation-input-measurement owner))))
       (dolist (bad (list '(:version 99) '(:version 1 :input 0)))
         (let ((fresh (conversation-create (getf fixture :configuration))))
           (dolist (item (conversation-input-items owner))
             (conversation-append-provider-item fresh (json-decode (json-encode item))))
           (conversation-append-provider-metadata fresh (list :request-number 1 :input-measurement bad
                                                             :usage '(("input_tokens" 160000) ("output_tokens" 30))))
           (let ((loaded (conversation-load (conversation-pathname fresh))))
             (test-assert (null (conversation-input-measurement loaded)) "Malformed/future metadata replay earns no receipt.")
             (context-measured-tests--ordinary-refusal provider loaded)))
         (conversation-append-provider-metadata owner (list :request-number 2 :input-measurement bad))
         (test-assert (equal receipt (conversation-input-measurement (conversation-load (conversation-pathname owner)))) "Malformed/future metadata cannot overwrite a valid durable receipt."))
       (let ((loaded (conversation-load (conversation-pathname owner))))
         (setf (gethash "role" (first (conversation-input-items loaded))) "assistant")
         (context-measured-tests--ordinary-refusal provider loaded))
       (let ((loaded (conversation-load (conversation-pathname owner))))
         (change-class provider 'context-measured-lifecycle-provider)
         (context-measured-tests--ordinary-refusal provider loaded)))))
  nil)

(-> test-context-measured-overlapping-completions () null)
(defun test-context-measured-overlapping-completions ()
  "Complete genuinely overlapping ordinary transports in reverse issuance order."
  (context-measured-tests--earned
   (lambda (fixture)
     (let* ((provider   (getf fixture :provider))
            (owner      (getf fixture :conversation))
            (before     (copy-list (conversation-usage owner)))
            (calls      (context-admission-test-provider-network-calls provider))
            (lock       (make-lock "Measured transport barrier"))
            (condition  (make-condition-variable))
            (entered    nil)
            (released   nil)
            (done       nil)
            (thread     nil)
            (failure    nil)
            (old-result nil)
            (old-ticket nil))
       (change-class provider 'context-measured-lifecycle-provider)
       (labels ((wait-for (predicate)
                  "Wait for one owned state change with a bounded deadline."
                  (let ((deadline (+ (get-internal-real-time) (* 10 internal-time-units-per-second))))
                    (with-lock-held (lock)
                      (loop until (funcall predicate)
                            do (when (or (>= (get-internal-real-time) deadline)
                                         (not (condition-wait condition lock :timeout 1)))
                                 (when (>= (get-internal-real-time) deadline)
                                   (error 'simple-error :format-control "Owned measurement barrier timed out")))))))

                (release ()
                  "Release only this test's blocked transport."
                  (with-lock-held (lock)
                    (setf released t)
                    (condition-notify condition))))
         (unwind-protect
              (progn
                (setf thread
                      (make-thread
                       (lambda ()
                         (let ((*context-contributors*            nil)
                               (*context-resolver*                (cl-llm-provider-api:make-context-resolver))
                               (*context-last-deliveries*         (make-hash-table :test #'equal))
                               (*context-last-delivery-order*     nil)
                               (*context-measured-lifecycle-mode* (json-object "input_tokens" 180000 "output_tokens" 30 "total_tokens" 180030))
                               (*context-measured-transport-hook*
                                 (lambda ()
                                   (with-lock-held (lock)
                                     (setf entered t)
                                     (condition-notify condition))
                                   (wait-for (lambda () released)))))
                           (unwind-protect
                                (handler-case
                                    (multiple-value-setq (old-result old-ticket) (context-measured-tests--attempt fixture))
                                  (error (caught) (setf failure caught)))
                             (with-lock-held (lock)
                               (setf done t)
                               (condition-notify condition)))))
                       :name "Owned measured older ordinary transport"))
                (wait-for (lambda () entered))
                (test-assert (thread-alive-p thread) "The older ordinary request is still inside transport when the newer one starts.")
                (let ((*context-measured-lifecycle-mode* (json-object "input_tokens" 170000 "output_tokens" 30 "total_tokens" 170030)))
                  (multiple-value-bind (result ticket) (context-measured-tests--attempt fixture)
                    (test-assert ticket "The newer overlapping request earns its own ticket.")
                    (agent--persist-provider-result (getf fixture :agent) result :request-number 3 :input-measurement ticket)))
                (let ((newer (copy-tree (conversation-input-measurement owner))))
                  (release)
                  (wait-for (lambda () done))
                  (join-thread thread)
                  (when failure (error failure))
                  (test-assert (= 180000 (json-get (provider-result-usage old-result) "input_tokens")) "The older request completes normally after the newer request.")
                  (test-assert (null old-ticket) "A late older completion receives no publishable ticket.")
                  (agent--persist-provider-result (getf fixture :agent) old-result :request-number 2 :input-measurement old-ticket)
                  (test-assert (equal newer (conversation-input-measurement owner)) "Late older paid metadata cannot replace the newer receipt.")
                  (test-assert (= 170000 (getf newer :input)) "The latest issuance owns the retained measured input.")
                  (test-assert (= (+ calls 2) (context-admission-test-provider-network-calls provider)) "Exactly two overlapping ordinary transports complete.")
                  (test-assert (= (+ (first before) 170000 180000) (first (conversation-usage owner))) "Both known paid inputs are preserved despite rejected old credit.")))
           (release)
           (when (and thread (thread-alive-p thread))
             (handler-case (wait-for (lambda () done))
               (error () (destroy-thread thread))))
           (when thread (join-thread thread)))))))
  nil)


;;;; -- Measured Admission with Paid Compaction and Missions --

(defmethod provider-native-compact-conversation
    ((provider context-measured-lifecycle-provider) (conversation conversation)
     &key tool-namespaces event-callback)
  "Select portable compaction for this owned transport fixture."
  (declare (ignore provider conversation tool-namespaces event-callback))
  (values nil nil))

(-> test-context-measured-paid-summary-rejection () null)
(defun test-context-measured-paid-summary-rejection ()
  "Preserve real paid summary usage when actual candidate admission rejects publication."
  (context-measured-tests--earned
   (lambda (fixture)
     (let* ((provider                             (getf fixture :provider))
            (owner                                (getf fixture :conversation))
            (before                               (copy-list (conversation-usage owner)))
            (history                              (mapcar #'json-encode (conversation-input-items owner)))
            (receipt                              (copy-tree (conversation-input-measurement owner)))
            (calls                                (context-admission-test-provider-network-calls provider))
            (paid                                 nil)
            (*context-measured-lifecycle-mode*    :summary)
            (*provider-input-measurement-context* (list :owner owner :adapter provider)))
       (change-class provider 'context-measured-lifecycle-provider)
       (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "900000"))
         (let ((observer
                 (make-instance 'callback-agent-observer
                                :status-callback
                                (lambda (status details)
                                  (declare (ignore details))
                                  (when (eq status :compaction-request-completed)
                                    (setf paid (copy-list (conversation-usage owner)))
                                    (platform-setenv "AUTOLITH_CONTEXT_WINDOW" "100"))
                                  nil))))
           (test-assert
            (handler-case
                (progn (agent-compact-conversation (getf fixture :agent) observer :tools-p nil) nil)
              (provider-context-budget-error () t))
            "After actual paid summary completion the real ordinary candidate validator refuses publication.")))
       (test-assert paid "The paid summary completion observer ran before rejection.")
       (test-assert (= (+ calls 1) (context-admission-test-provider-network-calls provider)) "Exactly one actual summary transport completes; no candidate transport occurs.")
       (test-assert (= (+ 170000 (first before)) (first (conversation-usage owner))) "Known paid summary input survives rejected publication.")
       (test-assert (= (+ 30 (second before)) (second (conversation-usage owner))) "Known paid summary output survives rejected publication.")
       (test-assert (equal history (mapcar #'json-encode (conversation-input-items owner))) "The rejected real summary never replaces canonical history.")
       (test-assert (equal receipt (conversation-input-measurement owner)) "The detached paid summary cannot overwrite its parent's ordinary receipt.")
       (let ((loaded (conversation-load (conversation-pathname owner))))
         (test-assert (equal (conversation-usage owner) (conversation-usage loaded)) "Known paid summary usage survives reload after refusal.")
         (test-assert (equal history (mapcar #'json-encode (conversation-input-items loaded))) "Reload contains no rejected replacement.")
         (test-assert (equal receipt (conversation-input-measurement loaded)) "Reload retains only the parent's ordinary receipt.")))))
  nil)

(-> test-context-measured-mission-accounting () null)
(defun test-context-measured-mission-accounting ()
  "Use real mission reservation, known settlement, UNKNOWN quarantine and refusal."
  (context-measured-tests--earned
   (lambda (fixture)
     (let* ((provider    (getf fixture :provider))
            (owner       (getf fixture :conversation))
            (application (mission-inference-test--application (getf fixture :configuration) provider)))
       (change-class provider 'context-measured-lifecycle-provider)
       (multiple-value-bind (result ticket) (context-measured-tests--attempt fixture)
         (agent--persist-provider-result (getf fixture :agent) result :request-number 2 :input-measurement ticket))
       (setf (application-conversation application) owner
             (application-agent application) (getf fixture :agent))
       (application-mission-start application (mission-test--specification :turns 5 :tokens 10000000))
       (unwind-protect
            (let* ((context  (mission-context-find owner))
                   (goal     (mission-context-goal context))
                   (reserved nil)
                   (calls    (context-admission-test-provider-network-calls provider)))
              ;; Model an engaged primary turn, including its post-UNKNOWN admission.
              (mission-context-bind context (getf fixture :agent))
              (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "600000"))
                (let ((*context-measured-lifecycle-mode* (json-object "input_tokens" 170000 "output_tokens" 30 "total_tokens" 170030))
                      (*context-measured-transport-hook*
                        (lambda ()
                          (setf reserved (list (getf goal :requests-outstanding) (getf goal :tokens-reserved))))))
                  (multiple-value-bind (result ticket) (context-measured-tests--attempt fixture)
                    (test-assert ticket "The exact mission owner consumes its own earned prefix at 600K.")
                    (agent--persist-provider-result (getf fixture :agent) result :request-number 3 :input-measurement ticket)))
                (test-assert (and (= 1 (first reserved)) (plusp (second reserved))) "The actual admitted transport holds one finite outstanding reservation.")
                (test-assert (and (= 170030 (getf goal :tokens-used)) (zerop (getf goal :requests-outstanding)) (zerop (getf goal :tokens-reserved))) "Known mission usage settles once without shrinking the reservation policy.")
                (let ((receipt                           (copy-tree (conversation-input-measurement owner)))
                      (known                             (copy-list (conversation-usage owner)))
                      (*context-measured-lifecycle-mode* (json-object)))
                  (multiple-value-bind (result ticket) (context-measured-tests--attempt fixture)
                    (test-assert (null ticket) "A successful transport with missing usage earns no ticket under a mission.")
                    (agent--persist-provider-result (getf fixture :agent) result :request-number 4 :input-measurement ticket))
                  (test-assert (and (= 1 (getf goal :unknown-usage)) (eq :blocked (getf goal :status)) (zerop (getf goal :requests-outstanding)) (zerop (getf goal :tokens-reserved))) "UNKNOWN uses the actual mission quarantine and releases the finite reservation.")
                  (test-assert (equal known (conversation-usage owner)) "Missing usage cannot become measured paid totals.")
                  (test-assert (equal receipt (conversation-input-measurement owner)) "UNKNOWN cannot replace the valid ordinary receipt.")
                  (test-assert (handler-case (progn (context-measured-tests--attempt fixture) nil) (mission-error () t)) "A quarantined mission refuses another ordinary attempt.")
                  (test-assert (= (+ calls 2) (context-admission-test-provider-network-calls provider)) "Only the known and missing-usage attempts reach transport."))))
         (mission-detach application)))))
  nil)

(-> test-context-measured-child-accounting () null)
(defun test-context-measured-child-accounting ()
  "Charge an admitted child's own measured requests through finite mission accounting."
  (context-measured-tests--earned
   (lambda (fixture)
     (let* ((parent         (getf fixture :conversation))
            (provider       (getf fixture :provider))
            (configuration  (getf fixture :configuration))
            (parent-receipt (copy-tree (conversation-input-measurement parent)))
            (parent-usage   (copy-list (conversation-usage parent)))
            (child          (conversation-create configuration :identifier "measured-accounted-child"))
            (child-agent    (agent-create :configuration configuration :conversation child :provider provider
                                          :tool-registry (make-instance 'tool-registry)))
            (attempt        (copy-list fixture))
            (application    (mission-inference-test--application configuration provider))
            (orchestrator   (task-orchestrator-create))
            (job            (task-tests--make-job orchestrator :identifier  "measured-finite-child"
                                                              :parent-agent (getf fixture :agent))))
       (change-class provider 'context-measured-lifecycle-provider)
       (setf (conversation-input-items child) (mapcar (lambda (item) (json-decode (json-encode item))) (conversation-input-items parent)))
       (setf (getf attempt :conversation) child
             (application-conversation application) parent
             (application-agent application) (getf fixture :agent))
       (application-mission-start application (mission-test--specification :turns 8 :tokens 10000000))
       (let* ((context (mission-context-find parent))
              (goal    (mission-context-goal context))
              (calls   (context-admission-test-provider-network-calls provider))
              (held    nil))
         (setf (slot-value job 'mission-context) context)
         (mission-context-bind context job)
         (mission-context-inherit parent child)
         (unwind-protect
              ;; Replace only the synthetic child's body, retaining actual child admission,
              ;; supervision, ordinary transport, reservation and settlement implementations.
              (test-call-with-function-replacements
               (list (list 'task-job--run
                           (lambda (running-job)
                             (test-assert (and (eq job running-job) (eq job *mission-admitted-operation*)) "The real child runner supplies the exact admitted job identity.")
                             (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "600000"))
                               (test-assert (handler-case (progn (context-measured-tests--attempt attempt) nil) (provider-context-budget-error () t)) "An admitted but unmeasured child cannot borrow its parent's receipt."))
                             (test-assert (and (= calls (context-admission-test-provider-network-calls provider)) (zerop (getf goal :tokens-used))) "Child local refusal has zero paid usage and no transport.")
                             (multiple-value-bind (result ticket) (context-measured-tests--attempt attempt)
                               (agent--persist-provider-result child-agent result :request-number 1 :input-measurement ticket))
                             ;; The inherited mission billable policy discounts cached reads;
                             ;; ordinary usage and the admission receipt retain full input.
                             (test-assert (and (conversation-input-measurement-valid-p (conversation-input-measurement child))
                                               (= 160000 (getf (conversation-input-measurement child) :input))
                                               (equal '(160000 30 160030 120000 0 t) (conversation-usage child))
                                               (= 40030 (getf goal :tokens-used))) "Child earning retains full input and follows the existing cached-read billable policy.")
                             (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "600000"))
                               (let ((*context-measured-lifecycle-mode* (json-object "input_tokens" 170000 "output_tokens" 30 "total_tokens" 170030
                                                                                                  "input_tokens_details" (json-object "cached_tokens" 120000)))
                                     (*context-measured-transport-hook*
                                       (lambda ()
                                         (setf held (list (getf goal :requests-outstanding) (getf goal :tokens-reserved)
                                                          *provider-mission-output-allowance* *mission-admitted-operation*)))))
                                 (multiple-value-bind (result ticket) (context-measured-tests--attempt attempt)
                                   (test-assert ticket "Only the child's own receipt admits its ordinary request at 600K.")
                                   (agent--persist-provider-result child-agent result :request-number 2 :input-measurement ticket)))
                               (test-assert (and (= 1 (first held)) (plusp (second held)) (= (second held) (third held)) (eq job (fourth held))) "The actual child transport holds its finite reservation under the exact child permit.")
                               (test-assert (and (= 90060 (getf goal :tokens-used)) (zerop (getf goal :tokens-reserved)) (zerop (getf goal :requests-outstanding))) "Known child requests settle once under the existing billable policy, without charging the output reserve.")
                               (test-assert (and (= 170000 (getf (conversation-input-measurement child) :input))
                                                 (equal '(330000 60 330060 240000 0 t) (conversation-usage child))) "Child receipts and known usage count full input with each cached subset included once.")
                               (let ((receipt                           (copy-tree (conversation-input-measurement child)))
                                     (known                             (copy-list (conversation-usage child)))
                                     (*context-measured-lifecycle-mode* (json-object)))
                                 (multiple-value-bind (result ticket) (context-measured-tests--attempt attempt)
                                   (test-assert (null ticket) "Missing child usage earns no replacement ticket.")
                                   (agent--persist-provider-result child-agent result :request-number 3 :input-measurement ticket))
                                 (let ((charge (getf goal :reservation-charged-tokens)))
                                   (test-assert (and (plusp charge) (= (+ 90060 charge) (getf goal :tokens-used)) (= 1 (mission-task-job-unknown-usage job))
                                                     (zerop (getf goal :unknown-usage)) (zerop (getf goal :tokens-reserved)) (zerop (getf goal :requests-outstanding))) "Missing usage uses the child's actual finite allocation and child UNKNOWN audit once.")
                                   (format t "~&MEASURED-CHILD known-total=330060 full-input=330000 output=60 cached-subset-total=240000 billable=90060 held=~D unknown-charge=~D child-unknown=~D~%"
                                           (second held) charge (mission-task-job-unknown-usage job)))
                                 (test-assert (and (equal known (conversation-usage child)) (equal receipt (conversation-input-measurement child))) "Child UNKNOWN preserves known conversation usage and its valid receipt."))
                               (mission--transition context :paused "Pause the owned child fixture." :cancel-p nil)
                               (test-assert (handler-case (progn (context-measured-tests--attempt attempt) nil) (mission-error () t)) "A receipt cannot bypass a paused child's mission admission.")
                               (test-assert (= (+ calls 3) (context-admission-test-provider-network-calls provider)) "Only earning, known and missing-usage child requests reach transport."))
                             nil)))
               (lambda () (mission-task-job-run job)))
           (mission-detach application)
           (task-orchestrator-close orchestrator)))
       (test-assert (and (equal parent-receipt (conversation-input-measurement parent)) (equal parent-usage (conversation-usage parent))) "Child accounting and measurement never modify the parent's conversation receipt or paid usage."))))
  nil)


;;;; -- Credential-Owned Routing --

(defclass context-route-test-provider (codex-subscription-provider) ()
  (:documentation "Real Codex transport with a first local-refusal observer."))

(defmethod provider-open-response-stream :around
    ((provider context-route-test-provider) (request hash-table) &key credentials conversation)
  "Observe the first real refusal before ordinary recovery can issue a summary."
  (declare (ignore request credentials conversation))
  (handler-bind ((provider-context-budget-error
                   (lambda (condition)
                     (when *context-measured-test-refusal-function*
                       (funcall *context-measured-test-refusal-function* condition)))))
    (call-next-method)))

(-> context-route-tests--credentials (configuration t string) oauth-credentials)
(defun context-route-tests--credentials (configuration account bearer)
  "Create synthetic credentials without consulting a credential store."
  (make-instance 'oauth-credentials :access-token bearer :refresh-token "route-fixture-refresh"
                 :id-token "route-fixture-id" :account-id account :expires-at nil
                 :source-path (configuration-auth-path configuration)))

(-> context-route-tests--run (t boolean boolean) null)
(defun context-route-tests--run (next-account headroom-p admitted-p)
  "Reload a real ordinary receipt and exercise the actual Codex header transport."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "900000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "8192"))
    (with-test-configuration (configuration)
      (let* ((configuration                 (configuration-copy configuration :model "gpt-6.1-sol" :provider-validation-p nil))
             (*context-contributors*        nil)
             (*context-resolver*            (cl-llm-provider-api:make-context-resolver))
             (*context-last-deliveries*     (make-hash-table :test #'equal))
             (*context-last-delivery-order* nil)
             (provider                      (make-instance 'context-route-test-provider :configuration configuration
                                                           :credential-manager                         (credential-manager-create configuration)))
             (owner                         (conversation-create configuration))
             (agent                         (agent-create :configuration configuration :conversation owner :provider provider
                                                          :tool-registry (make-instance 'tool-registry)))
             (observer                      (make-instance 'callback-agent-observer :status-callback
                                                          (lambda (&rest values) (declare (ignore values)) nil)))
             (credentials                   (context-route-tests--credentials configuration "route-fixture-account-A" "route-fixture-bearer-one"))
             (routes                        nil)
             (requests                      nil))
        (conversation-append-user-message owner (make-string 32000 :initial-element #\u))
        (context-measured-tests--append-images owner 0 35)
        (test-call-with-function-replacements
         (list (list 'credential-manager-credentials
                     (lambda (&rest arguments) (declare (ignore arguments)) credentials))
               (list 'provider-post-event-stream
                     (lambda (endpoint bytes &key headers keep-alive-p)
                       (declare (ignore endpoint keep-alive-p))
                       ;; Observe precisely the headers produced by the real Codex primary method.
                       (push (provider-input-measurement--digest (rest (assoc "ChatGPT-Account-ID" headers :test #'equal))) routes)
                       (push (json-decode (babel:octets-to-string bytes :encoding :utf-8)) requests)
                       (let* ((item     (json-object "type" "message" "id" "route-output" "role" "assistant" "status" "completed"
                                                     "content" (vector (json-object "type" "output_text" "text" "Synthetic route response."))))
                              (response (json-object "id" "route-response" "status" "completed" "output" (vector item)
                                                     "usage" (json-object "input_tokens" 160000 "output_tokens" 30 "total_tokens" 160030
                                                                          "input_tokens_details" (json-object "cached_tokens" 120000)))))
                         (values (make-instance 'test-character-input-stream :source
                                                (concatenate 'string
                                                             (test-sse-event-string (json-object "type" "response.created" "response" response))
                                                             (test-sse-event-string (json-object "type" "response.output_item.done" "item" item))
                                                             (test-sse-event-string (json-object "type" "response.completed" "response" response))))
                                 200 nil)))))
         (lambda ()
           (agent--run-provider-loop agent observer :tools-p nil)
           (test-assert (= 1 (length routes)) "Only the successful earning request reached the real header transport.")
           (test-assert (= 160000 (getf (conversation-input-measurement owner) :input)) "Complete normalized input earns the route's receipt.")
           (let* ((loaded                              (conversation-load (conversation-pathname owner)))
                  (receipt                             (copy-tree (conversation-input-measurement loaded)))
                  (next-agent                          (agent-create :configuration configuration :conversation loaded :provider provider
                                                                     :tool-registry (make-instance 'tool-registry)))
                  (*provider-mission-output-allowance* (when headroom-p 8192)))
             (test-assert (equal receipt (conversation-input-measurement owner)) "Ordinary replay restores the same receipt before route comparison.")
             (test-assert (= 160030 (getf (conversation-context-usage-sample loaded) :tokens))
                          "Disk resume retains upstream input-plus-output coverage separately from full input.")
             (let* ((*provider-context-request*               (provider-context-request-create provider loaded #()))
                    (*provider-input-measurement-credentials* credentials)
                    (*provider-input-measurement-route*       (provider-input-measurement--route provider credentials))
                    (request                                  (provider-request-object provider loaded #())))
               (test-assert (nth-value 1 (provider-context--matched-input provider request))
                            "The ordinary upstream path independently earns matched credit before account mutation."))
             (context-measured-tests--append-images loaded 35 2)
             (platform-setenv "AUTOLITH_CONTEXT_WINDOW" "600000")
             (setf credentials (context-route-tests--credentials configuration next-account "route-fixture-bearer-two"))
             (let ((result (catch 'route-refusal
                             (let ((*context-measured-test-refusal-function*
                                     (lambda (condition) (throw 'route-refusal condition))))
                               (agent--run-provider-loop next-agent observer :tools-p nil)))))
               (test-assert (eq admitted-p (not (typep result 'provider-context-budget-error))) "Only a proven equivalent account route may consume the loaded receipt.")
               (test-assert (= (if admitted-p 2 1) (length routes)) "Changed or unknown routing refuses before any target transport.")
               (when admitted-p
                 (test-assert (equal (first routes) (second routes)) "Bearer refresh preserves the actual stable account-routing header."))
               (unless admitted-p
                 (test-assert (equal receipt (conversation-input-measurement loaded)) "Route refusal preserves the prior valid receipt.")))
             (provider-tests--assert-credential-free
              (list receipt (conversation-input-measurement loaded)
                    (conversation--header-record loaded)
                    (uiop:read-file-string (conversation-log-pathname loaded)))
              '("route-fixture-account-A" "route-fixture-account-B" "route-fixture-bearer-one" "route-fixture-bearer-two"
                "route-fixture-refresh" "route-fixture-id" "ChatGPT-Account-ID" "Authorization")
              "Routing proofs and ordinary journals contain no raw routing headers or credentials.")))))))
  nil)

(-> test-context-measured-route-switch () null)
(defun test-context-measured-route-switch ()
  "Changed synthetic account routing cannot borrow a replayed ordinary receipt."
  (context-route-tests--run "route-fixture-account-B" nil nil)
  nil)

(-> test-context-measured-route-headroom () null)
(defun test-context-measured-route-headroom ()
  "Early headroom estimation must reject changed credential-owned routing."
  (context-route-tests--run "route-fixture-account-B" t nil)
  nil)

(-> test-context-measured-route-refresh () null)
(defun test-context-measured-route-refresh ()
  "A bearer refresh on the same attested account route retains measurement credit."
  (context-route-tests--run "route-fixture-account-A" nil t)
  (context-route-tests--run "route-fixture-account-A" t t)
  nil)

(-> test-context-measured-route-unknown () null)
(defun test-context-measured-route-unknown ()
  "Absent, blank or malformed account routing falls back before transport."
  (dolist (account '(nil "" " " "bad route"))
    (context-route-tests--run account nil nil)
    (context-route-tests--run account t nil))
  nil)

(-> test-context-measured-route-completion-change () null)
(defun test-context-measured-route-completion-change ()
  "Revoke a final-wire proof when credential-owned routing changes during parsing."
  (dolist (change-p '(nil t))
    (context-measured-tests--earned
     (lambda (fixture)
       (let* ((owner         (getf fixture :conversation))
              (configuration (getf fixture :configuration))
              (prior         (copy-tree (conversation-input-measurement owner)))
              (reader        (fdefinition 'oauth-credentials-account-id))
              (account       (funcall reader (provider-tests--credentials configuration)))
              (completions   0))
         (test-call-with-function-replacements
          (list (list 'oauth-credentials-account-id
                      (lambda (credentials) (declare (ignore credentials)) account)))
          (lambda ()
            (multiple-value-bind (result ticket)
                (context-measured-tests--attempt
                 fixture :callback
                 (lambda (event)
                   (when (typep event 'provider-completed-event)
                     (incf completions)
                     (when change-p (setf account "route-fixture-account-B")))))
              (test-assert (= 1 completions) "The route change happens in the actual terminal streaming callback.")
              (test-assert (= 160000 (json-get (provider-usage-normalize (provider-result-usage result)) "input_tokens")) "The successful response keeps its complete paid usage even when its route proof is revoked.")
              (test-assert (eq change-p (null ticket)) "Only unchanged routing may issue a post-cleanup publication ticket.")
              (provider-tests--assert-credential-free
               ticket '("provider-test-account-a0542e" "provider-test-access-7f386d" "route-fixture-account-B" "ChatGPT-Account-ID")
               "A transient publication ticket retains no raw account or credential headers.")
              (agent--persist-provider-result (getf fixture :agent) result :request-number 2 :input-measurement ticket)
              (when change-p
                (test-assert (equal prior (conversation-input-measurement owner)) "A mid-response route change cannot replace the valid prior receipt.")))))))))
  nil)

(-> context-boundary-tests--run (keyword boolean boolean) null)
(defun context-boundary-tests--run (stage headroom-p admitted-p)
  "Reload a real ordinary receipt and exercise the actual Codex header transport."
  (let ((*provider-image-token-estimate* 16384))
   (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "900000")
                           ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "8192"))
     (with-test-configuration (configuration)
       (let* ((configuration                 (configuration-copy configuration :model "gpt-6.1-sol" :provider-validation-p nil))
              (*context-contributors*        nil)
              (*context-resolver*            (cl-llm-provider-api:make-context-resolver))
              (*context-last-deliveries*     (make-hash-table :test #'equal))
              (*context-last-delivery-order* nil)
              (provider                      (make-instance 'context-route-test-provider :configuration configuration
                                                            :credential-manager                         (credential-manager-create configuration)))
              (owner                         (conversation-create configuration))
              (agent                         (agent-create :configuration configuration :conversation owner :provider provider
                                                           :tool-registry (make-instance 'tool-registry)))
              (observer                      (make-instance 'callback-agent-observer :status-callback
                                                           (lambda (&rest values) (declare (ignore values)) nil)))
              (credentials                   (context-route-tests--credentials configuration "route-fixture-account-A" "route-fixture-bearer-one"))
              (routes                        nil)
              (requests                      nil)
              (armed                         nil)
              (mutated                       nil)
              (old-observe                   (symbol-function 'provider-cache-prefix-observe))
              (old-prepare                   (symbol-function 'provider-prepare-stream-request)))
         (conversation-append-user-message owner (make-string 32000 :initial-element #\u))
         (context-measured-tests--append-images owner 0 35)
         (test-call-with-function-replacements
          (append
            (when (eq stage :prepare)
              (list (list 'provider-prepare-stream-request
                          (lambda (p r c)
                            (let ((final (funcall old-prepare p r c)))
                              (when armed (funcall armed final))
                             final)))))
            (list (list 'provider-cache-prefix-observe
                        (lambda (request)
                          (funcall old-observe request)
                          (when (and armed (eq stage :observer)) (funcall armed request)))))
            (list (list 'credential-manager-credentials
                      (lambda (&rest arguments) (declare (ignore arguments)) credentials))
                (list 'provider-post-event-stream
                      (lambda (endpoint bytes &key headers keep-alive-p)
                        (declare (ignore endpoint keep-alive-p))
                        ;; Observe precisely the headers produced by the real Codex primary method.
                        (push (provider-input-measurement--digest (rest (assoc "ChatGPT-Account-ID" headers :test #'equal))) routes)
                        (push (json-decode (babel:octets-to-string bytes :encoding :utf-8)) requests)
                        (let* ((item     (json-object "type" "message" "id" "route-output" "role" "assistant" "status" "completed"
                                                      "content" (vector (json-object "type" "output_text" "text" "Synthetic route response."))))
                               (response (json-object "id" "route-response" "status" "completed" "output" (vector item)
                                                      "usage" (json-object "input_tokens" 160000 "output_tokens" 30 "total_tokens" 160030
                                                                           "input_tokens_details" (json-object "cached_tokens" 120000)))))
                          (values (make-instance 'test-character-input-stream :source
                                                 (concatenate 'string
                                                              (test-sse-event-string (json-object "type" "response.created" "response" response))
                                                              (test-sse-event-string (json-object "type" "response.output_item.done" "item" item))
                                                              (test-sse-event-string (json-object "type" "response.completed" "response" response))))
                                  200 nil))))))
          (lambda ()
            (agent--run-provider-loop agent observer :tools-p nil)
            (test-assert (= 1 (length routes)) "Only the successful earning request reached the real header transport.")
            (test-assert (= 160000 (getf (conversation-input-measurement owner) :input)) "Complete normalized input earns the route's receipt.")
            (let* ((loaded                              (conversation-load (conversation-pathname owner)))
                   (receipt                             (copy-tree (conversation-input-measurement loaded)))
                   (next-agent                          (agent-create :configuration configuration :conversation loaded :provider provider
                                                                      :tool-registry (make-instance 'tool-registry)))
                   (*provider-mission-output-allowance* (when headroom-p 8192)))
              (test-assert (equal receipt (conversation-input-measurement owner)) "Ordinary replay restores the same receipt before route comparison.")
              (context-measured-tests--append-images loaded 35 2)
              (platform-setenv "AUTOLITH_CONTEXT_WINDOW" "600000")
              (setf armed
                      (lambda (request)
                        (setf armed nil mutated t)
                        (let* ((measured (provider-request-input-token-estimate provider request))
                               (raw      (let ((*provider-input-measurement-context* nil)
                                               (*provider-context-request*           nil))
                                          (provider-request-input-token-estimate provider request)))
                               (reserve  (provider-request-output-reserve provider request)))
                          (format t "BOUNDARY stage=~S headroom=~S same-account=~S measured=~D raw=~D reserve=~D window=600000~%" stage headroom-p admitted-p measured raw reserve)
                          (test-assert (> (+ raw reserve) 600000) "Conservative input cannot fit this window.")
                          (test-assert (<= (+ measured reserve) 600000) "Only measured credit fits this window."))
                        (if admitted-p
                            (setf (slot-value credentials 'cl-rfc8628::access-token) "route-fixture-bearer-two")
                            (setf (slot-value credentials 'cl-rfc8628::account-id) "route-fixture-account-B"))))
              (let ((result (catch 'route-refusal
                              (let ((*context-measured-test-refusal-function*
                                      (lambda (condition) (throw 'route-refusal condition))))
                                (agent--run-provider-loop next-agent observer :tools-p nil)))))
                (test-assert mutated "The armed final-boundary mutation ran.")
                (format t "BOUNDARY outcome refusal=~S transports=~D changed-header=~S~%" (typep result 'provider-context-budget-error) (length routes) (and (second routes) (not (equal (first routes) (second routes)))))
                (test-assert (eq admitted-p (not (typep result 'provider-context-budget-error))) "Only a proven equivalent account route may consume the loaded receipt.")
                (test-assert (= (if admitted-p 2 1) (length routes)) "Changed or unknown routing refuses before any target transport.")
                (when admitted-p
                  (test-assert (equal (first routes) (second routes)) "Bearer refresh preserves the actual stable account-routing header."))
                (unless admitted-p
                  (test-assert (equal receipt (conversation-input-measurement loaded)) "Route refusal preserves the prior valid receipt.")))
              (provider-tests--assert-credential-free
               (list receipt (conversation-input-measurement loaded)
                     (conversation--header-record loaded)
                     (uiop:read-file-string (conversation-log-pathname loaded)))
               '("route-fixture-account-A" "route-fixture-account-B" "route-fixture-bearer-one" "route-fixture-bearer-two"
                 "route-fixture-refresh" "route-fixture-id" "ChatGPT-Account-ID" "Authorization")
               "Routing proofs and ordinary journals contain no raw routing headers or credentials."))))))))
  nil)

(-> test-context-boundary-prepare-change () null)
(defun test-context-boundary-prepare-change ()
  "Refuse changed accounts during request preparation."
  (context-boundary-tests--run :PREPARE NIL NIL))

(-> test-context-boundary-headroom-change () null)
(defun test-context-boundary-headroom-change ()
  "Recompute output headroom without a changed account's measured credit."
  (context-boundary-tests--run :PREPARE T NIL))

(-> test-context-boundary-observer-change () null)
(defun test-context-boundary-observer-change ()
  "Refuse changed accounts at the final observer boundary."
  (context-boundary-tests--run :OBSERVER NIL NIL))

(-> test-context-boundary-prepare-refresh () null)
(defun test-context-boundary-prepare-refresh ()
  "Admit same-account bearer refresh during preparation."
  (context-boundary-tests--run :PREPARE NIL T))

(-> test-context-boundary-headroom-refresh () null)
(defun test-context-boundary-headroom-refresh ()
  "Preserve measured output headroom after same-account refresh."
  (context-boundary-tests--run :PREPARE T T))

(-> test-context-boundary-observer-refresh () null)
(defun test-context-boundary-observer-refresh ()
  "Admit same-account bearer refresh at the observer boundary."
  (context-boundary-tests--run :OBSERVER NIL T))
