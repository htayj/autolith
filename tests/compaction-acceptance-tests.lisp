(in-package #:autolith)

;;;; -- Compaction Acceptance and Paid Attempts --

(defclass compaction-admission-test-provider (scripted-provider codex-subscription-provider)
  ((caps :initform nil :accessor compaction-admission-test-caps
         :documentation "Output caps observed by ordinary scripted requests.")
   (encoded-choices :initform nil :accessor compaction-admission-test-encoded-choices
                    :documentation "Tool choices in encoded candidate requests.")
   (ordinary-choices :initform nil :accessor compaction-admission-test-ordinary-choices
                     :documentation "Tool choices supplied to ordinary requests."))
  (:documentation "Combine deterministic responses with the real Codex request encoder."))

(defmethod provider-request-object
    ((provider compaction-admission-test-provider) (conversation conversation)
     (tool-namespaces vector) &key goal-context compaction-p)
  "Build admission requests with the production Codex encoder."
  (multiple-value-bind (request delivery)
      (provider-request-object
       (make-instance 'codex-subscription-provider :configuration (provider-configuration provider))
       conversation tool-namespaces :goal-context goal-context :compaction-p compaction-p)
    (push (json-get request "tool_choice") (compaction-admission-test-encoded-choices provider))
    (values request delivery)))

(defmethod provider-native-compact-conversation
    ((provider compaction-admission-test-provider) (conversation conversation)
     &key tool-namespaces event-callback)
  "Use the portable path without credentials or transport."
  (declare (ignore provider conversation tool-namespaces event-callback))
  (values nil nil))

(defmethod provider-stream-turn :before
    ((provider compaction-admission-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Record the cap actually supplied to each ordinary request."
  (declare (ignore event-callback))
  (unless compaction-p
    ;; Scripted transport must expose the same prepared-candidate decision boundary.
    ;; Gap providers already traverse the actual open-response-stream methods.
    (when (eq (class-name (class-of provider)) 'compaction-admission-test-provider)
      (provider-call-prepared-request-decision
       provider (provider-request-object provider conversation tool-namespaces :goal-context goal-context)))
    (push *provider-maximum-output-tokens* (compaction-admission-test-caps provider))
    (push *provider-tool-choice* (compaction-admission-test-ordinary-choices provider))))

(defclass compaction-admission-test-observer (agent-observer)
  ((cap :initarg :cap :reader compaction-admission-test-cap
        :documentation "Selected ordinary output allowance.")
   (starts :initform nil :accessor compaction-admission-test-starts
           :documentation "Details for each announced ordinary request."))
  (:documentation "Select an observable cap once at each ordinary request boundary."))

(defmethod agent-observer-status
    ((observer compaction-admission-test-observer) status details)
  "Record request starts and return the selected output cap."
  (when (eq status ':provider-request-started)
    (push details (compaction-admission-test-starts observer))
    (compaction-admission-test-cap observer)))

(-> test-compaction-summary-acceptance () null)
(defun test-compaction-summary-acceptance ()
  "Validate paid summaries and publish the last text answer even when misshapen.

Each case lists its output items, turn completion, whether the handoff passes
the strict shape, and whether the only allowed request publishes it."
  (let* ((valid (agent-test-compaction-message))
         (text  (provider-result-assistant-text
                 (agent-test-result "text" (list valid) :turn-completion ':end))))
    (dolist (case (list (list (list valid) ':end t t)
                       (list (list valid) ':continue nil nil)
                       (list (list valid) ':unspecified t t)
                       (list nil ':end nil nil)
                       (list (list (agent-test-message "unstructured text")) ':end nil t)
                       (list (list (agent-test-message
                                    (format nil "## [Progress]~%Work is recorded.~2%**[Decisions]** Continue.~2%## [Context]~%State is durable.~2%## [Constraints]~%Follow the user.~2%## [Next steps]~%Resume.~2%## [References]~%The archive."))) ':end t t)
                       (list (list (agent-test-message
                                    (format nil "Here is the handoff.~%~A" text))) ':end nil t)
                       (list (list (agent-test-message
                                    (concatenate 'string text " [Progress] Duplicate."))) ':end nil t)
                       (list (list (agent-test-message
                                    (concatenate 'string text (make-string 12000 :initial-element #\x)))) ':end nil t)
                       (list (list valid (json-object "type" "refusal" "refusal" "No.")) ':end nil nil)
                       (list (list valid (json-object "type" "message" "role" "assistant"
                                                     "refusal" "No." "content" #())) ':end nil nil)))
      (destructuring-bind (items completion usable-p published-p) case
        (with-test-configuration (configuration)
          (let* ((conversation (conversation-create configuration))
                 (result       (agent-test-result "paid-summary" items :turn-completion completion))
                 (provider     (make-instance 'scripted-provider :configuration configuration :results (list result)))
                 (agent        (agent-create :configuration configuration :conversation conversation
                                             :provider provider :tool-registry (agent-test-registry) :worker nil)))
            (conversation-append-user-message conversation "retain exact source evidence")
            (let ((before                                      (mapcar #'json-encode (conversation-input-items conversation)))
                  (*agent-compaction-maximum-summary-requests* 1))
              (test-assert (eq usable-p (agent--compaction-summary-usable-p result))
                           "only a finished structured refusal-free handoff is usable")
              (let ((published
                      (handler-case
                          (progn (agent-compact-conversation agent (make-instance 'agent-observer)) t)
                        (provider-protocol-error () nil))))
                (test-assert (eq published-p published)
                             "the last request publishes any text answer and nothing else"))
              (test-assert (equal '((1 :summary)) (agent-tests--request-numbers conversation))
                           "every completed summary attempt is charged once")
              (unless published-p
                (test-assert (equal before (mapcar #'json-encode (conversation-input-items conversation)))
                             "rejection preserves the source projection")
                (test-assert (equal before
                                    (mapcar #'json-encode
                                            (conversation-input-items
                                             (conversation-load (conversation-pathname conversation)))))
                             "rejection preserves the replayed projection"))))))))
  nil)

(-> test-compaction-native-paid-rejection () null)
(defun test-compaction-native-paid-rejection ()
  "Meter paid native completion even with no opaque output or rejected content."
  (dolist (invalid-p '(nil t))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (provider     (make-instance 'scripted-provider :configuration configuration
                                          :results                          (list (agent-test-result "summary"
                                                                                                    (list (agent-test-compaction-message))
                                                                                                    :turn-completion ':end))))
             (agent        (agent-create :configuration configuration :conversation conversation
                                         :provider provider :tool-registry (agent-test-registry) :worker nil)))
        (conversation-append-user-message conversation "native source")
        (test-call-with-function-replacements
         (list (list 'provider-native-compact-conversation
                     (lambda (provider view &key tool-namespaces event-callback)
                       (declare (ignore provider view tool-namespaces))
                       (funcall event-callback
                                (make-instance 'native-compaction-completed-event
                                               :usage nil :response-id "native-paid"))
                       (when invalid-p
                         (error 'provider-protocol-error :message "Rejected paid native output."
                                                        :status nil :code nil :request-id nil
                                                        :response-id "native-paid" :response nil))
                       (values nil nil))))
         (lambda ()
           (handler-case (agent-compact-conversation agent (make-instance 'agent-observer))
             (provider-protocol-error () nil))))
        (test-assert (equal (if invalid-p '((1 :native)) '((1 :native) (2 :summary)))
                            (agent-tests--request-numbers conversation))
                     "paid native completion is recorded once, before interpretation"))))
  nil)

(-> test-compaction-next-request-admission () null)
(defun test-compaction-next-request-admission ()
  "Validate pending caps and MCP contributions before publishing a checkpoint."
  (dolist (large-mcp-p '(nil t))
    (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "50000"))
      (with-test-configuration (configuration)
        (let* ((conversation (conversation-create configuration))
               (provider     (make-instance 'compaction-admission-test-provider
                                            :configuration configuration
                                            :results       (list (agent-test-result "summary"
                                                                                   (list (agent-test-compaction-message))
                                                                                   :turn-completion ':end))))
               (observer     (make-instance 'compaction-admission-test-observer
                                            :cap (if large-mcp-p 1000 50000)))
               (agent        (agent-create :configuration configuration :conversation conversation
                                           :provider provider :tool-registry (agent-test-registry) :worker nil)))
          (conversation-append-user-message conversation "original source")
          (let ((before (mapcar #'json-encode (conversation-input-items conversation))))
            (test-call-with-function-replacements
             (list (list 'mcp-tool-registry-context-contributions
                         (lambda (registry)
                           (declare (ignore registry))
                           (when large-mcp-p
                            (loop for index below 2
                                  collect (make-context-contribution
                                           :identifier  (format nil "large-mcp-~D" index)
                                           :priority 1000 :class ':mandatory
                                           :instruction (make-string 120000 :initial-element #\x)))))))
             (lambda ()
               (test-assert
                (handler-case
                    (progn (agent-compact-conversation agent observer
                                                      :next-request-status-details t) nil)
                  (provider-context-budget-error () t))
                "candidate admission includes the actual cap and MCP context")))
            (test-assert (equal before (mapcar #'json-encode (conversation-input-items conversation)))
                         "oversized next-request context cannot replace the source")
            (test-assert (equal '((1 :summary)) (agent-tests--request-numbers conversation))
                         "candidate rejection records the paid summary, not a phantom ordinary request")
            (test-assert (= 1 (length (compaction-admission-test-starts observer)))
                         "the next ordinary cap is selected once"))))))
  nil)

(-> test-compaction-repeated-continuation-accounting () null)
(defun test-compaction-repeated-continuation-accounting ()
  "Continue through repeated checkpoints and reloads with exact request accounting."
  (with-test-configuration (configuration)
    (let* ((conversation                           (conversation-create configuration))
           (provider                               (make-instance 'compaction-admission-test-provider
                                                                  :configuration configuration
                                                                  :results       (loop repeat 3 append
                                                                                   (list (agent-test-result "summary"
                                                                                                            (list (agent-test-compaction-message))
                                                                                                            :turn-completion ':end)
                                                                                         (agent-test-result "answer" (list (agent-test-message "verified"))
                                                                                                            :turn-completion ':end)))))
           (observer                               (make-instance 'compaction-admission-test-observer :cap 1000))
           (pending                                nil)
           (*agent-restricted-maximum-tool-rounds* 0))
      (test-call-with-function-replacements
       (list (list 'agent-should-compact-p
                   (lambda (agent) (declare (ignore agent))
                     (prog1 pending (setf pending nil)))))
       (lambda ()
         (loop for index from 1 to 3
               do (setf pending t)
                  (let ((agent (agent-create :configuration configuration :conversation conversation
                                             :provider provider :tool-registry (agent-test-registry) :worker nil)))
                    (agent-run-user-turn agent (format nil "literal continuation ~D" index)
                                         :observer observer :tool-allowlist '("test.echo")
                                         :tool-restriction-p (oddp index)))
                  (setf conversation (conversation-load (conversation-pathname conversation))))))
      ;; The unissued soft candidate consumes safety allowance, not a paid number.
      (test-assert (equal '((1 :summary) (2 nil) (1 :summary) (2 nil) (1 :summary) (2 nil))
                          (agent-tests--request-numbers conversation))
                   "every checkpoint and continuation contributes one paid record")
      (test-assert (equal '(1000 1000 1000) (compaction-admission-test-caps provider))
                   "ordinary requests reuse the cap checked before publication")
      (test-assert (= 6 (length (compaction-admission-test-starts observer)))
                   "each refused candidate and resumed ordinary request is announced once")
      (test-assert (equal '("none" "none" "none" "auto" "auto" "auto" "none" "none" "none")
                         (compaction-admission-test-encoded-choices provider))
                   "initial, validation and resumed candidates retain the resolved tool choice")
      (test-assert (equal '(:none :auto :none)
                          (compaction-admission-test-ordinary-choices provider))
                   "ordinary requests use the same tool choice checked before publication")
      (test-assert (test-object-contains-string-p (conversation-input-items conversation)
                                                "literal continuation 3")
                   "the latest exact user continuation survives publication and replay")))
  nil)

(defvar *compaction-gap-test-contributions* nil
  "Dynamically bound contributor invocation counter for recovery fixtures.")

(-> compaction-gap-test--contribute (request-context) context-contribution)
(defun compaction-gap-test--contribute (context)
  "Record one invocation and deliver a real request-local instruction."
  (declare (ignore context))
  (incf (first *compaction-gap-test-contributions*))
  (make-context-contribution :identifier "recovery" :instruction "Keep the continuation concise."))

(defclass compaction-gap-test-provider
    (compaction-admission-test-provider context-admission-test-provider)
  ((mode :initarg :mode :initform nil :reader compaction-gap-test-mode
         :documentation "Optional failure control at actual request admission.")
   (attempts :initform 0 :accessor compaction-gap-test-attempts
             :documentation "Ordinary attempts, including locally refused ones.")
   (reservations :initform nil :accessor compaction-gap-test-reservations
                 :documentation "Independent mission reservations seen by each attempt."))
  (:documentation "Use real encoded admission before deterministic scripted responses."))

(defmethod provider-stream-turn
    ((provider compaction-gap-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Exercise final admission without credentials or network access."
  (declare (ignore event-callback))
  (unless compaction-p
    (incf (compaction-gap-test-attempts provider)))
  (let ((context (mission-context-find conversation)))
    (when context
      (let ((ledger (mission-context-goal context)))
        (push (list (getf ledger :requests-outstanding) (getf ledger :tokens-used)
                    (getf ledger :tokens-reserved) *provider-mission-output-allowance*)
              (compaction-gap-test-reservations provider)))))
  (when (and (not compaction-p) (eq (compaction-gap-test-mode provider) :generic))
    (error 'provider-protocol-error :message "Scripted transport failure."
           :status nil :code nil :request-id nil :response-id nil :response nil))
  (let ((before (and *compaction-gap-test-contributions* (first *compaction-gap-test-contributions*))))
    (multiple-value-bind (request delivery)
        (provider-request-object provider conversation tool-namespaces
                                 :goal-context goal-context :compaction-p compaction-p)
      (declare (ignore delivery))
      (when (and before (not compaction-p))
        (push (- (first *compaction-gap-test-contributions*) before)
              (rest *compaction-gap-test-contributions*)))
      (when (and (not compaction-p)
                 (or (eq (compaction-gap-test-mode provider) :refuse)
                     (and (eq (compaction-gap-test-mode provider) :second-refusal)
                          (> (compaction-gap-test-attempts provider) 1))))
        (setf (gethash "max_output_tokens" request) 6000))
      (provider-open-response-stream provider request :conversation conversation)))
  (call-next-method))

(defclass compaction-gap-test-observer (agent-observer)
  ((compactions :initform 0 :accessor compaction-gap-test-compactions
                :documentation "Automatic compaction announcements."))
  (:documentation "Observe automatic compaction without changing its decision."))

(defmethod agent-observer-status ((observer compaction-gap-test-observer) status details)
  "Count announced automatic compactions."
  (declare (ignore details))
  (when (eq status :compaction-started)
    (incf (compaction-gap-test-compactions observer)))
  nil)

(-> compaction-gap-test--recover
    (boolean &key (:mode (option keyword)) (:proactive-p boolean)
                  (:request-limit (integer 1)) (:failure-type symbol)
                  (:compactions (integer 0)) (:network-calls (integer 0)))
    null)
(defun compaction-gap-test--recover
    (mission-p &key mode proactive-p (request-limit 512) (failure-type 'null)
                   (compactions 1) (network-calls 2))
  "Exercise local refusal and recovery, optionally under mission accounting."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((provider
               (make-instance 'compaction-gap-test-provider
                              :configuration configuration
                              :mode          mode
                              :results       (list (agent-test-result "summary" (list (agent-test-compaction-message)))
                                                   (agent-test-result "answer" (list (agent-test-message "OK"))))))
             (application
               (when mission-p (mission-inference-test--application configuration provider)))
             (conversation
               (if application (application-conversation application) (conversation-create configuration)))
             (observer
               (make-instance 'compaction-gap-test-observer))
             (agent
               (agent-create :configuration configuration :conversation conversation
                             :provider provider :tool-registry (make-instance 'tool-registry)))
             (goal
               (make-string 10000 :initial-element #\g))
             (*conversation-compaction-tail-characters*
               1024)
             (*agent-maximum-provider-requests-per-turn*
               request-limit)
             (*context-contributors*
               nil)
             (*context-resolver*
               (cl-llm-provider-api:make-context-resolver))
             (*compaction-gap-test-contributions*
               (list 0)))
        (register-context-contributor "recovery" 'compaction-gap-test--contribute)
        (when application
          (application-mission-start application (mission-test--specification :turns 8 :tokens 100000)))
        (unwind-protect
             (progn
               (conversation-append-user-message conversation (make-string 12000 :initial-element #\h))
               (conversation-append-user-message conversation "continue")
               (test-call-with-function-replacements
                (list (list 'system-prompt (lambda (c) (declare (ignore c)) "small"))
                      (list 'request-context-session-state
                            (lambda (c &key hurry-up-p) (declare (ignore c hurry-up-p)) ""))
                      (list 'agent-should-compact-p
                            (let ((original (symbol-function 'agent-should-compact-p)))
                              (lambda (candidate)
                                (if proactive-p t (funcall original candidate))))))
                (lambda ()
                  (test-assert (eq (not (null proactive-p)) (agent-should-compact-p agent))
                               "the controlled soft trigger matches this scenario")
                  (let ((failure nil))
                    (handler-case (agent--run-provider-loop agent observer :goal-context goal :tools-p nil)
                      (error (condition) (setf failure condition)))
                    (format t "~&FINAL-ADMISSION-WITNESS mission=~S compactions=~D transport=~D error=~S~%"
                            mission-p (compaction-gap-test-compactions observer)
                            (context-admission-test-provider-network-calls provider)
                            (and failure (type-of failure)))
                    (test-assert (typep failure failure-type) "the ordinary attempt has the expected outcome")
                    (test-assert (= compactions (compaction-gap-test-compactions observer))
                                 "the step uses at most one compaction opportunity")
                    (test-assert (= network-calls (context-admission-test-provider-network-calls provider))
                                 "only admitted summary and ordinary requests reach inert transport"))))
               (test-assert (every (lambda (count) (= 1 count))
                                   (rest *compaction-gap-test-contributions*))
                            "each ordinary preparation invokes its contributor exactly once")
               (when application
                 (let ((ledger (mission-context-goal (mission-context-find conversation))))
                   (format t "~&MISSION-LEDGER-WITNESS status=~S used=~D turns=~D unknown=~D reserved=~D outstanding=~D attempts=~S~%"
                           (getf ledger :status) (getf ledger :tokens-used) (getf ledger :turns-used)
                           (getf ledger :unknown-usage 0) (getf ledger :tokens-reserved)
                           (getf ledger :requests-outstanding) (compaction-gap-test-reservations provider))
                   (test-assert
                    (every (lambda (snapshot)
                             (destructuring-bind (outstanding used reserved allowance) snapshot
                               (declare (ignore used))
                               (and (= 1 outstanding) (= reserved allowance))))
                           (compaction-gap-test-reservations provider))
                    "each attempt owns a single independent reservation")
                   (when (eq mode :second-refusal)
                     (test-assert (and (eq ':active (getf ledger :status))
                                       (zerop (getf ledger :unknown-usage 0))
                                       (zerop (getf ledger :tokens-reserved))
                                       (zerop (getf ledger :requests-outstanding))
                                       (= (* 2 network-calls) (getf ledger :tokens-used))
                                       (= (+ 2 network-calls) (getf ledger :turns-used)))
                                  "both refused attempts and the paid summary reserve turns"))
                   (unless (eq mode :second-refusal)
                     (test-assert (and (eq ':active (getf ledger :status))
                                       (zerop (getf ledger :unknown-usage 0))
                                       (zerop (getf ledger :tokens-reserved))
                                       (zerop (getf ledger :requests-outstanding))
                                       (= (* 2 network-calls) (getf ledger :tokens-used))
                                      (= (+ (if proactive-p 2 1) network-calls) (getf ledger :turns-used)))
                                  "recovery settles all reservations without unknown usage")))))
          (when application (mission-detach application))))))
  nil)

(-> test-compaction-final-admission-trigger () null)
(defun test-compaction-final-admission-trigger ()
  "Recover a final request larger than the known-history projection."
  (compaction-gap-test--recover nil))

(-> test-compaction-mission-admission-trigger () null)
(defun test-compaction-mission-admission-trigger ()
  "Recover outside mission ownership after a proven zero-usage refusal."
  (compaction-gap-test--recover t))

(-> test-compaction-admission-recovery-bounds () null)
(defun test-compaction-admission-recovery-bounds ()
  "Bound recovery, share the proactive opportunity and preserve unrelated errors."
  (dolist (mission-p '(nil t))
    (compaction-gap-test--recover mission-p :mode ':second-refusal
                                 :failure-type 'provider-context-budget-error :network-calls 1)
    (compaction-gap-test--recover mission-p :mode ':refuse :proactive-p t
                                 :failure-type 'provider-context-budget-error :network-calls 1)
    (compaction-gap-test--recover mission-p :request-limit 1
                                 :failure-type 'agent-loop-error :compactions 0 :network-calls 0)
    (compaction-gap-test--recover mission-p :request-limit 2
                                 :failure-type 'agent-loop-error :network-calls 1))
  (compaction-gap-test--recover nil :mode ':generic :failure-type 'provider-protocol-error
                               :compactions 0 :network-calls 0)
  nil)

(-> test-compaction-final-projection-gap () null)
(defun test-compaction-final-projection-gap ()
  "Characterize the difference between known history and final encoded admission."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (provider     (make-instance 'context-admission-test-provider :configuration configuration))
             (agent        (agent-create :configuration configuration :conversation conversation
                                         :provider provider :tool-registry (make-instance 'tool-registry))))
        (conversation-append-user-message conversation (make-string 12000 :initial-element #\h))
        (test-call-with-function-replacements
         (list (list 'system-prompt (lambda (c) (declare (ignore c)) "small"))
               (list 'request-context-session-state
                     (lambda (c &key hurry-up-p) (declare (ignore c hurry-up-p)) "")))
         (lambda ()
           (test-assert (not (agent-should-compact-p agent))
                        "the fresh known estimate is below the soft threshold")
           (let ((request (provider-request-object provider conversation #()
                                                   :goal-context (make-string 10000 :initial-element #\g))))
             (test-assert (handler-case (progn (provider-check-request-context provider request) nil)
                            (provider-context-budget-error () t))
                          "the final encoded request nevertheless exceeds the hard window")))))))
  nil)


;;;; -- Mission Ownership of Detached Compaction Requests --

(defclass compaction-ownership-test-provider (compaction-gap-test-provider)
  ((summary-mode :initarg       :summary-mode
                 :initform      ':known
                 :reader        compaction-ownership-test-summary-mode
                 :documentation "Known, unknown, failed or locally refused summary requests.")
   (views :initform nil :accessor compaction-ownership-test-views
          :documentation "Request views and exact mission identity during dispatch.")
   (native-p :initarg :native-p :initform nil :reader compaction-ownership-test-native-p
             :documentation "Whether to enter an actual native request attempt."))
  (:documentation "Observe detached request ownership without credentials or network access."))

(defmethod provider-stream-turn
    ((provider compaction-ownership-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Record summary identity and exercise ordinary failure accounting."
  (declare (ignore tool-namespaces event-callback goal-context))
  (when compaction-p
    (push (list conversation (mission-context-find conversation))
          (compaction-ownership-test-views provider))
    (case (compaction-ownership-test-summary-mode provider)
      (:error
       (error 'provider-protocol-error :message "Scripted summary failure."
              :status nil :code nil :request-id nil :response-id nil :response nil))
      (:refuse
       (provider-open-response-stream
        provider (json-object "input" "short" "max_output_tokens" 6000)
        :conversation conversation))))
  (call-next-method))

(defmethod provider-native-compact-conversation
    ((provider compaction-ownership-test-provider) (conversation conversation)
     &key tool-namespaces event-callback)
  "Enter the native-attempt boundary only when this fixture supports native requests."
  (if (compaction-ownership-test-native-p provider)
      (provider-attempt-native-compaction provider conversation
                                          :tool-namespaces tool-namespaces
                                          :force-refresh nil :event-callback event-callback)
      (values nil nil)))

(defmethod provider-attempt-native-compaction
    ((provider compaction-ownership-test-provider) (conversation conversation)
     &key tool-namespaces force-refresh event-callback)
  "Report separately paid native usage without an opaque checkpoint."
  (declare (ignore tool-namespaces force-refresh event-callback))
  (push (list conversation (mission-context-find conversation))
        (compaction-ownership-test-views provider))
  (case (compaction-ownership-test-summary-mode provider)
    (:native-error
     (error 'provider-protocol-error :message "Scripted native failure."
            :status nil :code nil :request-id nil :response-id nil :response nil))
    (:native-refuse
     (provider-open-response-stream
      provider (json-object "input" "short" "max_output_tokens" 6000)
      :conversation conversation)))
  (values nil (if (eq (compaction-ownership-test-summary-mode provider) :native-unknown)
                  (json-object)
                  (json-object "input_tokens" 2 "output_tokens" 3))))

(-> compaction-ownership-test--run
    (&key (:mission-p boolean) (:mode keyword) (:native-p boolean)
          (:turns (integer 1)) (:continued-p boolean)) null)
(defun compaction-ownership-test--run
    (&key (mission-p t) (mode ':known) native-p (turns 8) continued-p)
  "Verify actual usage, refusal, budget and cleanup on a private mission."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "5000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((usage        (if (eq mode :unknown) (json-object)
                              (json-object "input_tokens" 1 "output_tokens" 1)))
             (provider     (make-instance
                            'compaction-ownership-test-provider
                            :configuration configuration :summary-mode mode :native-p native-p
                            :results (append
                                      (when continued-p
                                        (list (agent-test-result "unfinished" nil :turn-completion ':continue)))
                                      (list (agent-test-result
                                             "summary" (list (agent-test-compaction-message))
                                             :usage usage)))))
             (application  (when mission-p (mission-inference-test--application configuration provider)))
             (conversation (if application (application-conversation application)
                               (conversation-create configuration)))
             (agent        (agent-create :configuration configuration :conversation conversation
                                         :provider      provider :tool-registry (make-instance 'tool-registry)))
             (failure      nil)
             (context      nil))
        (when application
          (application-mission-start application (mission-test--specification :turns turns :tokens 100000))
          (setf context (mission-context-find conversation)))
        (unwind-protect
             (progn
               (conversation-append-user-message conversation "Preserve this exact instruction.")
               (when (and application (= turns 1))
                 (mission-inference-test--call
                  (make-instance 'scripted-provider
                                 :configuration configuration
                                 :results       (list (agent-test-result "prior" (list (agent-test-message "prior")))))
                  conversation))
               (test-call-with-function-replacements
                (list (list 'system-prompt (lambda (c) (declare (ignore c)) "small"))
                      (list 'request-context-session-state
                            (lambda (c &key hurry-up-p) (declare (ignore c hurry-up-p)) "")))
                (lambda ()
                  (handler-case (agent-compact-conversation agent (make-instance 'agent-observer))
                    (error (condition) (setf failure condition)))))
               (dolist (entry (compaction-ownership-test-views provider))
                 (test-assert (eq context (second entry)) "the request uses its source's exact mission identity")
                 (test-assert (null (mission-context-find (first entry))) "fresh ownership ends on every unwind"))
               (when application
                 (let* ((ledger      (mission-context-goal context))
                        (blocked-p   (member mode '(:unknown :error :native-unknown :native-error)))
                        (exhausted-p (= turns 1))
                        (expected-tokens
                          (cond
                            (exhausted-p
                             2)
                            (blocked-p
                             0)
                            ((eq mode :refuse)
                             0)
                            ((eq mode :native-refuse)
                             2)
                            (t
                             (+ (if native-p 5 0) (if continued-p 4 2))))))
                   (format t "~&OWNERSHIP-WITNESS mode=~S native=~S turns=~D error=~S ledger=~S~%"
                           mode native-p turns (and failure (type-of failure)) ledger)
                   (test-assert (= expected-tokens (getf ledger :tokens-used)) "only actual reported usage is charged")
                   (test-assert (= (if blocked-p 1 0) (getf ledger :unknown-usage 0)) "unknown follows ordinary mission policy")
                   (test-assert (zerop (getf ledger :requests-outstanding)) "all request reservations settle")
                   (test-assert (zerop (getf ledger :tokens-reserved)) "no output reservation leaks")
                   (cond
                     (exhausted-p
                      (test-assert failure "the exhausted mission refuses summary dispatch")
                      (test-assert (null (compaction-ownership-test-views provider)) "no summary enters after turn exhaustion"))
                     (blocked-p
                      (test-assert (eq ':blocked (getf ledger :status)) "unknown summary usage blocks new mission requests"))
                     ((eq mode :refuse)
                      (test-assert (typep failure 'provider-context-budget-error) "a local refusal retains its type")
                      (test-assert (eq ':active (getf ledger :status)) "a local compaction refusal has known zero usage"))
                     ((eq mode :native-refuse)
                      (test-assert (null failure) "exhausted native admission falls back to the admitted portable summary")
                      (test-assert (eq ':active (getf ledger :status)) "known-zero native refusals do not invent token usage"))
                     (t
                      (test-assert (null failure) "owned compaction succeeds")
                      (test-assert (= (+ (if native-p 1 0) (if continued-p 2 1)) (getf ledger :turns-used))
                                   "each actual native or continued summary request reserves one turn"))))))
          (when application (mission-detach application))))))
  nil)

(-> test-compaction-summary-mission-ownership () null)
(defun test-compaction-summary-mission-ownership ()
  "Charge known summaries and bound view ownership, including unsupported native probes."
  (compaction-ownership-test--run)
  (compaction-ownership-test--run :continued-p t)
  (compaction-ownership-test--run :mission-p nil))

(-> test-compaction-summary-mission-failures () null)
(defun test-compaction-summary-mission-failures ()
  "Retain ordinary UNKNOWN and budget policies without invented usage."
  (dolist (mode '(:unknown :error :refuse))
    (compaction-ownership-test--run :mode mode))
  (compaction-ownership-test--run :turns 1))

(-> test-compaction-native-view-mission-ownership () null)
(defun test-compaction-native-view-mission-ownership ()
  "Charge the detached native request and summary exactly once each."
  (compaction-ownership-test--run :native-p t)
  (dolist (mode '(:native-unknown :native-error :native-refuse))
    (compaction-ownership-test--run :native-p t :mode mode))
  (compaction-ownership-test--run :native-p t :turns 1))

(-> test-compaction-request-view-isolation () null)
(defun test-compaction-request-view-isolation ()
  "Bound exact mission aliases without clobbering nested or unrelated request owners."
  (with-test-configuration (configuration)
    (let* ((first-application  (mission-test--application configuration))
           (second-application (mission-test--application configuration))
           (first-source       (application-conversation first-application))
           (second-source      (application-conversation second-application))
           (view               (conversation-create configuration)))
      (application-mission-start first-application (mission-test--specification))
      (application-mission-start second-application (mission-test--specification))
      (unwind-protect
           (let ((first-context  (mission-context-find first-source))
                 (second-context (mission-context-find second-source)))
             (mission-call-with-request-view
              first-context view
              (lambda ()
                (test-assert (eq first-context (mission-context-find view)) "the outer request owns its fresh view")
                (mission-call-with-request-view
                 second-context view
                 (lambda ()
                   (test-assert (eq first-context (mission-context-find view)) "nested work cannot replace an existing owner")))
                (test-assert (eq first-context (mission-context-find view)) "nested cleanup preserves the outer association")))
             (test-assert (null (mission-context-find view)) "outer cleanup removes only its introduced association")
             (mission-context-bind second-context view)
             (mission-call-with-request-view
              first-context view
              (lambda ()
                (test-assert (eq second-context (mission-context-find view)) "unrelated existing ownership is preserved")))
             (test-assert (eq second-context (mission-context-find view)) "cleanup never removes an unrelated association")
             (with-lock-held (*mission-contexts-lock*)
               (remhash view *mission-contexts*)))
        (mission-detach first-application)
        (mission-detach second-application))))
  nil)


;;;; -- Participating Agent Integration Boundary --

(defclass prepared-scripted-credential-manager (credential-manager) ()
  (:documentation "A fixture-local manager with synthetic credentials and no credential sources."))

(defmethod credential-manager-load ((manager prepared-scripted-credential-manager))
  "Return synthetic, nonexpiring authentication without reading credential storage."
  (declare (ignore manager))
  (make-instance 'oauth-credentials :access-token "inert-prepared-test"
                 :account-id "prepared-test" :refresh-token nil :id-token nil
                 :expires-at nil :source-path nil))

(defclass prepared-scripted-provider (context-measured-test-provider)
  ((results :initarg :results :accessor scripted-provider-results
            :documentation "Responses consumed only by admitted transport.")
   (input-counts :initform nil :accessor scripted-provider-input-counts
                 :documentation "Durable input counts at admitted transport.")
   (input-snapshots :initform nil :accessor scripted-provider-input-snapshots
                    :documentation "Durable inputs at admitted transport.")
   (compaction-flags :initform nil :accessor scripted-provider-compaction-flags
                     :documentation "Compaction roles of admitted requests.")
   (native-items :initarg :native-items :initform nil :accessor native-scripted-provider-native-items
                 :documentation "Synthetic native checkpoints.")
   (native-input-snapshots :initform nil :accessor native-scripted-provider-native-input-snapshots
                           :documentation "Durable inputs at native transport."))
  (:documentation "Production preparation and admission with inert scripted Responses I/O."))

(defclass prepared-native-scripted-provider (prepared-scripted-provider) ()
  (:documentation "A participating fixture exercising real native request preparation."))

(defmethod initialize-instance :around
    ((provider prepared-scripted-provider) &rest initargs &key configuration &allow-other-keys)
  "Use a private synthetic manager, never the application's credential sources."
  (declare (ignore configuration))
  (apply #'call-next-method provider
         :credential-manager (make-instance 'prepared-scripted-credential-manager) initargs))

(defparameter *prepared-scripted-compaction-p* nil
  "Role of the candidate currently passing through real preparation.")

(defmethod provider-stream-turn :around
    ((provider prepared-scripted-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Record the candidate role without replacing its production preparation."
  (declare (ignore tool-namespaces event-callback goal-context))
  (let ((*prepared-scripted-compaction-p* compaction-p))
    (prepared-scripted--with-credentials (lambda () (call-next-method)))))

(defmethod provider-open-response-stream
    ((provider prepared-scripted-provider) (request hash-table)
     &key credentials conversation)
  "Consume a scripted response only after final preparation and hard admission."
  (declare (ignore credentials))
  (let* ((items  (conversation-input-items-for-family
                  conversation (provider-family provider)
                  :include-ephemeral-p (not *prepared-scripted-compaction-p*)))
         (result (pop (scripted-provider-results provider)))
         (output (coerce (provider-result-output-items result) 'vector))
         (id     (provider-result-response-id result)))
    (push request (context-measured-test-requests provider))
    (push (copy-list items) (scripted-provider-input-snapshots provider))
    (push (length items) (scripted-provider-input-counts provider))
    (push *prepared-scripted-compaction-p* (scripted-provider-compaction-flags provider))
    (values
     (make-instance
      'test-character-input-stream
      :source
      (concatenate
       'string
       (test-sse-event-string (json-object "type" "response.created" "response" (json-object "id" id)))
       (with-output-to-string (stream)
         (loop for item across output
               do (write-string (test-sse-event-string
                                 (json-object "type" "response.output_item.done" "item" item)) stream)))
       (test-sse-event-string
        (json-object "type" "response.completed"
                     "response" (json-object "id" id "status" "completed" "output" output
                                             "usage" (provider-result-usage result))))))
     200 nil)))

(defmethod provider-native-compact-conversation
    ((provider prepared-scripted-provider) (conversation conversation)
     &key tool-namespaces event-callback)
  "Use production native preparation only for the native fixture."
  (declare (ignore tool-namespaces event-callback))
  (when (typep provider 'prepared-native-scripted-provider)
    (prepared-scripted--with-credentials (lambda () (call-next-method)))))

(defmethod provider-open-native-compaction
    ((provider prepared-scripted-provider) (request hash-table)
     &key credentials conversation)
  "Return an inert checkpoint after genuine native hard admission."
  (declare (ignore credentials conversation))
  (test-assert (null *provider-input-measurement-context*) "native I/O cannot publish ordinary measurements")
  (push (coerce (json-get request "input") 'list)
        (native-scripted-provider-native-input-snapshots provider))
  (values (json-encode (json-object "id" "prepared-native"
                                   "output" (vector (pop (native-scripted-provider-native-items provider)))
                                   "usage" (json-object "input_tokens" 1 "output_tokens" 1)))
          200 nil))

(-> prepared-scripted--with-credentials (function) t)
(defun prepared-scripted--with-credentials (function)
  "Forbid unhandled network I/O while the private manager supplies synthetic credentials."
  (test-call-with-function-replacements
   (list (list 'dexador:request
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (error "Prepared fixture forbids unhandled HTTP I/O."))))
   function))

(defmethod provider-final-request-participation-p ((provider prepared-scripted-provider))
  "Opt the real-boundary fixture into final-candidate soft decisions."
  (declare (ignore provider))
  t)

(-> test-compaction-provider-participation () null)
(defun test-compaction-provider-participation ()
  "Keep legacy proactive preflight while participants decide only on final prepared input."
  (dolist (participating-p '(nil t))
    (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "50000")
                            ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
      (with-test-configuration (configuration)
        (let* ((conversation (conversation-create configuration))
               (provider     (make-instance
                              (if participating-p 'prepared-scripted-provider 'scripted-provider)
                              :configuration configuration
                              :results       (list (agent-test-result "compat-summary"
                                                                      (list (agent-test-compaction-message))
                                                                      :turn-completion ':end)
                                                   (mission-test--result "Done."))))
               (agent        (agent-create :configuration configuration :provider provider
                                           :conversation conversation :tool-registry (agent-test-registry) :worker nil))
               (raw-calls    0))
          (conversation-append-user-message conversation "earlier context")
          (test-call-with-function-replacements
           (list (list 'context-estimate-known-tokens
                       (lambda (&rest arguments)
                         (declare (ignore arguments))
                         (incf raw-calls)
                         999999)))
           (lambda () (agent-run-user-turn agent "hello")))
          (test-assert (equal (reverse (scripted-provider-compaction-flags provider))
                              (if participating-p '(nil) '(t nil)))
                       "only the legacy path schedules proactive compaction from the raw estimate")
          (test-assert (if participating-p (zerop raw-calls) (plusp raw-calls))
                       "participation excludes the raw soft fallback, without removing legacy preflight")
          (test-assert (= (length (scripted-provider-results provider)) (if participating-p 1 0))
                       "each admitted request consumes exactly one response")))))
  nil)

(-> prepared-scripted--anchored-context (request-context) context-contribution)
(defun prepared-scripted--anchored-context (context)
  "Provide one stable block whose boundary must follow the compacted history."
  (declare (ignore context))
  (change-class
   (make-context-contribution :identifier  "prepared-observer"
                              :instruction "Retain the current continuation.")
   'anchored-context-contribution))

(-> prepared-scripted--run-observed (agent string) provider-result)
(defun prepared-scripted--run-observed (agent message)
  "Check each ordinary candidate before refusal unwinds and after native recovery."
  (let ((counts                 (make-hash-table))
        (decisions              nil)
        (history                nil)
        (anchor-items           nil)
        (anchors-valid-p        nil)
        (*context-contributors* nil)
        (*context-resolver*     (cl-llm-provider-api:make-context-resolver))
        (names                  '(provider-request-tool-namespaces context-resolve-request
                                  context-delivery-project-items context-anchors-update)))
    (register-context-contributor "prepared-observer" 'prepared-scripted--anchored-context)
    (flet ((ordinary-p ()
             (and *provider-input-measurement-context*
                  (not *prepared-scripted-compaction-p*))))
      (let ((result
              (test-call-with-function-replacements
               (append
                (loop for name in names
                      for original = (symbol-function name)
                      collect
                      (let ((name name) (original original))
                        (list name
                              (lambda (&rest arguments)
                                (when (ordinary-p)
                                  (incf (gethash name counts 0))
                                  (when (eq name 'context-delivery-project-items)
                                    (setf history (copy-list (third arguments)))))
                                (multiple-value-prog1 (apply original arguments)
                                  (when (and (ordinary-p) (eq name 'context-anchors-update))
                                    (let ((anchors (second arguments)))
                                      (setf anchor-items
                                            (mapcar (lambda (anchor) (getf anchor :item)) anchors)
                                            anchors-valid-p
                                            (and anchors
                                                 (equal anchors
                                                        (conversation-context-anchors (first arguments)))
                                                 (every
                                                  (lambda (anchor)
                                                    (let ((index (getf anchor :index)))
                                                      (and (typep index '(integer 0))
                                                           (< index (length history))
                                                           (equal (getf anchor :item)
                                                                  (context-anchors-item-digest
                                                                   (nth index history))))))
                                                  anchors))))))))))
                (let ((original (symbol-function 'provider-call-prepared-request-decision)))
                  (list
                   (list 'provider-call-prepared-request-decision
                         (lambda (provider request)
                           (if (and (ordinary-p) *provider-prepared-request-decision*)
                               (let ((decision
                                       (list :request         request
                                             :counts          (loop for name in names collect (gethash name counts 0))
                                             :anchors-valid-p anchors-valid-p
                                             :anchor-items    anchor-items
                                             :refused-p       nil)))
                                 ;; Snapshot before a soft refusal unwinds the candidate.
                                 (push decision decisions)
                                 (clrhash counts)
                                 (setf history nil anchor-items nil anchors-valid-p nil)
                                 (handler-bind
                                     ((provider-proactive-compaction-required
                                        (lambda (condition)
                                          (declare (ignore condition))
                                          (setf (getf decision :refused-p) t))))
                                   (funcall original provider request)))
                               (funcall original provider request)))))))
               (lambda () (agent-run-user-turn agent message)))))
        (test-assert (= 2 (length decisions))
                     "the refused and resumed ordinary candidates each decide exactly once")
        (dolist (decision decisions)
          (format t "~&PREPARED-CANDIDATE-WITNESS refused=~S counts=~S anchors-valid=~S~%"
                  (getf decision :refused-p) (getf decision :counts)
                  (not (null (getf decision :anchors-valid-p))))
          (test-assert (equal '(1 1 1 1) (getf decision :counts))
                       "each ordinary candidate selects, resolves, projects and updates anchors once")
          (test-assert (getf decision :anchors-valid-p)
                       "each candidate's installed anchor names its own projected history"))
        (when (= 2 (length decisions))
          (let* ((admitted (first decisions))
                 (refused  (second decisions))
                 (requests (context-measured-test-requests (agent-provider agent))))
            (format t "~&PREPARED-RETRY-WITNESS admitted-transport-eq=~S refused-transported=~S fresh-request=~S relocated-anchor=~S~%"
                    (eq (getf admitted :request) (first requests))
                    (not (null (member (getf refused :request) requests :test #'eq)))
                    (not (eq (getf refused :request) (getf admitted :request)))
                    (not (equal (getf refused :anchor-items) (getf admitted :anchor-items))))
            (test-assert (and (getf refused :refused-p) (not (getf admitted :refused-p)))
                         "the first ordinary candidate is refused and the retry is admitted")
            (test-assert (eq (getf admitted :request) (first requests))
                         "the admitted decision request is EQ to the inert transport request")
            (test-assert (not (member (getf refused :request) requests :test #'eq))
                         "the refused candidate never reaches inert transport")
            (test-assert (not (eq (getf refused :request) (getf admitted :request)))
                         "recovery prepares a fresh ordinary candidate")
            (test-assert (not (equal (getf refused :anchor-items) (getf admitted :anchor-items)))
                         "the retry anchor follows the native checkpoint rather than removed history")))
        result))))


;;;; -- Unissued Attempt Safety --

(-> compaction-attempt-test--run
    (&key (:mission-p boolean) (:limit (integer 1)) (:repeats (integer 1))
     (:repeated-refusal-p boolean)) null)
(defun compaction-attempt-test--run (&key mission-p limit (repeats 1) repeated-refusal-p)
  "Exercise real soft refusal, native recovery and paid numbering at a bounded turn limit."
  (with-test-environment (("AUTOLITH_CONTEXT_WINDOW" "50000")
                          ("AUTOLITH_CONTEXT_OUTPUT_RESERVE" "256"))
    (with-test-configuration (configuration)
      (let* ((provider                                   (make-instance
                                                          'prepared-native-scripted-provider :configuration configuration
                                                          :native-items                                     (loop repeat (if repeated-refusal-p 2 repeats) collect
                                                                                                              (json-object "type" "compaction" "encrypted_content" "attempt-native"))
                                                          :results                                          (loop for round from 1 to (if repeated-refusal-p 2 repeats) append
                                                                                                              (list (agent-test-result "attempt-summary"
                                                                                                                                       (list (agent-test-compaction-message))
                                                                                                                                       :turn-completion ':end)
                                                                                                                    (agent-test-result "attempt-answer"
                                                                                                                                       (list (if (and repeated-refusal-p (= round 1))
                                                                                                                                                 (agent-test-call :call-id   "attempt-continue"
                                                                                                                                                                  :arguments "{\"value\":\"continue\"}")
                                                                                                                                                 (agent-test-message "Done.")))
                                                                                                                                       :usage           (json-object "input_tokens" 2 "output_tokens" 3)
                                                                                                                                       :turn-completion (if (and repeated-refusal-p (= round 1))
                                                                                                                                                            ':continue ':end))))))
             (application                                (when mission-p (mission-inference-test--application configuration provider)))
             (conversation                               (if application (application-conversation application)
                                                               (conversation-create configuration)))
             (agent                                      (agent-create :configuration configuration :conversation conversation
                                                                       :provider provider :tool-registry (agent-test-registry) :worker nil))
             (*agent-maximum-provider-requests-per-turn* limit)
             (soft-count                                 0)
             (stream-turn                                (symbol-function 'provider-stream-turn))
             (pending                                    nil))
        (when application
          (application-mission-start application (mission-test--specification :turns 8 :tokens 8000)))
        (unwind-protect
             (test-call-with-function-replacements
              (list (list 'agent-should-compact-p
                          (lambda (candidate)
                            (declare (ignore candidate))
                            (or repeated-refusal-p (prog1 pending (setf pending nil)))))
                    (list 'provider-stream-turn
                          (lambda (&rest arguments)
                            (handler-bind ((provider-proactive-compaction-required
                                             (lambda (condition)
                                               (declare (ignore condition))
                                               (incf soft-count))))
                              (apply stream-turn arguments)))))
              (lambda ()
                (loop for turn from 1 to repeats
                      do (setf pending t soft-count 0)
                         (let ((failure nil)
                               (paid    (cond ((= limit 1)
                                               nil)
                                              ((= limit 2)
                                               '((1 :native)))
                                              ((= limit 3)
                                               '((1 :native) (2 :summary)))
                                              (repeated-refusal-p
                                               '((1 :native) (2 :summary) (3 nil) (4 :native)))
                                              (t
                                               '((1 :native) (2 :summary) (3 nil))))))
                           (handler-case (agent-run-user-turn agent "attempt continuation"
                                                              :tools-p repeated-refusal-p)
                             (agent-loop-error (condition) (setf failure condition)))
                           (format t "~&ATTEMPT-SAFETY mission=~S limit=~D turn=~D soft=~D paid=~S failure=~S~%"
                                   mission-p limit turn soft-count (agent-tests--request-numbers conversation)
                                   (not (null failure)))
                           (test-assert (= soft-count (if repeated-refusal-p 2 1))
                                        "each real unissued soft refusal is observed once and recovery is bounded")
                           (test-assert (eq (not (null failure)) (or (< limit 4) repeated-refusal-p))
                                        "the original safety allowance includes unissued attempts")
                           (test-assert (equal (loop repeat turn append paid)
                                               (agent-tests--request-numbers conversation))
                                        "paid native, summary and ordinary records are contiguous and reset each turn")
                           (when application
                             (let ((ledger (mission-context-goal (mission-context-find conversation)))
                                   (used   (cond ((= limit 1)
                                                  0)
                                                 ((= limit 2)
                                                  2)
                                                 ((= limit 3)
                                                  4)
                                                 (repeated-refusal-p
                                                  11)
                                                 (t
                                                  9))))
                               (test-assert (= (* turn used) (getf ledger :tokens-used))
                                            "unissued soft safety attempts add no paid usage")
                               (test-assert (= (* turn (if repeated-refusal-p 6 (min limit 4)))
                                               (getf ledger :turns-used))
                                            "mission attempts retain their existing ledger charge")
                               (test-assert (and (zerop (getf ledger :tokens-reserved))
                                                 (zerop (getf ledger :requests-outstanding)))
                                            "bounded recovery settles every mission reservation")))))))
          (when application (mission-detach application))))))
  nil)

(-> test-compaction-unissued-attempt-safety () null)
(defun test-compaction-unissued-attempt-safety ()
  "Keep mission and ordinary native recovery bounded without gaps in paid numbering."
  (dolist (mission-p '(nil t))
    (dolist (limit '(1 2 3 4))
      (compaction-attempt-test--run :mission-p mission-p :limit limit
                                   :repeats (if (= limit 4) 2 1)))
    (compaction-attempt-test--run :mission-p mission-p :limit 6 :repeated-refusal-p t))
  nil)
