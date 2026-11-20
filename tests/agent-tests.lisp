(in-package #:autolith)

;;;; -- Scripted Agent Boundary --

(defclass scripted-provider (model-provider)
  ((configuration
    :initarg       :configuration
    :initform      nil
    :reader        scripted-provider-configuration
    :type          (option configuration)
    :documentation "Optional configuration used to inspect request-local Skills.")
   (results
    :initarg       :results
    :accessor      scripted-provider-results
    :type          list
    :documentation "The provider results returned in request order.")
   (input-counts
    :initform      nil
    :accessor      scripted-provider-input-counts
    :type          list
    :documentation "Conversation input lengths observed before each request.")
   (input-snapshots
    :initform      nil
    :accessor      scripted-provider-input-snapshots
    :type          list
    :documentation "Request projections observed before each provider request.")
   (skill-selection-snapshots
    :initform      nil
    :accessor      scripted-provider-skill-selection-snapshots
    :type          list
    :documentation "Logical-turn Skill names observed before each request.")
   (skill-contribution-snapshots
    :initform      nil
    :accessor      scripted-provider-skill-contribution-snapshots
    :type          list
    :documentation "Skill contribution identifiers observed before each request.")
   (tool-schema-counts
    :initform      nil
    :accessor      scripted-provider-tool-schema-counts
    :type          list
    :documentation "The number of tool namespaces advertised on each request.")
   (tool-choices
    :initform      nil
    :accessor      scripted-provider-tool-choices
    :type          list
    :documentation "The tool choice bound for each request.")
   (goal-contexts
    :initform      nil
    :accessor      scripted-provider-goal-contexts
    :type          list
    :documentation "The goal context supplied on each request.")
   (compaction-flags
    :initform      nil
    :accessor      scripted-provider-compaction-flags
    :type          list
    :documentation "The compaction flag supplied on each request."))
  (:documentation "A deterministic provider for exercising repeated agent rounds."))

(defclass codex-scripted-provider (scripted-provider) ()
  (:documentation "A scripted provider in the Codex model family."))

(defmethod provider-family ((provider codex-scripted-provider))
  "Place the scripted provider in the Codex model family."
  (declare (ignore provider))
  ':codex)

(defvar *test-goal-disposition-handler* nil
  "Optional isolated auxiliary transport for deterministic goal-disposition tests.")

(defmethod provider-stream-turn
    ((provider scripted-provider)
     (conversation conversation)
     &key
     tool-namespaces
     event-callback
     goal-context
     compaction-p)
  "Return PROVIDER's next scripted result after recording request state."
  (declare (type vector tool-namespaces)
           (type function event-callback))
  (when (eq *telemetry-request-role* ':goal-disposition)
    (return-from provider-stream-turn
      (if *test-goal-disposition-handler*
          (funcall *test-goal-disposition-handler* provider conversation tool-namespaces)
          (agent-test-result
           "ordinary-disposition"
           (list (json-object "type" "message" "role" "assistant"
                              "content" (json-array (json-object "type" "output_text"
                                                                   "text" "{\"outcome\":\"continue\"}"))))
           :turn-completion ':end))))
  (let ((input-items
          (conversation-input-items-for-request
           conversation
           :include-ephemeral-p (not compaction-p))))
    (push (copy-list input-items)
          (scripted-provider-input-snapshots provider))
    (push (length input-items)
        (scripted-provider-input-counts provider))
    (push (and (skill-logical-turn-active-p)
               (skill-logical-turn-selection-names))
          (scripted-provider-skill-selection-snapshots provider))
    (push
     (and (scripted-provider-configuration provider)
          (mapcar
           #'context-contribution-identifier
           (skill-request-contributions
            (scripted-provider-configuration provider)
            conversation)))
     (scripted-provider-skill-contribution-snapshots provider)))
  (push (length tool-namespaces)
        (scripted-provider-tool-schema-counts provider))
  (push *provider-tool-choice*
        (scripted-provider-tool-choices provider))
  (push goal-context
        (scripted-provider-goal-contexts provider))
  (push compaction-p
        (scripted-provider-compaction-flags provider))
  (let ((result (pop (scripted-provider-results provider))))
    (unless result
      (error "The scripted provider has no remaining result."))
    (when (typep result 'serious-condition)
      (error result))
    (funcall event-callback
             (make-instance 'assistant-delta-event :text "delta"))
    result))

(defmethod provider-configuration ((provider scripted-provider))
  "Expose the optional scripted provider configuration to provider checks."
  (or (scripted-provider-configuration provider) *configuration*))

(defmethod provider-request-object
    ((provider scripted-provider) conversation tool-namespaces
     &key goal-context compaction-p)
  "Encode scripted input and selected tools for candidate admission tests."
  (declare (ignore provider goal-context))
  (values (json-object
           "messages" (apply #'json-array
                              (conversation-input-items-for-request
                               conversation
                               :include-ephemeral-p (not compaction-p)))
           "tools"    tool-namespaces)
          nil))

(defclass agent-test-echo-tool (tool)
  ()
  (:documentation "Return one required string to the scripted agent provider."))

(defmethod tool-execute ((tool agent-test-echo-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return the required test value without external effects."
  (declare (ignore tool context))
  (tool-success
   (format nil "echo: ~A"
           (tool-argument arguments "value" :required t))))

(defclass agent-test-read-only-echo-tool (agent-test-echo-tool)
  ()
  (:documentation "A deterministic echo tool exempt from mutating-call storm guards."))

(defmethod tool-storm-guard-exempt-p ((tool agent-test-read-only-echo-tool))
  "Exempt the deterministic read-only echo tool from storm detection."
  t)

(defclass agent-test-changing-failure-tool (tool)
  ((attempt-count
    :initform      0
    :accessor      agent-test-changing-failure-tool-attempt-count
    :type          (integer 0)
    :documentation "The number of deterministic failures returned so far."))
  (:documentation "Return a different failed result on each execution."))

(defmethod tool-execute ((tool agent-test-changing-failure-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return the next deterministic failure text for retry-diagnosis tests."
  (declare (ignore context arguments))
  (tool-failure
   (format nil "changing failure ~D"
           (incf (agent-test-changing-failure-tool-attempt-count tool)))))

(defclass agent-test-concurrency-state ()
  ((lock
    :initform      (make-lock "Autolith agent test tool state")
    :reader        agent-test-concurrency-state-lock
    :documentation "The lock protecting mutable execution state.")
   (condition-variable
    :initform      (make-condition-variable)
    :reader        agent-test-concurrency-state-condition-variable
    :documentation "The condition used to align concurrent tool starts.")
   (active-count
    :initform      0
    :accessor      agent-test-concurrency-state-active-count
    :type          (integer 0)
    :documentation "The number of currently executing test tools.")
   (maximum-active-count
    :initform      0
    :accessor      agent-test-concurrency-state-maximum-active-count
    :type          (integer 0)
    :documentation "The largest observed concurrent execution count.")
   (overlap-observed-p
    :initform      nil
    :accessor      agent-test-concurrency-state-overlap-observed-p
    :type          boolean
    :documentation "Whether two tool bodies executed at the same time.")
   (logical-turn-states
    :initform      nil
    :accessor      agent-test-concurrency-state-logical-turn-states
    :type          list
    :documentation "The turn-scoped Skill state observed by each tool worker.")
   (events
    :initform      nil
    :accessor      agent-test-concurrency-state-events
    :type          list
    :documentation "Execution start and finish events in reverse time order."))
  (:documentation "Shared state for deterministic concurrent tool tests."))

(defclass agent-test-concurrent-tool (tool)
  ((state
    :initarg       :state
    :reader        agent-test-concurrent-tool-state
    :type          agent-test-concurrency-state
    :documentation "The shared execution state recorded by this tool.")
   (execution-policy
    :initarg       :execution-policy
    :initform      ':parallel
    :reader        agent-test-concurrent-tool-execution-policy
    :type          (member :parallel :exclusive)
    :documentation "Whether this test tool requires exclusive execution.")
   (concurrency-key
    :initarg       :concurrency-key
    :initform      nil
    :reader        agent-test-concurrent-tool-concurrency-key
    :type          t
    :documentation "The optional runtime key shared with conflicting tools."))
  (:documentation "A deterministic tool that records and aligns execution."))

(defmethod tool-execution-policy ((tool agent-test-concurrent-tool))
  "Return TOOL's configured execution policy."
  (agent-test-concurrent-tool-execution-policy tool))

(defmethod tool-concurrency-key ((tool agent-test-concurrent-tool))
  "Return TOOL's configured shared-runtime key."
  (agent-test-concurrent-tool-concurrency-key tool))

(defmethod tool-execute ((tool agent-test-concurrent-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Record one test execution, optionally aligning it with a sibling call."
  (let* ((state
           (agent-test-concurrent-tool-state tool))
         (label
           (tool-argument arguments "label" :required t))
         (delay
           (or (tool-argument arguments "delay") 0.0d0))
         (await-peer-p
           (eq (tool-argument arguments "await_peer") t))
         (fail-p
           (eq (tool-argument arguments "fail") t))
         (fatal
           (tool-argument arguments "fatal")))
    (with-lock-held ((agent-test-concurrency-state-lock state))
      (push *skill-logical-turn-state*
            (agent-test-concurrency-state-logical-turn-states state))
      (incf (agent-test-concurrency-state-active-count state))
      (setf (agent-test-concurrency-state-maximum-active-count state)
            (max (agent-test-concurrency-state-maximum-active-count state)
                 (agent-test-concurrency-state-active-count state)))
      (push (list ':start label)
            (agent-test-concurrency-state-events state))
      (when (> (agent-test-concurrency-state-active-count state) 1)
        (setf (agent-test-concurrency-state-overlap-observed-p state) t)
        (condition-notify
         (agent-test-concurrency-state-condition-variable state)))
      (when (and await-peer-p
                 (= (agent-test-concurrency-state-active-count state) 1))
        (condition-wait
         (agent-test-concurrency-state-condition-variable state)
         (agent-test-concurrency-state-lock state)
         :timeout 0.5)))
    (unwind-protect
         (progn
           (agent-observer-status
            (tool-context-observer context)
            ':agent-test-tool-callback
            (list :label label))
           (sleep delay)
           (when fail-p
             (error "requested test failure"))
           (cond
             ((equal fatal "rollback")
              (error 'rollback-requested
                     :message       "requested rollback test"
                     :generation-id "test-generation"))
             ((equal fatal "corruption")
              (error
               'active-image-corruption
               :message "requested corruption test"
               :original-condition
               (make-condition 'simple-error
                               :format-control "original failure")
               :restoration-condition
               (make-condition 'simple-error
                               :format-control "restoration failure")))
             ((equal fatal "job-aborted")
              (error 'job-aborted
                     :message    "requested job abort test"
                     :identifier "test-job"
                     :reason     ':test)))
           (tool-success (format nil "completed: ~A" label)))
      (with-lock-held ((agent-test-concurrency-state-lock state))
        (push (list ':finish label)
              (agent-test-concurrency-state-events state))
        (decf (agent-test-concurrency-state-active-count state))))))

(-> agent-test-registry () tool-registry)
(defun agent-test-registry ()
  "Return a registry containing the deterministic echo tool."
  (let ((registry (make-instance 'tool-registry)))
    (tool-registry-register
     registry
     (make-instance
      'agent-test-echo-tool
      :namespace   "test"
      :name        "echo"
      :description "Echo a test string."
      :parameters
      (tool-object-schema
       (json-object
        "value" (tool-string-property "The value to echo."))
       '("value"))))
    registry))

(-> agent-test-read-only-registry () tool-registry)
(defun agent-test-read-only-registry ()
  "Return a registry containing one storm-guard-exempt echo tool."
  (let ((registry (make-instance 'tool-registry)))
    (tool-registry-register
     registry
     (make-instance
      'agent-test-read-only-echo-tool
      :namespace   "test"
      :name        "inspect"
      :description "Echo a read-only test string."
      :parameters
      (tool-object-schema
       (json-object
        "value" (tool-string-property "The value to echo."))
       '("value"))))
    registry))

(-> agent-test-changing-failure-registry () tool-registry)
(defun agent-test-changing-failure-registry ()
  "Return a registry containing one stateful deterministic failing tool."
  (let ((registry (make-instance 'tool-registry)))
    (tool-registry-register
     registry
     (make-instance
      'agent-test-changing-failure-tool
      :namespace   "test"
      :name        "changing-failure"
      :description "Return a changing deterministic failure."
      :parameters
      (tool-object-schema
       (json-object
        "value" (tool-string-property "The stable retry identity."))
       '("value"))))
    registry))

(-> agent-test-restricted-registry () tool-registry)
(defun agent-test-restricted-registry ()
  "Return read-only and mutation-labelled deterministic test tools."
  (let ((registry (agent-test-registry)))
    (tool-registry-register
     registry
     (make-instance
      'agent-test-echo-tool
      :namespace   "mutation"
      :name        "write"
      :description "Represent a forbidden mutation tool."
      :parameters
      (tool-object-schema
       (json-object
        "value" (tool-string-property "The value to echo."))
       '("value"))))
    registry))

(-> agent-test-call
    (&key
     (:call-id (option string))
     (:namespace string)
     (:name string)
     (:arguments string))
    json-object)
(defun agent-test-call
    (&key call-id (namespace "test") (name "echo") (arguments "{}"))
  "Return a scripted function call with optional CALL-ID."
  (let ((call (json-object
               "type" "function_call"
               "namespace" namespace
               "name" name
               "arguments" arguments)))
    (when call-id
      (setf (gethash "call_id" call) call-id))
    call))

(-> agent-test-message (string) json-object)
(defun agent-test-message (text)
  "Return one scripted assistant message containing TEXT."
  (json-object
   "type" "message"
   "role" "assistant"
   "content" (json-array
              (json-object "type" "output_text" "text" text))))

(-> agent-test-checkpoint-call (&optional string) json-object)
(defun agent-test-checkpoint-call
    (&optional (arguments "{\"intent\":\"Resume the pending request.\",\"notes\":[{\"kind\":\"completed\",\"text\":\"Earlier work is recorded.\",\"refs\":[]}],\"next\":\"Verify the result.\",\"next_refs\":[]}"))
  "Return a scripted compaction.submit call with ARGUMENTS, by default a valid checkpoint."
  (agent-test-call :call-id "checkpoint" :namespace "compaction" :name "submit"
                   :arguments arguments))

(-> agent-test-refusal () json-object)
(defun agent-test-refusal ()
  "Return a scripted assistant message that refuses."
  (json-object "type" "message" "role" "assistant"
               "content" (json-array (json-object "type" "refusal" "refusal" "No."))))

(-> agent-test-result
    (string list
     &key
     (:turn-completion turn-completion)
     (:usage json-object)
     (:reasoning-context (option string)))
    provider-result)
(defun agent-test-result
    (response-id output-items
     &key (turn-completion :unspecified)
       (usage (json-object "input_tokens" 1 "output_tokens" 1))
       reasoning-context)
  "Return a scripted provider result containing OUTPUT-ITEMS.

REASONING-CONTEXT is the retention mode its response metadata reports."
  (make-instance 'provider-result
                 :response-id     response-id
                 :output-items    output-items
                 :tool-calls      (remove-if-not #'function-call-item-p output-items)
                 :usage           usage
                 :metadata        (make-instance 'provider-response-metadata
                                                 :reasoning-context reasoning-context)
                 :turn-completion turn-completion))

(-> agent-test-concurrency-tool
    (agent-test-concurrency-state string
     &key (:execution-policy (member :parallel :exclusive))
          (:concurrency-key t))
    agent-test-concurrent-tool)
(defun agent-test-concurrency-tool
    (state name &key (execution-policy ':parallel) concurrency-key)
  "Create one concurrency test tool named NAME sharing STATE."
  (make-instance
   'agent-test-concurrent-tool
   :namespace        "concurrency"
   :name             name
   :description      "Record deterministic concurrent execution."
   :parameters
   (tool-object-schema
    (json-object
     "label" (tool-string-property "The execution label.")
     "delay" (json-object "type" "number")
     "await_peer" (tool-boolean-property "Wait for one concurrent peer.")
     "fail" (tool-boolean-property "Signal a test failure.")
     "fatal" (tool-string-property
              "The optional fatal condition kind to signal."))
    '("label"))
   :state            state
   :execution-policy execution-policy
   :concurrency-key  concurrency-key))

(-> agent-test-concurrency-registry (list) tool-registry)
(defun agent-test-concurrency-registry (tools)
  "Return a registry containing concurrency test TOOLS."
  (let ((registry (make-instance 'tool-registry)))
    (dolist (tool tools)
      (tool-registry-register registry tool))
    registry))

(-> agent-test-tool-outputs (conversation) list)
(defun agent-test-tool-outputs (conversation)
  "Return provider-visible tool outputs from CONVERSATION in durable order."
  (loop for item in (conversation-input-items conversation)
        when (string= (or (json-get item "type") "")
                      "function_call_output")
          collect (json-get item "output")))

(-> test-agent-tool-loop () null)
(defun test-agent-tool-loop ()
  "Test authoritative replay, correlated tool output, and callbacks."
  (let* ((configuration       (test-configuration))
         (root                (test-configuration-root configuration))
         (conversation        (conversation-create configuration :identifier "agent-loop"))
         (call
           (json-object
            "type" "function_call"
            "call_id" "call-1"
            "namespace" "test"
            "name" "echo"
            "arguments" "{\"value\":\"hello\"}"))
         (blank-message       (agent-test-message "   "))
         (message
           (json-object
            "type" "message"
            "role" "assistant"
            "content" (json-array
                       (json-object
                        "type" "output_text"
                        "text" "complete"))))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "response-1"
              (list call blank-message))
             (agent-test-result
              "response-2"
              (list message)
              :reasoning-context "all_turns"
              :usage             (json-object
                                  "input_tokens" 10
                                  "output_tokens" 2
                                  "input_tokens_details"
                                  (json-object "cached_tokens" 8
                                               "cache_write_tokens" 1))))))
         (registry            (agent-test-registry))
         (deltas              nil)
         (statuses            nil)
         (completed-usages    nil)
         (persisted-responses nil))
    (unwind-protect
         (let* ((agent
                  (agent-create
                   :configuration configuration
                   :provider      provider
                   :conversation  conversation
                   :tool-registry registry
                   :worker        ':unused))
                (observer
                  (callback-agent-observer-create
                   :text-callback
                   (lambda (text)
                     (push text deltas))
                   :status-callback
                   (lambda (status details)
                     (push status statuses)
                     (when (eq status ':provider-request-completed)
                       (push (copy-tree (getf details :usage))
                             completed-usages))
                     (when (eq status ':assistant-response-persisted)
                       (let ((text (getf details :text)))
                         (push
                          (list
                           :details (copy-tree details)
                           :durable-p
                           (some
                            (lambda (item)
                              (and
                               (json-object-p item)
                               (string=
                                (or (response-item-assistant-text item) "")
                                text)))
                            (conversation-input-items conversation)))
                          persisted-responses))))))
                (result
                  (agent-run-user-turn
                   agent "run the echo" :observer observer)))
           (test-assert
            (string= (provider-result-response-id result) "response-2")
            "the agent returns the final tool-free provider result")
           (test-assert
            (equal (nreverse (scripted-provider-input-counts provider))
                   '(1 4))
            "the second request replays the call, blank message, and tool output")
           (test-assert
            (= (length (conversation-input-items conversation)) 5)
            "conversation history contains user, call, blank answer, output, and answer")
           (test-assert
            (equal (nreverse deltas) '("delta" "delta"))
            "the observer receives deltas from every provider request")
           (test-assert
            (member :tool-call-completed statuses)
            "the observer receives correlated tool lifecycle status")
           (test-assert
            (member :user-message-persisted statuses)
            "the observer learns when user input becomes durable")
           (let ((usage (first completed-usages)))
             (test-assert
              (and (= (second (assoc "cached_input_tokens" usage
                                     :test #'string=))
                      8)
                   (= (second (assoc "cache_creation_input_tokens" usage
                                     :test #'string=))
                      1))
              "provider completion status carries normalized cache usage"))
           (let* ((responses        (nreverse persisted-responses))
                  (response         (first responses))
                  (details          (getf response :details))
                  (ordered-statuses (reverse statuses))
                  (response-position
                    (position ':assistant-response-persisted ordered-statuses))
                  (completion-position
                    (position ':provider-request-completed ordered-statuses
                              :from-end t)))
             (test-assert
              (and (= (length responses) 1)
                   (= (getf details :request-number) 2)
                   (string= (getf details :response-id) "response-2")
                   (string= (getf details :text) "complete")
                   (typep (getf details :time) 'timestamp)
                   (getf response :durable-p))
              "only durable nonblank verbal provider results emit response status")
             (test-assert
              (and response-position
                   completion-position
                   (= (1+ response-position) completion-position))
              "durable verbal response status precedes request completion"))
           (let* ((records
                    (conversation--read-records
                     (conversation-pathname conversation)))
                  (tool-result
                    (find :tool-result records :key #'first))
                  (provider-record
                    (find-if
                     (lambda (record)
                       (and (eq (first record) :provider)
                            (string= (getf (getf (rest record) :metadata)
                                           :response-id)
                                     "response-2")))
                     records))
                  (persisted-usage
                    (getf (getf (rest provider-record) :metadata) :usage)))
             (test-assert
              (and (typep (getf (rest tool-result) :cpu-microseconds)
                          '(integer 0))
                   (typep (getf (rest tool-result) :real-microseconds)
                          '(integer 0)))
              "executed tool results persist CPU and real timing")
             (test-assert
              (and (= (second (assoc "cached_input_tokens" persisted-usage
                                     :test #'string=))
                      8)
                   (= (second (assoc "cache_creation_input_tokens"
                                     persisted-usage :test #'string=))
                      1))
              "provider metadata persists normalized cache usage")
             (test-assert
              (equal "all_turns"
                     (getf (getf (rest provider-record) :metadata) :reasoning-context))
              "provider metadata records the reasoning retention mode the response reports"))
           (test-assert
            (= (count :provider-progress statuses) 2)
            "every streamed delta refreshes visible provider progress"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-tool-free-turn () null)
(defun test-agent-tool-free-turn ()
  "Test diagnosis-style turns advertise and execute no tools."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-tool-free"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "tool-free"
              (list (agent-test-message "diagnosis only"))))))
         (agent
           (agent-create :configuration configuration
                         :provider      provider
                         :conversation  conversation
                         :tool-registry (agent-test-registry)
                         :worker        ':unused)))
    (unwind-protect
         (progn
           (agent-run-user-turn agent "diagnose the crash" :tools-p nil)
           (test-assert
            (equal (scripted-provider-tool-schema-counts provider) '(0))
            "a tool-free turn advertises no tool schemas")
           (let* ((call-conversation
                    (conversation-create
                     configuration :identifier "agent-tool-free-call"))
                  (call-provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list
                      (agent-test-result
                       "forbidden-call"
                       (list
                        (agent-test-call
                         :call-id   "forbidden-call"
                         :arguments "{\"value\":\"no\"}"))))))
                  (call-agent
                    (agent-create
                     :configuration configuration
                     :provider      call-provider
                     :conversation  call-conversation
                     :tool-registry (agent-test-registry)
                     :worker        ':unused)))
             (test-assert
              (handler-case
                  (progn
                    (agent-run-user-turn
                     call-agent
                     "do not call tools"
                     :tools-p nil)
                    nil)
                (agent-loop-error ()
                  t))
              "a tool-free turn rejects provider function calls")
             (test-assert
              (= (length (conversation-input-items call-conversation)) 1)
              "a rejected tool-free call is never persisted or executed")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-read-only-tool-allowlist () null)
(defun test-agent-read-only-tool-allowlist ()
  "Test restricted turns advertise and execute only explicitly allowed tools."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-read-only"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "read-only-call"
              (list
               (agent-test-call
                :call-id   "read-only-call"
                :arguments "{\"value\":\"inspect\"}")))
             (agent-test-result
              "read-only-answer"
              (list (agent-test-message "diagnosed"))
              :turn-completion ':end))))
         (agent
           (agent-create :configuration configuration
                         :provider      provider
                         :conversation  conversation
                         :tool-registry (agent-test-restricted-registry)
                         :worker        ':unused)))
    (unwind-protect
         (progn
           (let ((*agent-restricted-maximum-tool-rounds* 1))
             (agent-run-user-turn
              agent
              "diagnose safely"
              :tool-allowlist     '("test.echo")
              :tool-restriction-p t))
           (test-assert
            (and (equal (scripted-provider-tool-schema-counts provider) '(2 2))
                 (equal (scripted-provider-tool-choices provider) '(:none :auto)))
            "a restricted turn keeps its tool schemas but disables calls after its bounded round")
           (test-assert
            (equal (nreverse (scripted-provider-input-counts provider)) '(1 3))
            "an allowed read-only call executes and returns to the same turn")
           (let* ((forbidden-conversation
                    (conversation-create
                     configuration :identifier "agent-read-only-forbidden"))
                  (forbidden-provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list
                      (agent-test-result
                       "forbidden-mutation"
                       (list
                        (agent-test-call
                         :call-id   "forbidden-mutation"
                         :namespace "mutation"
                         :name      "write"
                         :arguments "{\"value\":\"change\"}"))))))
                  (forbidden-agent
                    (agent-create
                     :configuration configuration
                     :provider      forbidden-provider
                     :conversation  forbidden-conversation
                     :tool-registry (agent-test-restricted-registry)
                     :worker        ':unused)))
             (test-assert
              (handler-case
                  (progn
                    (agent-run-user-turn
                     forbidden-agent
                     "do not mutate"
                     :tool-allowlist     '("test.echo")
                     :tool-restriction-p t)
                    nil)
                (agent-loop-error ()
                  t))
              "a restricted turn rejects a non-allowlisted mutation call")
             (test-assert
              (= (length (conversation-input-items forbidden-conversation)) 1)
              "a forbidden mutation call is neither persisted nor executed")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-restricted-resource-schemes () null)
(defun test-agent-restricted-resource-schemes ()
  "Test restricted turns confine generic resource reads to workspace URIs."
  (let* ((base-configuration (test-configuration))
         (root               (test-configuration-root base-configuration))
         (workspace          (merge-pathnames "restricted-workspace/" root))
         (configuration
           (configuration-copy base-configuration
                                 :working-directory workspace))
         (conversation
           (conversation-create configuration
                                :identifier "agent-restricted-resources"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "restricted-workspace-read"
              (list
               (agent-test-call
                :call-id   "restricted-workspace-read"
                :namespace "resource"
                :name      "read"
                :arguments
                (json-encode (json-object "uri" "workspace:allowed.txt")))))
             (agent-test-result
              "restricted-agenda-read"
              (list
               (agent-test-call
                :call-id   "restricted-agenda-read"
                :namespace "resource"
                :name      "read"
                :arguments
                (json-encode (json-object "uri" "agenda:current")))))
             (agent-test-result
              "restricted-memory-read"
              (list
               (agent-test-call
                :call-id   "restricted-memory-read"
                :namespace "resource"
                :name      "read"
                :arguments
                (json-encode (json-object "uri" "memory:relevant")))))
             (agent-test-result
              "restricted-resource-answer"
              (list (agent-test-message "diagnosed"))
              :turn-completion ':end))))
         (registry           (make-default-tool-registry))
         (agent
           (agent-create :configuration configuration
                         :provider      provider
                         :conversation  conversation
                         :tool-registry registry
                         :worker        ':unused)))
    (unwind-protect
         (progn
           (ensure-directories-exist workspace)
           (with-open-file (stream (merge-pathnames "allowed.txt" workspace)
                                   :direction         ':output
                                   :if-exists         ':supersede
                                   :if-does-not-exist ':create
                                   :external-format   ':utf-8)
             (write-string "restricted workspace content" stream))
           (let ((codes   (make-hash-table :test #'equal))
                 (execute (symbol-function 'tool-registry-execute-call)))
             (test-call-with-function-replacements
              (list
               (list 'tool-registry-execute-call
                     (lambda (registry call context)
                       (let ((result (funcall execute registry call context)))
                         (setf (gethash (json-get call "call_id") codes)
                               (tool-result-error-code result))
                         result))))
              (lambda ()
                (let ((*agent-restricted-maximum-tool-rounds* 3))
                  (agent-run-user-turn
                   agent
                   "diagnose within the workspace"
                   :tool-allowlist     '("resource.read")
                   :tool-restriction-p t))))
             (let ((outputs
                     (loop for item in (conversation-input-items conversation)
                           when (string= (or (json-get item "type") "")
                                         "function_call_output")
                             collect (json-get item "output"))))
               (test-assert
                (and (= (length outputs) 3)
                     (search "restricted workspace content" (first outputs)))
                "a restricted turn may read workspace resources")
               (test-assert
                (and (search "agenda:current" (second outputs))
                     (eq (gethash "restricted-agenda-read" codes) ':access-denied))
                "a restricted turn rejects agenda resources")
               (test-assert
                (and (search "memory:relevant" (third outputs))
                     (eq (gethash "restricted-memory-read" codes) ':access-denied))
                "a restricted turn rejects memory resources"))))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-restricted-tool-round-limit () null)
(defun test-agent-restricted-tool-round-limit ()
  "Test restricted turns reject calls returned after their bounded tool rounds."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-tool-limit"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "tool-limit-first"
              (list
               (agent-test-call
                :call-id   "tool-limit-first"
                :arguments "{\"value\":\"first\"}")))
             (agent-test-result
              "tool-limit-second"
              (list
               (agent-test-call
                :call-id   "tool-limit-second"
                :arguments "{\"value\":\"second\"}"))))))
         (agent
           (agent-create :configuration configuration
                         :provider      provider
                         :conversation  conversation
                         :tool-registry (agent-test-restricted-registry)
                         :worker        ':unused)))
    (unwind-protect
         (let ((*agent-restricted-maximum-tool-rounds* 1))
           (test-assert
            (handler-case
                (progn
                  (agent-run-user-turn
                   agent
                   "inspect once"
                   :tool-allowlist     '("test.echo")
                   :tool-restriction-p t)
                  nil)
              (agent-loop-error (condition)
                (search "tool-round limit" (format nil "~A" condition))))
            "a restricted turn rejects calls beyond its tool-round limit")
           (test-assert
            (and (equal (scripted-provider-tool-schema-counts provider) '(2 2))
                 (equal (scripted-provider-tool-choices provider) '(:none :auto)))
            "the provider may not call tools after the restricted limit")
           (test-assert
            (= (length (conversation-input-items conversation)) 3)
            "the over-limit call is rejected before persistence or execution"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-empty-tool-allowlist () null)
(defun test-agent-empty-tool-allowlist ()
  "Test an explicit empty restriction advertises and executes no tools."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-empty-tools"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "empty-allowlist-call"
              (list
               (agent-test-call
                :call-id   "empty-allowlist-call"
                :arguments "{\"value\":\"blocked\"}"))))))
         (agent
           (agent-create :configuration configuration
                         :provider      provider
                         :conversation  conversation
                         :tool-registry (agent-test-restricted-registry)
                         :worker        ':unused)))
    (unwind-protect
         (progn
           (test-assert
            (handler-case
                (progn
                  (agent-run-user-turn
                   agent
                   "inspect nothing"
                   :tool-allowlist     nil
                   :tool-restriction-p t)
                  nil)
              (agent-loop-error ()
                t))
            "an explicit empty restriction rejects every function call")
           (test-assert
            (equal (scripted-provider-tool-schema-counts provider) '(0))
            "an explicit empty restriction advertises zero namespaces")
           (test-assert
            (= (length (conversation-input-items conversation)) 1)
            "an empty restriction persists no rejected call or tool result"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-steering () null)
(defun test-agent-steering ()
  "Test pending user input is persisted after a tool round and before its follow-up."
  (let* ((configuration         (test-configuration))
         (root                  (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-steering"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "steering-1"
              (list (agent-test-call
                     :call-id   "steering-call"
                     :arguments "{\"value\":\"before\"}")))
             (agent-test-result
              "steering-2"
              (list (agent-test-message "changed course"))
              :turn-completion ':end))))
         (pending-input
           (list
            (agent-steering-input-create
             :identifier "steering-persisted"
             :content    "change direction")
            (agent-steering-input-create
             :identifier        "channel:steering-message"
             :content           "[Message from @other (al-other) to channel agent-steering, where you reside]"
             :operation-request '(:operation "channel" :summary "hello"))))
         (persisted-identifiers nil)
         (statuses              nil))
    (unwind-protect
         (let* ((agent
                  (agent-create :configuration configuration
                                :provider      provider
                                :conversation  conversation
                                :tool-registry (agent-test-registry)
                                :worker        nil))
                (observer
                  (callback-agent-observer-create
                   :steering-callback
                   (lambda ()
                     (prog1 pending-input
                       (setf pending-input nil)))
                   :steering-persisted-callback
                   (lambda (identifier)
                     (push identifier persisted-identifiers))
                   :status-callback
                   (lambda (status details)
                     (declare (ignore details))
                     (push status statuses)))))
           (agent-run-user-turn agent "start here" :observer observer)
           (test-assert
            (equal (nreverse (scripted-provider-input-counts provider))
                   '(1 5))
            "steering follows the function call and correlated tool output")
           (let ((user-records
                   (loop for record in
                           (rest (conversation--read-records
                                  (conversation-pathname conversation)))
                         when (and (eq (first record) ':message)
                                   (eq (getf (rest record) :role) ':user))
                           collect record)))
             (test-assert
              (equal (mapcar (lambda (record)
                               (getf (rest record) :content))
                             user-records)
                     '("start here" "change direction"
                       "[Message from @other (al-other) to channel agent-steering, where you reside]"))
              "steering is durable user input")
             (test-assert
              (string= (getf (rest (second user-records))
                             :pending-input-identifier)
                       "steering-persisted")
              "identified steering records its pending provenance")
             (test-assert
              (and (not (getf (rest (second user-records)) :automatic-p))
                   (getf (rest (third user-records)) :automatic-p)
                   (equal (getf (rest (third user-records)) :operation-request)
                          '(:operation "channel" :summary "hello")))
              "steering an operation generated is automatic input recording that operation"))
           (test-assert
            (equal persisted-identifiers '("channel:steering-message" "steering-persisted"))
            "the observer acknowledges each steering append immediately")
           (test-assert (member :steering-applied statuses)
                        "the observer is notified after steering becomes durable"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-explicit-continuation () null)
(defun test-agent-explicit-continuation ()
  "Test a tool-free explicit continuation receives another bounded request."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation  (conversation-create configuration :identifier "agent-continue"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list (agent-test-result "response-1"
                                     (list (agent-test-message "working"))
                                     :turn-completion ':continue)
                  (agent-test-result "response-2"
                                     (list (agent-test-message "done"))
                                     :turn-completion ':end)))))
    (unwind-protect
         (let* ((agent  (agent-create
                         :configuration configuration
                         :provider      provider
                         :conversation  conversation
                         :tool-registry (agent-test-registry)
                         :worker        ':unused))
                (result (agent-run-user-turn agent "continue explicitly")))
           (test-assert (string= (provider-result-response-id result) "response-2")
                        "the agent follows an explicit provider continuation")
           (test-assert
            (equal (nreverse (scripted-provider-input-counts provider)) '(1 2))
            "the continuation request replays the first completed message"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-provider-request-limit () null)
(defun test-agent-provider-request-limit ()
  "Test an ordinary turn stops before an unbounded provider continuation loop."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-request-limit"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (loop for index from 1 to 3
                  collect
                  (agent-test-result
                   (format nil "response-~D" index)
                   (list (agent-test-message "still working"))
                   :turn-completion ':continue)))))
    (unwind-protect
         (let* ((agent
                  (agent-create
                   :configuration configuration
                   :provider      provider
                   :conversation  conversation
                   :tool-registry (agent-test-registry)
                   :worker        ':unused))
                (condition                                  nil)
                (*agent-maximum-provider-requests-per-turn* 2))
           (handler-case
               (agent-run-user-turn agent "keep continuing")
             (agent-loop-error (caught)
               (setf condition caught)))
           (test-assert
            (and condition
                 (= (agent-loop-error-request-number condition) 2))
            "the request safety limit reports the completed request count")
           (test-assert
            (= (length (scripted-provider-input-snapshots provider)) 2)
            "the request safety limit prevents another paid provider call"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-invalid-call-history () null)
(defun test-agent-invalid-call-history ()
  "Test uncorrelatable and duplicate calls cannot poison durable history."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let* ((conversation
                    (conversation-create configuration :identifier "missing-call-id"))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list
                      (agent-test-result
                       "missing-id"
                       (list (agent-test-call :arguments "{\"value\":\"x\"}"))))))
                  (agent
                    (agent-create :configuration configuration
                                  :provider      provider
                                  :conversation  conversation
                                  :tool-registry (agent-test-registry)
                                  :worker        ':unused)))
             (test-assert
              (handler-case
                  (progn
                    (agent-run-user-turn agent "reject missing id")
                    nil)
                (agent-loop-error ()
                  t))
              "a call without correlation identity is rejected")
             (test-assert (= (length (conversation-input-items conversation)) 1)
                          "an uncorrelatable provider item is never persisted"))
           (let* ((conversation
                    (conversation-create configuration :identifier "duplicate-call-id"))
                  (first-call
                    (agent-test-call :call-id   "duplicate"
                                     :arguments "{\"value\":\"first\"}"))
                  (duplicate-call
                    (agent-test-call :call-id   "duplicate"
                                     :arguments "{\"value\":\"second\"}"))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list (agent-test-result "first" (list first-call))
                           (agent-test-result "duplicate" (list duplicate-call))
                           (agent-test-result
                            "duplicate-final"
                            (list (agent-test-message "duplicate handled"))))))
                  (agent
                    (agent-create :configuration configuration
                                  :provider      provider
                                  :conversation  conversation
                                  :tool-registry (agent-test-registry)
                                  :worker        ':unused))
                  (result (agent-run-user-turn agent "accept duplicate id")))
             (test-assert
              (string= "duplicate-final" (provider-result-response-id result))
              "a repeated call identity does not end the turn")
             (test-assert
              (equal '("duplicate" "duplicate-2")
                     (loop for item in (conversation-input-items conversation)
                           when (function-call-item-p item)
                             collect (json-get item "call_id")))
              "a repeated call identity is renamed before persistence")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-duplicate-call-identity () null)
(defun test-agent-duplicate-call-identity ()
  "Refuse a reused call identifier without corrupting the transcript."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                  :identifier "duplicate-call-identity"))
                (call         (agent-test-call :call-id "reused"))
                (reordered    (json-object "status" "completed"
                                           "call_id" "reused"
                                           "arguments" "{}"
                                           "name" "echo"
                                           "namespace" "test"
                                           "type" "function_call"))
                (conflict     (agent-test-call :call-id   "reused"
                                               :arguments "{\"value\":1}"))
                (anonymous    (json-object "type" "function_call"
                                           "arguments" "{}"
                                           "name" "echo"
                                           "namespace" "test")))
           (conversation-append-provider-item conversation call)
           (let ((records (conversation-next-sequence conversation)))
             (test-assert (handler-case
                              (progn
                                (conversation-append-provider-item conversation call)
                                nil)
                            (provider-call-conflict () t))
                          "repeating the same call is refused")
             (test-assert (handler-case
                              (progn
                                (conversation-append-provider-item
                                 conversation reordered)
                                nil)
                            (provider-call-conflict () t))
                          "repeating the same call in another serialization is refused")
             (test-assert (= records (conversation-next-sequence conversation))
                          "a repeated call adds no record")
             (test-assert (= 1 (count-if #'function-call-item-p
                                         (conversation-input-items conversation)))
                          "a repeated call is projected once"))
           (test-assert (handler-case
                            (progn
                              (conversation-append-provider-item conversation conflict)
                              nil)
                          (provider-call-conflict () t))
                        "a reused identifier that names another call is refused")
           (test-assert (handler-case
                            (progn
                              (conversation-append-provider-item conversation anonymous)
                              nil)
                          (conversation-invariant-error () t))
                        "a call without an identifier is refused")
           (test-assert (= 1 (count-if #'function-call-item-p
                                       (conversation-input-items conversation)))
                        "a refused call is never persisted"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-repeated-call-identifiers () null)
(defun test-agent-repeated-call-identifiers ()
  "Test a provider that repeats a call identifier still names distinct calls."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create
                               configuration :identifier "repeated-call-identifiers"))
                (agent        (agent-create
                               :configuration configuration
                               :provider      (make-instance 'scripted-provider :results nil)
                               :conversation  conversation
                               :tool-registry (agent-test-registry)
                               :worker        ':unused))
                (first-call   (agent-test-call :call-id   "shared"
                                               :arguments "{\"value\":1}"))
                (second-call  (agent-test-call :call-id   "shared"
                                               :arguments "{\"value\":2}"))
                (unique-call  (agent-test-call :call-id   "shared-2"
                                               :arguments "{\"value\":3}")))
           (agent--validate-tool-call-identifiers
            agent (list first-call second-call unique-call) :request-number 1)
           (test-assert (string= "shared" (json-get first-call "call_id"))
                        "the first call keeps its identifier")
           (test-assert (string= "shared-3" (json-get second-call "call_id"))
                        "a repeated identifier gets a fresh one")
           (test-assert (string= "shared-2" (json-get unique-call "call_id"))
                        "a replacement never takes an identifier the response names")
           (conversation-append-provider-item conversation first-call)
           (conversation-append-provider-item conversation second-call)
           (conversation-append-provider-item conversation unique-call)
           (let ((later-call (agent-test-call :call-id   "shared"
                                              :arguments "{\"value\":4}")))
             (agent--validate-tool-call-identifiers
              agent (list later-call) :request-number 2)
             (test-assert (string= "shared-4" (json-get later-call "call_id"))
                          "an identifier the conversation already names is replaced")
             (test-assert (handler-case
                              (progn
                                (conversation-append-provider-item conversation later-call)
                                t)
                            (provider-call-conflict () nil))
                          "the replacement identifier is appended without a conflict")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-refused-item-keeps-usage () null)
(defun test-agent-refused-item-keeps-usage ()
  "Record billed usage once across item refusal, early validation and acceptance."
  (with-test-configuration (configuration)
    (dolist (scenario '(:duplicate :arguments :missing-identifier :accepted))
      (let* ((conversation (conversation-create
                            configuration :identifier (string-downcase (symbol-name scenario))))
             (items        (case scenario
                             (:duplicate
                              (list (agent-test-call :call-id "twice" :arguments "{}")
                                    (agent-test-call :call-id "twice" :arguments "{\"value\":1}")))
                             (:arguments
                              (list (agent-test-call :call-id "string" :arguments "\"{}\"")))
                             (:missing-identifier
                              (list (agent-test-call :arguments "{}")))
                             (:accepted
                              (list (agent-test-message "accepted")))))
             (result       (agent-test-result "accounted" items))
             (agent        (agent-create
                            :configuration configuration
                            :provider      (make-instance 'scripted-provider :results (list result))
                            :conversation  conversation
                            :tool-registry (agent-test-registry)
                            :worker        ':unused))
             (before       (conversation-next-sequence conversation))
             (failed-p     (handler-case
                               (progn
                                 (if (eq scenario ':missing-identifier)
                                     (agent-run-user-turn agent "reject missing identity")
                                     (agent--persist-provider-result agent result :request-number 1))
                                 nil)
                             (agent-loop-error () t))))
        (test-assert (eq failed-p (not (eq scenario ':accepted)))
                     "refused responses report the agent boundary failure")
        (test-assert (equal '(1 1 2) (subseq (conversation-usage conversation) 0 3))
                     "each response accounts for exactly its billed tokens")
        (let* ((records (conversation--read-records (conversation-pathname conversation)))
               (usage-records
                 (remove-if-not
                  (lambda (record)
                    (and (eq (first record) ':provider)
                         (getf (getf (rest record) :metadata) :usage)))
                  records)))
          (test-assert (= (length usage-records) 1)
                       "refusal and acceptance each append one usage record"))
        (when (eq scenario ':accepted)
          (test-assert (= (- (conversation-next-sequence conversation) before) 2)
                       "accepted responses append one item and one usage record")))))
  nil)

(-> test-agent-malformed-tool-arguments () null)
(defun test-agent-malformed-tool-arguments ()
  "Test malformed tool arguments fail safely and legacy poison is repaired on replay."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (malformed     "{\"value\":\"first\"}{\"value\":\"second\"}"))
    (unwind-protect
         (progn
           (let* ((conversation
                    (conversation-create
                     configuration :identifier "malformed-tool-arguments"))
                  (call
                    (agent-test-call
                     :call-id "malformed-call" :arguments malformed))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list
                      (agent-test-result "malformed-call" (list call))
                      (agent-test-result
                       "malformed-final"
                       (list (agent-test-message "malformed call handled"))))))
                  (agent
                    (agent-create :configuration configuration
                                  :provider      provider
                                  :conversation  conversation
                                  :tool-registry (agent-test-registry)
                                  :worker        ':unused))
                  (result  (agent-run-user-turn agent "handle malformed arguments"))
                  (snapshots
                    (nreverse (scripted-provider-input-snapshots provider)))
                  (replayed-call
                    (find-if #'function-call-item-p (second snapshots)))
                  (outputs (agent-test-tool-outputs conversation)))
             (test-assert
              (string= (provider-result-response-id result) "malformed-final")
              "malformed arguments do not stop the provider tool loop")
             (test-assert
              (string= (json-get replayed-call "arguments") "{}")
              "the next provider request receives replayable call arguments")
             (test-assert
              (and (= (length outputs) 1)
                   (search "not a valid JSON object" (first outputs)))
              "malformed arguments receive one correlated failed tool result")
             (let* ((records
                      (conversation--read-records
                       (conversation-pathname conversation)))
                    (provider-record
                      (find ':provider-item records :key #'first))
                    (persisted-call
                      (json-decode (getf (rest provider-record) :wire-json))))
               (test-assert
                (string= (json-get persisted-call "arguments") "{}")
                "new malformed calls are sanitized before durable persistence")))
           (let* ((conversation
                    (conversation-create
                     configuration :identifier "legacy-malformed-tool-arguments"))
                  (call
                    (agent-test-call
                     :call-id "legacy-malformed-call" :arguments malformed)))
             (conversation-append-user-message conversation "legacy history")
             (conversation-append-record
              conversation
              (list :provider-item :wire-json (json-encode call)))
             (conversation-append-tool-result
              conversation
              "legacy-malformed-call"
              :tool-name "test.echo"
              :output    "The legacy call completed."
              :success-p t)
             (test-assert
              (handler-case
                  (progn
                    (conversation-append-provider-item conversation call)
                    nil)
                (conversation-invariant-error ()
                  t))
              "the durable provider-item boundary rejects malformed arguments")
             (let* ((pathname (conversation-pathname conversation))
                    (reloaded
                      (conversation-load-by-id
                       configuration "legacy-malformed-tool-arguments"))
                    (replayed-call
                      (find-if #'function-call-item-p
                               (conversation-input-items reloaded)))
                    (provider
                      (make-instance
                       'scripted-provider
                       :results
                       (list
                        (agent-test-result
                         "legacy-recovered"
                         (list (agent-test-message "legacy history recovered"))))))
                    (agent
                      (agent-create :configuration configuration
                                    :provider      provider
                                    :conversation  reloaded
                                    :tool-registry (agent-test-registry)
                                    :worker        ':unused)))
               (test-assert
                (string= (json-get replayed-call "arguments") "{}")
                "loading legacy history repairs malformed call arguments in memory")
               (let* ((records (conversation--read-records pathname))
                      (provider-record
                        (find ':provider-item records :key #'first))
                      (persisted-call
                        (json-decode (getf (rest provider-record) :wire-json))))
                 (test-assert
                  (string= (json-get persisted-call "arguments") malformed)
                  "legacy repair leaves the append-only record unchanged"))
               (agent-run-user-turn agent "continue recovered history")
               (let* ((snapshot
                        (first (scripted-provider-input-snapshots provider)))
                      (provider-call (find-if #'function-call-item-p snapshot)))
                 (test-assert
                  (string= (json-get provider-call "arguments") "{}")
                  "recovered legacy history is replayable on the next request")))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-tool-storm-guard () null)
(defun test-agent-tool-storm-guard ()
  "Test canonical repetition and oscillation guards while read-only calls remain exempt."
  (let* ((*agent-tool-storm-identical-call-limit* 3)
         (configuration                           (test-configuration))
         (root                                    (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let ((left
                   (agent-test-call
                    :arguments
                    "{\"second\":1.0,\"first\":{\"b\":2,\"a\":1}}"))
                 (right
                   (agent-test-call
                    :arguments
                    "{\"first\":{\"a\":1.0,\"b\":2},\"second\":1}")))
             (test-assert
              (equalp (agent--tool-call-signature left)
                      (agent--tool-call-signature right))
              "storm signatures canonicalize object order and numeric representation"))
           (flet ((tool-result-records (conversation)
                    (remove-if-not
                     (lambda (record)
                       (and (listp record)
                            (eq (first record) ':tool-result)))
                     (conversation--read-records
                      (conversation-pathname conversation)))))
             (let* ((conversation
                      (conversation-create
                       configuration :identifier "tool-storm-repetition"))
                    (provider
                      (make-instance
                       'scripted-provider
                       :results
                       (list
                        (agent-test-result
                         "repeat-1"
                         (list (agent-test-call
                                :call-id   "repeat-1"
                                :arguments "{\"value\":\"same\"}")))
                        (agent-test-result
                         "repeat-2"
                         (list (agent-test-call
                                :call-id   "repeat-2"
                                :arguments "{\"value\":\"same\"}")))
                        (agent-test-result
                         "repeat-3"
                         (list (agent-test-call
                                :call-id   "repeat-3"
                                :arguments "{\"value\":\"same\"}")))
                        (agent-test-result
                         "repeat-4"
                         (list (agent-test-call
                                :call-id   "repeat-4"
                                :arguments "{\"value\":\"same\"}")))
                        (agent-test-result
                         "repeat-done"
                         (list (agent-test-message "repetition handled"))))))
                    (agent
                      (agent-create
                       :configuration configuration
                       :provider      provider
                       :conversation  conversation
                       :tool-registry (agent-test-registry)
                       :worker        ':unused)))
               (agent-run-user-turn agent "repeat a mutating call")
               (let ((outputs (agent-test-tool-outputs conversation))
                     (records (tool-result-records conversation)))
                 (test-assert
                  (equal (subseq outputs 0 2)
                         '("echo: same" "echo: same"))
                  "the first two identical mutating calls execute")
                 (test-assert
                  (and (= (length outputs) 4)
                       (search "withheld repeated call" (third outputs)))
                  "the third identical mutating call is withheld")
                 (test-assert
                  (search "second consecutive withheld call" (fourth outputs))
                  "consecutive withholds escalate their model-visible diagnosis")
                 (test-assert
                  (equal (mapcar (lambda (record)
                                   (getf (rest record) :category))
                                 records)
                         '(:success :success :mechanics :mechanics))
                  "withheld calls persist as mechanics rather than failures")))
             (let* ((conversation
                      (conversation-create
                       configuration :identifier "tool-storm-oscillation"))
                    (arguments
                      '("{\"value\":\"a\"}"
                        "{\"value\":\"b\"}"
                        "{\"value\":\"a\"}"
                        "{\"value\":\"b\"}"))
                    (provider
                      (make-instance
                       'scripted-provider
                       :results
                       (append
                        (loop for source in arguments
                              for index from 1
                              collect
                              (agent-test-result
                               (format nil "oscillate-~D" index)
                               (list (agent-test-call
                                      :call-id   (format nil "oscillate-~D" index)
                                      :arguments source))))
                        (list
                         (agent-test-result
                          "oscillate-done"
                          (list (agent-test-message "oscillation handled")))))))
                    (agent
                      (agent-create
                       :configuration configuration
                       :provider      provider
                       :conversation  conversation
                       :tool-registry (agent-test-registry)
                       :worker        ':unused)))
               (agent-run-user-turn agent "oscillate between mutating calls")
               (let ((outputs (agent-test-tool-outputs conversation)))
                 (test-assert
                  (and (= (length outputs) 4)
                       (search "A-B-A-B oscillation" (fourth outputs)))
                  "the fourth alternating mutating call is withheld")))
             (let* ((conversation
                      (conversation-create
                       configuration :identifier "tool-storm-read-only"))
                    (provider
                      (make-instance
                       'scripted-provider
                       :results
                       (append
                        (loop for index from 1 to 4
                              collect
                              (agent-test-result
                               (format nil "read-only-~D" index)
                               (list (agent-test-call
                                      :call-id   (format nil "read-only-~D" index)
                                      :name      "inspect"
                                      :arguments "{\"value\":\"same\"}"))))
                        (list
                         (agent-test-result
                          "read-only-done"
                          (list (agent-test-message "inspection complete")))))))
                    (agent
                      (agent-create
                       :configuration configuration
                       :provider      provider
                       :conversation  conversation
                       :tool-registry (agent-test-read-only-registry)
                       :worker        ':unused)))
               (agent-run-user-turn agent "repeat a read-only call")
               (test-assert
                (equal (agent-test-tool-outputs conversation)
                       '("echo: same" "echo: same" "echo: same" "echo: same"))
                "repeated read-only calls remain executable"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-tool-storm-guard-defaults () null)
(defun test-agent-tool-storm-guard-defaults ()
  "Test the repetition guard fires under the unmodified production limits."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (limit         *agent-tool-storm-identical-call-limit*))
    (unwind-protect
         (let* ((conversation
                  (conversation-create
                   configuration :identifier "tool-storm-defaults"))
                (provider
                  (make-instance
                   'scripted-provider
                   :results
                   (append
                    (loop for index from 1 to limit
                          collect
                          (agent-test-result
                           (format nil "default-repeat-~D" index)
                           (list (agent-test-call
                                  :call-id   (format nil "default-repeat-~D" index)
                                  :arguments "{\"value\":\"same\"}"))))
                    (list
                     (agent-test-result
                      "default-repeat-done"
                      (list (agent-test-message "repetition handled")))))))
                (agent
                  (agent-create
                   :configuration configuration
                   :provider      provider
                   :conversation  conversation
                   :tool-registry (agent-test-registry)
                   :worker        ':unused)))
           (test-assert
            (> limit *agent-tool-storm-signature-window-size*)
            "the production repetition limit exceeds the oscillation window")
           (agent-run-user-turn agent "repeat a mutating call many times")
           (let ((outputs (agent-test-tool-outputs conversation)))
             (test-assert
              (and (= (length outputs) limit)
                   (every (lambda (output) (string= output "echo: same"))
                          (subseq outputs 0 (1- limit))))
              "every identical call below the production limit executes")
             (test-assert
              (search "withheld repeated call" (first (last outputs)))
              "the call reaching the production limit is withheld")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-tool-retry-guidance () null)
(defun test-agent-tool-retry-guidance ()
  "Test missing-argument skeletons and inherited diagnoses for changed failures."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let* ((conversation
                    (conversation-create
                     configuration :identifier "tool-missing-argument-skeleton"))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list
                      (agent-test-result
                       "missing-argument"
                       (list (agent-test-call
                              :call-id   "missing-argument"
                              :arguments "{}")))
                      (agent-test-result
                       "complete-argument"
                       (list (agent-test-call
                              :call-id   "complete-argument"
                              :arguments "{\"value\":\"fixed\"}")))
                      (agent-test-result
                       "argument-done"
                       (list (agent-test-message "argument repaired"))))))
                  (agent
                    (agent-create
                     :configuration configuration
                     :provider      provider
                     :conversation  conversation
                     :tool-registry (agent-test-registry)
                     :worker        ':unused)))
             (agent-run-user-turn agent "repair missing arguments")
             (let* ((outputs (agent-test-tool-outputs conversation))
                    (records
                      (remove-if-not
                       (lambda (record)
                         (and (listp record)
                              (eq (first record) ':tool-result)))
                       (conversation--read-records
                        (conversation-pathname conversation)))))
               (test-assert
                (and (= (length outputs) 2)
                     (search "required arguments are missing: value"
                             (first outputs))
                     (search "Call skeleton:" (first outputs))
                     (search "\"value\":\"\"" (first outputs)))
                "a missing required argument returns a concrete retry skeleton")
               (test-assert
                (string= (second outputs) "echo: fixed")
                "the corrected call executes on the following provider round")
               (test-assert
                (eq (getf (rest (first records)) :category) ':mechanics)
                "missing required arguments are mechanics rather than failures")))
           (let* ((conversation
                    (conversation-create
                     configuration :identifier "tool-retry-diagnosis"))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list
                      (agent-test-result
                       "failure-1"
                       (list (agent-test-call
                              :call-id   "failure-1"
                              :name      "changing-failure"
                              :arguments "{\"value\":\"same\"}")))
                      (agent-test-result
                       "failure-2"
                       (list (agent-test-call
                              :call-id   "failure-2"
                              :name      "changing-failure"
                              :arguments "{\"value\":\"same\"}")))
                      (agent-test-result
                       "failure-done"
                       (list (agent-test-message "failures diagnosed"))))))
                  (agent
                    (agent-create
                     :configuration configuration
                     :provider      provider
                     :conversation  conversation
                     :tool-registry (agent-test-changing-failure-registry)
                     :worker        ':unused)))
             (agent-run-user-turn agent "retry the same failed call")
             (let ((outputs (agent-test-tool-outputs conversation)))
               (test-assert
                (string= (first outputs) "changing failure 1")
                "the initial failure is returned without inherited diagnosis")
               (test-assert
                (and (search "changing failure 2" (second outputs))
                     (search "Previous failure for this exact call: changing failure 1"
                             (second outputs)))
                "a changed exact-call failure quotes the previous diagnosis"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-tool-failures () null)
(defun test-agent-tool-failures ()
  "Test successful and failed calls retain independent correlation."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "mixed-tools"))
                (provider
                  (make-instance
                   'scripted-provider
                   :results
                   (list
                    (agent-test-result
                     "tool-batch"
                     (list
                      (agent-test-call :call-id   "good"
                                       :arguments "{\"value\":\"ok\"}")
                      (agent-test-call :call-id   "bad"
                                       :namespace "missing"
                                       :name      "tool")))
                    (agent-test-result "tool-final"
                                       (list (agent-test-message "finished"))))))
                (agent
                  (agent-create :configuration configuration
                                :provider      provider
                                :conversation  conversation
                                :tool-registry (agent-test-registry)
                                :worker        ':unused)))
           (agent-run-user-turn agent "run mixed tools")
           (test-assert (= (length (conversation-input-items conversation)) 6)
                        "multiple calls each receive one correlated output")
           (let* ((records
                    (conversation--read-records
                     (conversation-pathname conversation)))
                  (tool-results
                    (remove-if-not
                     (lambda (record)
                       (and (listp record) (eq (first record) :tool-result)))
                     records)))
             (test-assert
              (equal (mapcar (lambda (record) (getf (rest record) :status))
                             tool-results)
                     '(:ok :error))
              "successful and failed calls remain explicitly distinguished")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-provider-failure-persistence () null)
(defun test-agent-provider-failure-persistence ()
  "Test a terminal provider failure is durable but absent from model replay."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "provider-failure"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (make-condition
              'provider-error
              :message     "The provider rejected the prompt."
              :status      400
              :code        "invalid_prompt"
              :request-id  "request-invalid"
              :response-id "response-invalid"
              :response    nil))))
         (agent
           (agent-create :configuration configuration
                         :provider      provider
                         :conversation  conversation
                         :tool-registry (agent-test-registry)
                         :worker        ':unused)))
    (unwind-protect
         (progn
           (test-assert
            (handler-case
                (progn
                  (agent-run-user-turn agent "persist this failure")
                  nil)
              (provider-error ()
                t))
            "terminal provider failures still reach the caller")
           (let* ((records
                    (conversation--read-records
                     (conversation-pathname conversation)))
                  (provider-record
                    (find-if (lambda (record)
                               (eq (first record) ':provider))
                             records))
                  (metadata (getf (rest provider-record) :metadata))
                  (failure  (getf metadata :failure)))
             (test-assert (= (getf metadata :request-number) 1)
                          "provider failure metadata retains its request number")
             (test-assert (string= (getf failure :code) "invalid_prompt")
                          "provider failure metadata retains its error code")
             (test-assert (null (getf failure :incomplete-reason))
                          "ordinary provider failures have no incomplete reason")
             (test-assert
              (string= (getf failure :request-id) "request-invalid")
              "provider failure metadata retains its request identifier")
             (test-assert
              (string= (getf failure :response-id) "response-invalid")
              "provider failure metadata retains its response identifier")
             (test-assert (null (getf failure :retryable-p))
                          "terminal failure metadata is not marked retryable"))
           (let ((reloaded
                   (conversation-load-by-id configuration "provider-failure")))
             (test-assert (= (length (conversation-input-items reloaded)) 1)
                          "provider failure metadata stays outside model replay")
             (test-assert
              (string= (json-get (first (conversation-input-items reloaded))
                                 "role")
                       "user")
              "replayed input retains the user message that failed")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-incomplete-provider-failure-persistence () null)
(defun test-agent-incomplete-provider-failure-persistence ()
  "Test incomplete provider reasons survive durable failure recording."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create
            configuration
            :identifier "provider-incomplete-failure"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (make-condition
              'provider-incomplete-response
              :message
              "The provider returned an incomplete response (max_output_tokens)."
              :reason      "max_output_tokens"
              :status      nil
              :code        "response_incomplete"
              :request-id  "request-incomplete"
              :response-id "response-incomplete"
              :response    nil))))
         (agent
           (agent-create :configuration configuration
                         :provider      provider
                         :conversation  conversation
                         :tool-registry (agent-test-registry)
                         :worker        ':unused)))
    (unwind-protect
         (progn
           (test-assert
            (handler-case
                (progn
                  (agent-run-user-turn agent "persist this incomplete failure")
                  nil)
              (provider-incomplete-response ()
                t))
            "incomplete provider failures still reach the caller")
           (let* ((records
                    (conversation--read-records
                     (conversation-pathname conversation)))
                  (provider-record
                    (find-if (lambda (record)
                               (eq (first record) ':provider))
                             records))
                  (metadata (getf (rest provider-record) :metadata))
                  (failure  (getf metadata :failure)))
             (test-assert
              (string= (getf failure :code) "response_incomplete")
              "incomplete failure metadata retains its stable error code")
             (test-assert
              (string= (getf failure :incomplete-reason) "max_output_tokens")
              "incomplete failure metadata retains its structured reason")
             (test-assert (null (getf failure :retryable-p))
                          "incomplete failure metadata is terminal")
             (test-assert
              (string= (getf failure :request-id) "request-incomplete")
              "incomplete failure metadata retains its request identifier")
             (test-assert
              (string= (getf failure :response-id) "response-incomplete")
              "incomplete failure metadata retains its response identifier")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-provider-credential-failure-containment () null)
(defun test-agent-provider-credential-failure-containment ()
  "Test provider credential echoes cannot reach durable failure metadata."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create
            configuration
            :identifier "provider-secret-failure"))
         (provider      (provider-create configuration))
         (credentials   (provider-tests--credentials configuration))
         (secrets       (oauth-credentials-secret-values credentials))
         (source
           (test-sse-event-string
            (json-object
             "type" "response.failed"
             "response"
             (json-object
              "id" (oauth-credentials-access-token credentials)
              "error"
              (json-object
               "code" "invalid_prompt"
               "message"
               (format
                nil
                "~A/~A"
                (oauth-credentials-refresh-token credentials)
                (oauth-credentials-id-token credentials))
               "request_id"
               (oauth-credentials-account-id credentials))))))
         (agent
           (agent-create
            :configuration configuration
            :provider      provider
            :conversation  conversation
            :tool-registry (agent-test-registry)
            :worker        ':unused)))
    (unwind-protect
         (progn
           (credential-source-save
            (credential-manager-primary-source
             (provider-credential-manager provider))
            credentials)
           (test-assert
            (handler-case
                (test-call-with-function-replacements
                 (list
                  (list
                   'provider-open-response-stream
                   (lambda (active-provider request &rest arguments)
                     (declare
                      (ignore active-provider request arguments))
                     (values
                      (make-instance
                       'test-character-input-stream
                       :source source)
                      200
                      nil))))
                 (lambda ()
                   (agent-run-user-turn
                    agent
                    "persist a provider credential echo")))
              (provider-error ()
                t))
            "a credential-echoing provider failure reaches the caller")
           (let ((records nil))
             (conversation-map-records
              conversation
              (lambda (record)
                (push record records)))
             (setf records (nreverse records))
             (let* ((text
                      (with-output-to-string (stream)
                        (dolist (segment
                                 (conversation-storage-pathnames
                                  (conversation-pathname conversation)))
                          (write-string (uiop:read-file-string segment) stream))))
                    (provider-record
                      (find-if
                       (lambda (record)
                         (eq (first record) ':provider))
                       records))
                    (failure
                      (getf
                       (getf (rest provider-record) :metadata)
                       :failure)))
               (provider-tests--assert-credential-free
                (list records text)
                secrets
                "durable provider failure state contains no credential")
               (test-assert
                (and
                 (string= (getf failure :code) "invalid_prompt")
                 (test-object-contains-string-p
                  failure
                  *provider-credential-redaction-marker*)
                 (search *provider-credential-redaction-marker* text))
                "durable provider failure metadata retains sanitized diagnostics"))))
      (platform-delete-directory-tree
       *platform*
       root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-long-tool-turn () null)
(defun test-agent-long-tool-turn ()
  "Test a useful turn may execute more than eight tool batches before completion."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation  (conversation-create configuration :identifier "long-turn"))
         (tool-results
           (loop for index from 1 to 12
                 collect (agent-test-result
                          (format nil "tool-~D" index)
                          (list (agent-test-call
                                 :call-id   (format nil "call-~D" index)
                                 :arguments (format nil "{\"value\":\"~D\"}" index))))))
         (provider
           (make-instance
            'scripted-provider
            :results (append tool-results
                             (list (agent-test-result
                                    "long-final"
                                    (list (agent-test-message "done")))))))
         (agent
           (agent-create :configuration configuration
                         :provider      provider
                         :conversation  conversation
                         :tool-registry (agent-test-registry)
                         :worker        ':unused)))
    (unwind-protect
         (progn
           (test-assert
            (string= (provider-result-response-id
                      (agent-run-user-turn agent "perform a long task"))
                     "long-final")
            "a twelve-batch turn completes without a fixed eight-round cutoff")
           (test-assert
            (= (count-if (lambda (record)
                           (eq (first record) :tool-result))
                         (conversation--read-records
                          (conversation-pathname conversation)))
               12)
            "every long-turn tool call receives a durable correlated output"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-unbounded-tool-calls () null)
(defun test-agent-unbounded-tool-calls ()
  "Test one turn may exceed the former cumulative tool-call ceiling."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "unbounded-tools"))
         (tool-results
           (loop for index from 1 to 257
                 collect
                 (agent-test-result
                  (format nil "tool-~D" index)
                  (list
                   (agent-test-call
                    :call-id   (format nil "call-~D" index)
                    :arguments (format nil "{\"value\":\"~D\"}" index))))))
         (provider
           (make-instance
            'scripted-provider
            :results
            (append tool-results
                    (list
                     (agent-test-result
                      "large-tool-final"
                      (list (agent-test-message "done")))))))
         (agent
           (agent-create :configuration configuration
                         :provider      provider
                         :conversation  conversation
                         :tool-registry (agent-test-registry)
                         :worker        ':unused)))
    (unwind-protect
         (progn
           (test-assert
            (string= (provider-result-response-id
                      (agent-run-user-turn agent "run every requested tool"))
                     "large-tool-final")
            "a turn exceeding 256 calls reaches its normal completion")
           (test-assert
            (= (count-if (lambda (record)
                           (eq (first record) :tool-result))
                         (conversation--read-records
                          (conversation-pathname conversation)))
               257)
            "every call above the former ceiling receives a durable result"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-default-turn-has-no-step-guillotine () null)
(defun test-agent-default-turn-has-no-step-guillotine ()
  "Test the default turn may keep working beyond the former provider-step limit."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "unbounded-turn"))
         (continuations
           (loop for index from 1 below 64
                 collect
                 (agent-test-result
                  (format nil "continue-~D" index)
                  (list (agent-test-message "Still working."))
                  :turn-completion ':continue)))
         (provider
           (make-instance
            'scripted-provider
            :results
            (append continuations
                    (list
                     (agent-test-result
                      "step-64-tool"
                      (list (agent-test-call :call-id   "late-tool"
                                             :arguments "{\"value\":\"late\"}")))
                     (agent-test-result
                      "step-65-final"
                      (list (agent-test-message "Done."))
                      :turn-completion ':end)))))
         (agent
           (agent-create :configuration configuration
                         :provider      provider
                         :conversation  conversation
                         :tool-registry (agent-test-registry)
                         :worker        ':unused)))
    (unwind-protect
         (progn
           (test-assert
            (string= (provider-result-response-id
                      (agent-run-user-turn agent "finish a long task"))
                     "step-65-final")
            "the default turn continues past provider step 64")
           (test-assert
            (every #'plusp
                   (scripted-provider-tool-schema-counts provider))
            "tools stay available throughout the default turn")
           (test-assert
            (find "late-tool"
                  (conversation--read-records
                   (conversation-pathname conversation))
                  :key  (lambda (record)
                          (and (listp record)
                               (eq (first record) :tool-result)
                               (getf (rest record) :call-id)))
                  :test #'string=)
            "the tool requested on provider step 64 executes normally"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-skill-provider-barrier () null)
(defun test-agent-skill-provider-barrier ()
  "Test skill.load correlation, persistence, and same-result action blocking."
  (let* ((base-configuration (test-configuration))
         (root               (test-configuration-root base-configuration))
         (project            (merge-pathnames "project/" root))
         (skill-root         (merge-pathnames ".autolith/skills/" project))
         (configuration
           (progn
             (ensure-directories-exist
              (merge-pathnames ".git/marker" project))
             (configuration-copy
              base-configuration :working-directory
              project)))
         (conversation
           (conversation-create configuration
                                :identifier "agent-skill-barrier"))
         (before-call
           (agent-test-call
            :call-id   "before-call"
            :arguments "{\"value\":\"before\"}"))
         (skill-call
           (agent-test-call
            :call-id   "skill-call"
            :namespace "skill"
            :name      "load"
            :arguments "{\"name\":\"alpha\"}"))
         (after-call
           (agent-test-call
            :call-id   "after-call"
            :arguments "{\"value\":\"after\"}"))
         (provider
           (make-instance
            'scripted-provider
            :configuration configuration
            :results
            (list
             (agent-test-result
              "skill-barrier-1"
              (list before-call skill-call after-call))
             (agent-test-result
              "skill-barrier-2"
              (list (agent-test-message "Applied selected instructions."))
              :turn-completion ':end))))
         (registry
           (skill-augment-tool-registry (agent-test-registry)))
         (terminal           (make-instance 'recording-terminal :columns 80))
         (ui                 (terminal-ui-create :terminal terminal))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation  conversation
                          :tool-registry registry
                          :ui            ui))
         (observer
           (application-agent-observer
            application
            :user-message-input
            (user-message-input-create
             :text "Select the relevant Skill, then continue."))))
    (unwind-protect
         (progn
           (terminal-ui-start ui)
           (recording-terminal-reset terminal)
           (skill-tests--write
            skill-root
            "alpha/SKILL.sexp"
            (skill-tests--definition
             "alpha"
             "Apply the barrier test instructions."
             "BARRIER-SKILL-INSTRUCTIONS"))
           (agent-run-user-turn
            (agent-create :configuration configuration
                          :provider      provider
                          :conversation  conversation
                          :tool-registry registry
                          :worker        nil)
            "Select the relevant Skill, then continue."
            :observer observer)
           (let ((terminal-output (recording-terminal-output terminal)))
             (test-assert
              (and (search "◆ loaded skill: alpha" terminal-output)
                   (null (search "✓ skill.load" terminal-output)))
              "the real Skill result path emits one compact transcript marker"))
           (let* ((snapshots
                    (reverse
                     (scripted-provider-input-snapshots provider)))
                  (second-request (second snapshots))
                  (outputs
                    (loop for item in second-request
                          when (string=
                                (or (json-get item "type") "")
                                "function_call_output")
                            collect item))
                  (records
                    (conversation--read-records
                     (conversation-pathname conversation)))
                  (record-source
                    (with-output-to-string (stream)
                      (prin1 records stream))))
             (test-assert
              (equal
               (reverse
                (scripted-provider-skill-selection-snapshots provider))
               '(nil ("alpha")))
              "skill.load selection remains active at the required provider boundary")
             (test-assert
              (equal
               (second
                (reverse
                 (scripted-provider-skill-contribution-snapshots provider)))
               '("skill-catalog" "skill-selected-alpha"))
              "the provider retry receives the selected instructions before more actions")
             (test-assert
              (and (= (length second-request) 7)
                   (equal
                    (mapcar
                     (lambda (item)
                       (json-get item "call_id"))
                     (rest second-request))
                    '("before-call"
                      "skill-call"
                      "after-call"
                      "before-call"
                      "skill-call"
                      "after-call")))
              "mixed durable and request-local calls preserve provider wire order")
             (test-assert
              (and (= (length outputs) 3)
                   (search "echo: before"
                           (json-get (first outputs) "output"))
                   (search "Selected skill alpha"
                           (json-get (second outputs) "output"))
                   (search "preceding tool requires a provider round trip"
                           (json-get (third outputs) "output"))
                   (null (search "echo: after"
                                 (json-get (third outputs) "output"))))
              "calls after skill.load are explicitly deferred instead of executed")
             (test-assert
              (and (search "skill-call" record-source)
                   (null (search "after-call" record-source))
                   (null (search "BARRIER-SKILL-INSTRUCTIONS" record-source)))
              "selection metadata persists without instruction bodies or deferred calls")
             (test-assert
              (and (= (length (conversation-input-items conversation)) 6)
                   (null (conversation-ephemeral-input-entries conversation)))
              "a successful response consumes deferred correlation and retains selection metadata")
             (let* ((reloaded
                      (conversation-load-by-id
                       configuration
                       "agent-skill-barrier"))
                    (replay-terminal
                      (make-instance 'recording-terminal :columns 80))
                    (replay-ui
                      (terminal-ui-create :terminal replay-terminal))
                    (replay-application
                      (make-instance 'application
                                     :configuration configuration
                                     :conversation  reloaded
                                     :tool-registry registry
                                     :ui            replay-ui)))
               (test-assert
                (and (= (length (conversation-input-items reloaded)) 6)
                     (equal
                      (loop for item in (conversation-input-items reloaded)
                            for call-id = (json-get item "call_id")
                            when call-id collect call-id)
                      '("before-call" "skill-call" "before-call" "skill-call"))
                     (string=
                      (json-encode (coerce (conversation-input-items reloaded) 'vector))
                      (json-encode (coerce (conversation-input-items conversation) 'vector))))
                "replay restores complete durable call pairs in their original order")
               (unwind-protect
                    (progn
                      (terminal-ui-start replay-ui)
                      (recording-terminal-reset replay-terminal)
                      (application-render-records replay-application)
                      (let ((replay-output
                              (recording-terminal-output replay-terminal)))
                        (test-assert
                         (and (null (search "◆ loaded skill" replay-output))
                              (null (search "BARRIER-SKILL-INSTRUCTIONS"
                                            replay-output)))
                         "conversation reload does not replay Skill presentation or instructions")))
                 (ignore-errors (terminal-ui-stop replay-ui))))))
      (ignore-errors (terminal-ui-stop ui))
      (platform-delete-directory-tree *platform* root
                                      :validate          t
                                      :if-does-not-exist ':ignore)))
  nil)

(-> agent-tests--request-numbers (conversation) list)
(defun agent-tests--request-numbers (conversation)
  "Return CONVERSATION's persisted provider request numbers with their compaction kinds."
  (let ((numbers nil))
    (conversation-map-records
     conversation
     (lambda (record)
       (let ((metadata (and (eq (first record) ':provider)
                            (getf (rest record) :metadata))))
         (when (and metadata (getf metadata :request-number))
           (push (list (getf metadata :request-number)
                       (getf metadata :compaction))
                 numbers)))))
    (nreverse numbers)))

(-> agent-tests--compaction-agent (configuration conversation list) agent)
(defun agent-tests--compaction-agent (configuration conversation results)
  "Return an agent for CONVERSATION whose scripted provider returns RESULTS in order."
  (agent-create :configuration configuration
                :provider      (make-instance 'scripted-provider
                                              :configuration configuration
                                              :results       results)
                :conversation  conversation
                :tool-registry (agent-test-registry)
                :worker        nil))

(-> test-agent-compaction-rejected-submission () null)
(defun test-agent-compaction-rejected-submission ()
  "Test a rejected submission earns one follow-up naming its problem and stays out of durable history."
  (loop for (rejected problem) in
        (list (list (agent-test-call :call-id "call-wait" :namespace "job" :name "wait")
                    "exactly one compaction.submit call")
              (list (agent-test-checkpoint-call
                     "{\"intent\":\"Go on.\",\"notes\":[],\"next\":\"\",\"next_refs\":[\"missing\"]}")
                    "does not list missing")
              (list (agent-test-message "Here is a summary in prose.")
                    "exactly one compaction.submit call"))
        do (with-test-configuration (configuration)
             (let* ((conversation (conversation-create configuration))
                    (agent        (agent-tests--compaction-agent
                                   configuration conversation
                                   (list (agent-test-result "rejected" (list rejected))
                                         (agent-test-result "accepted"
                                                            (list (agent-test-checkpoint-call))
                                                            :turn-completion ':end)))))
               (conversation-append-user-message conversation "earlier context")
               (test-assert (= 2 (agent-compact-conversation agent (make-instance 'agent-observer)))
                            "a rejected submission costs one follow-up")
               (let ((correction (first (last (second (reverse (scripted-provider-input-snapshots
                                                                (agent-provider agent))))))))
                 (test-assert (and (if (function-call-item-p rejected)
                                       (equal (json-get rejected "call_id")
                                              (json-get correction "call_id"))
                                       (json-string= (json-get correction "role") "user"))
                                   (search problem (or (json-get correction "output")
                                                       (context--message-text correction))))
                              "the follow-up answers the rejected response and names its problem"))
               (let ((kinds (mapcar #'first (conversation--read-records
                                             (conversation-pathname conversation)))))
                 (test-assert (and (= 1 (count :summary kinds))
                                   (notany (lambda (kind) (member kind '(:provider-item :tool-result)))
                                           kinds))
                              "a rejected submission never enters durable history"))
               (test-assert (equal '((1 :summary) (2 :summary))
                                   (agent-tests--request-numbers conversation))
                            "both requests are accounted"))))
  nil)

(-> test-agent-compaction-failed-generation () null)
(defun test-agent-compaction-failed-generation ()
  "Test a generation that fails after it began costs one request and is asked again."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (agent        (agent-tests--compaction-agent
                          configuration conversation
                          (list (make-condition 'provider-stream-abandoned
                                                :message     "The stream ended early."
                                                :status      nil :code     nil
                                                :request-id  nil :response nil
                                                :response-id nil :attempts 1)
                                (agent-test-result "accepted" (list (agent-test-checkpoint-call))
                                                   :turn-completion ':end)))))
      (conversation-append-user-message conversation "earlier context")
      (test-assert (= 2 (agent-compact-conversation agent (make-instance 'agent-observer)))
                   "a failed generation is counted and the checkpoint is requested again")
      (test-assert (and (conversation-continuation conversation)
                        (equal '((1 :summary) (2 :summary))
                               (agent-tests--request-numbers conversation)))
                   "the retry publishes its checkpoint beside the record of the failure")))
  nil)

(-> test-agent-compaction-without-submission () null)
(defun test-agent-compaction-without-submission ()
  "Test compaction without a usable submission keeps the earlier notes and marks the gap, while a refusal publishes nothing."
  (with-test-configuration (configuration)
    (let* ((*conversation-compaction-tail-characters* 0)
           (conversation                              (conversation-create configuration))
           (observer                                  (make-instance 'agent-observer))
           (agent
             (agent-tests--compaction-agent
              configuration conversation
              (append (list (agent-test-result "first" (list (agent-test-checkpoint-call))
                                               :turn-completion ':end))
                      (loop repeat *agent-compaction-maximum-generations*
                            collect (agent-test-result "empty" nil :turn-completion ':end))
                      (list (agent-test-result "refusal" (list (agent-test-refusal)))))))
           (sample
             (list :format 1 :soft-header (provider-context--soft-header (agent-provider agent) #())
                   :history-count 0 :history-digest (provider-cache-prefix--entry #())
                   :wire-fields nil :wire-count 0 :wire-digest (provider-cache-prefix--entry #())
                   :tokens 1200)))
      (conversation-append-user-message conversation "first request")
      (let ((carried (1- (conversation-next-sequence conversation))))
        (agent-compact-conversation agent observer)
        (conversation-append-user-message conversation "second request")
        (test-assert (= *agent-compaction-maximum-generations*
                        (agent-compact-conversation agent observer))
                     "every generation is spent before compaction stops asking")
        (let ((continuation (conversation-continuation conversation)))
          (test-assert (and (equal "Resume the pending request." (getf continuation :intent))
                            (equal (list (list (conversation-identifier conversation)
                                               carried carried))
                                   (getf continuation :unsummarized)))
                       "the fallback checkpoint keeps the earlier notes and names the record nobody summarized, which the first checkpoint carried forward"))
        (test-assert (search "second request" (json-encode (conversation-input-items conversation)))
                     "the fallback checkpoint retains the recent tail verbatim"))
      (conversation-append-record conversation (list :context-usage :sample sample))
      (setf (conversation-context-usage-sample conversation) sample)
      (let ((records (length (conversation--read-records (conversation-pathname conversation)))))
        (test-assert (handler-case
                         (progn (agent-compact-conversation agent observer) nil)
                       (provider-protocol-error ()
                         t))
                     "a refusal ends compaction with a provider protocol error")
        (test-assert (equal '(:provider)
                            (mapcar #'first
                                    (nthcdr records (conversation--read-records
                                                     (conversation-pathname conversation)))))
                     "a refused request records its usage and publishes no checkpoint"))
      (dolist (current (list conversation (conversation-load (conversation-pathname conversation))))
        (test-assert (equal sample (conversation-context-usage-sample current))
                     "failed compaction preserves the measured context anchor"))))
  nil)

(-> test-agent-compaction () null)
(defun test-agent-compaction ()
  "Test threshold-triggered compaction through the scripted provider."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier
                                                   "agent-compaction"))
                (summary-item (agent-test-checkpoint-call))
                (answer-item
                  (json-object
                   "type" "message"
                   "role" "assistant"
                   "content" (json-array
                              (json-object "type" "output_text"
                                           "text" "Done."))))
                (provider
                  (make-instance
                   'prepared-scripted-provider
                   :configuration configuration
                   :results       (list (agent-test-result "compact-1"
                                                           (list summary-item)
                                                           :turn-completion ':end)
                                        (agent-test-result "turn-1"
                                                           (list answer-item)
                                                           :turn-completion ':end))))
                (agent        (agent-create :configuration configuration
                                            :provider      provider
                                            :conversation  conversation
                                            :tool-registry (agent-test-registry)
                                            :worker        nil)))
           (conversation-append-user-message conversation "earlier context")
           (conversation-append-provider-metadata
            conversation
            (list :request-number 1
                  :response-id    "seed"
                  :usage          '(("total_tokens" 999999))))
           (test-call-with-function-replacements
            (list (list 'agent-should-compact-p
                        (lambda (agent)
                          (declare (ignore agent))
                          (null (scripted-provider-compaction-flags provider)))))
            (lambda ()
              (agent-run-user-turn agent "hello")))
           (test-assert (equal (reverse
                                (scripted-provider-compaction-flags provider))
                               '(t nil))
                        "the compaction request precedes the user request")
           (let ((snapshots (reverse (scripted-provider-input-snapshots provider))))
             (test-assert (= 2 (length snapshots))
                          "compaction and user requests both reach the provider")
             (test-assert (some (lambda (items)
                                  (search "hello" (json-encode items)))
                                snapshots)
                          "the user question survives compaction verbatim"))
           (test-assert (find :summary
                              (rest (conversation--read-records
                                     (conversation-pathname conversation)))
                              :key #'first)
                        "compaction persists one durable summary record")
           (test-assert (search "hello"
                                (json-encode (conversation-input-items conversation)))
                        "the live projection retains the verbatim user question")
           (test-assert (search "A previous segment"
                                (json-get
                                 (aref (json-get
                                        (first (conversation-input-items
                                                conversation))
                                        "content")
                                       0)
                                 "text"))
                        "the live projection starts from the summary bridge"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-parallel-tool-wave () null)
(defun test-agent-parallel-tool-wave ()
  "Test independent calls overlap while callbacks and persistence stay ordered."
  (let* ((configuration                 (test-configuration))
         (root                          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-parallel-wave"))
         (state                         (make-instance 'agent-test-concurrency-state))
         (tool                          (agent-test-concurrency-tool state "run"))
         (registry                      (agent-test-concurrency-registry (list tool)))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "parallel-calls"
              (list
               (agent-test-call
                :call-id   "parallel-a"
                :namespace "concurrency"
                :name      "run"
                :arguments
                "{\"label\":\"a\",\"delay\":0.05,\"await_peer\":true}")
               (agent-test-call
                :call-id   "parallel-b"
                :namespace "concurrency"
                :name      "run"
                :arguments
                "{\"label\":\"b\",\"await_peer\":true}")))
             (agent-test-result
              "parallel-done"
              (list (agent-test-message "done"))))))
         (callback-lock                 (make-lock "Autolith observer callback test"))
         (callback-active-count         0)
         (callback-maximum-active-count 0)
         (observer
           (callback-agent-observer-create
            :status-callback
            (lambda (status details)
              (declare (ignore details))
              (when (eq status ':agent-test-tool-callback)
                (with-lock-held (callback-lock)
                  (incf callback-active-count)
                  (setf callback-maximum-active-count
                        (max callback-maximum-active-count
                             callback-active-count)))
                (sleep 0.02)
                (with-lock-held (callback-lock)
                  (decf callback-active-count)))))))
    (unwind-protect
         (let ((agent
                 (agent-create
                  :configuration configuration
                  :provider      provider
                  :conversation  conversation
                  :tool-registry registry
                  :worker        ':unused)))
           (agent-run-user-turn agent "run both" :observer observer)
           (test-assert
            (agent-test-concurrency-state-overlap-observed-p state)
            "independent tool bodies overlap")
           (test-assert
            (= (agent-test-concurrency-state-maximum-active-count state) 2)
            "one provider batch uses two concurrent tool workers")
           (let ((logical-turn-states
                   (agent-test-concurrency-state-logical-turn-states state)))
             (test-assert
              (and (= (length logical-turn-states) 2)
                   (first logical-turn-states)
                   (every (lambda (turn-state)
                            (eq turn-state (first logical-turn-states)))
                          (rest logical-turn-states)))
              "parallel tool workers share the active logical-turn state"))
           (test-assert
            (= callback-maximum-active-count 1)
            "observer callbacks remain serialized across tool workers")
           (test-assert
            (equal (agent-test-tool-outputs conversation)
                   '("completed: a" "completed: b"))
            "tool outputs persist in provider wire order")
           (let ((events
                   (reverse (agent-test-concurrency-state-events state))))
             (test-assert
              (and (every (lambda (event)
                            (eq (first event) ':start))
                          (subseq events 0 2))
                   (equal (subseq events 2)
                          '((:finish "b") (:finish "a"))))
              "both bodies start before reverse completion finishes")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-tool-wave-cancellation () null)
(defun test-agent-tool-wave-cancellation ()
  "Cancel an active supervised wave without claiming its remaining calls."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (agent         (agent-create
                         :configuration configuration
                         :provider      (make-instance 'scripted-provider :results nil)
                         :conversation  (conversation-create configuration :identifier "cancel-wave")
                         :tool-registry (agent-test-registry) :worker nil))
         (pool          (make-job-pool :name "wave cancellation test" :maximum-concurrency 1))
         (lock          (make-lock "wave test counters"))
         (started       0)
         (stopped       0)
         (job           nil))
    (unwind-protect
         (test-call-with-function-replacements
          (list (list 'agent--execute-tool-plan
                      (lambda (&rest arguments)
                        (declare (ignore arguments))
                        (unwind-protect
                             (progn
                               (with-lock-held (lock) (incf started))
                               (loop (sleep 0.01)))
                          (with-lock-held (lock) (incf stopped))))))
          (lambda ()
            (setf job
                  (first
                   (job-pool-submit-batch
                    pool
                    (list
                     (list :function
                           (lambda (job)
                             (declare (ignore job))
                             (let ((*agent-maximum-concurrent-tool-workers* 1))
                               (agent--run-tool-wave
                                agent
                                (loop for index below 3
                                      collect (list :call
                                                    (agent-test-call
                                                     :call-id   (format nil "wave-~D" index)
                                                     :namespace "concurrency" :name "run"
                                                     :arguments "{}")))
                                :observer (callback-agent-observer-create)))))))))
            (test-assert
             (task-tests--wait-until
              (lambda () (with-lock-held (lock) (= started 1))) 2)
             (format nil "the first supervised tool body starts: ~S" (job-snapshot job)))
            (job-cancel job :reason ':test-cancel :cascade-p t)
            (multiple-value-bind (snapshot terminal-p) (job-await job :timeout-seconds 3)
              (test-assert (and terminal-p (eq (getf snapshot :state) ':aborted))
                           "wave cancellation unwinds the coordinator promptly"))
            (test-assert (with-lock-held (lock) (and (= started 1) (= stopped 1)))
                         "active execution stops and queued wave calls never start")))
      (job-pool-close pool)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-tool-concurrency-key () null)
(defun test-agent-tool-concurrency-key ()
  "Test calls sharing one runtime identity execute in separate waves."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-runtime-key"))
         (state         (make-instance 'agent-test-concurrency-state))
         (runtime       (list ':shared-runtime))
         (tool          (agent-test-concurrency-tool
                         state "keyed" :concurrency-key runtime))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "keyed-calls"
              (list
               (agent-test-call
                :call-id   "keyed-a"
                :namespace "concurrency"
                :name      "keyed"
                :arguments "{\"label\":\"a\",\"delay\":0.03}")
               (agent-test-call
                :call-id   "keyed-b"
                :namespace "concurrency"
                :name      "keyed"
                :arguments "{\"label\":\"b\",\"delay\":0.03}")))
             (agent-test-result
              "keyed-done"
              (list (agent-test-message "done")))))))
    (unwind-protect
         (let ((agent
                 (agent-create
                  :configuration configuration
                  :provider      provider
                  :conversation  conversation
                  :tool-registry
                  (agent-test-concurrency-registry (list tool))
                  :worker        ':unused)))
           (agent-run-user-turn agent "run keyed calls")
           (test-assert
            (= (agent-test-concurrency-state-maximum-active-count state) 1)
            "calls sharing one runtime key do not overlap")
           (test-assert
            (equal (reverse (agent-test-concurrency-state-events state))
                   '((:start "a") (:finish "a")
                     (:start "b") (:finish "b")))
            "shared-runtime calls preserve provider order"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-exclusive-tool-waves () null)
(defun test-agent-exclusive-tool-waves ()
  "Test an exclusive call divides parallel calls into ordered waves."
  (test-assert
   (eq (tool-execution-policy
        (make-instance 'shell-run-tool
                       :namespace   "shell"
                       :name        "run"
                       :description "Run one command."
                       :parameters  (json-object)))
       ':exclusive)
   "shell commands opt into exclusive ordered waves")
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-exclusive-wave"))
         (state         (make-instance 'agent-test-concurrency-state))
         (before        (agent-test-concurrency-tool state "before"))
         (exclusive
           (agent-test-concurrency-tool
            state "exclusive" :execution-policy ':exclusive))
         (after         (agent-test-concurrency-tool state "after"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "exclusive-calls"
              (list
               (agent-test-call
                :call-id   "before"
                :namespace "concurrency"
                :name      "before"
                :arguments "{\"label\":\"before\",\"delay\":0.02}")
               (agent-test-call
                :call-id   "exclusive"
                :namespace "concurrency"
                :name      "exclusive"
                :arguments "{\"label\":\"exclusive\",\"delay\":0.02}")
               (agent-test-call
                :call-id   "after"
                :namespace "concurrency"
                :name      "after"
                :arguments "{\"label\":\"after\",\"delay\":0.02}")))
             (agent-test-result
              "exclusive-done"
              (list (agent-test-message "done")))))))
    (unwind-protect
         (let ((agent
                 (agent-create
                  :configuration configuration
                  :provider      provider
                  :conversation  conversation
                  :tool-registry
                  (agent-test-concurrency-registry
                   (list before exclusive after))
                  :worker        ':unused)))
           (agent-run-user-turn agent "run exclusive call")
           (test-assert
            (= (agent-test-concurrency-state-maximum-active-count state) 1)
            "exclusive execution prevents overlap across adjacent waves")
           (test-assert
            (equal (reverse (agent-test-concurrency-state-events state))
                   '((:start "before") (:finish "before")
                     (:start "exclusive") (:finish "exclusive")
                     (:start "after") (:finish "after")))
            "the exclusive call divides calls into ordered waves"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-parallel-tool-failure () null)
(defun test-agent-parallel-tool-failure ()
  "Test one failed parallel call does not discard its sibling result."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-parallel-failure"))
         (state         (make-instance 'agent-test-concurrency-state))
         (tool          (agent-test-concurrency-tool state "fail"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "failure-calls"
              (list
               (agent-test-call
                :call-id   "failed"
                :namespace "concurrency"
                :name      "fail"
                :arguments
                "{\"label\":\"failed\",\"await_peer\":true,\"fail\":true}")
               (agent-test-call
                :call-id   "sibling"
                :namespace "concurrency"
                :name      "fail"
                :arguments
                "{\"label\":\"sibling\",\"await_peer\":true}")))
             (agent-test-result
              "failure-done"
              (list (agent-test-message "done")))))))
    (unwind-protect
         (let ((agent
                 (agent-create
                  :configuration configuration
                  :provider      provider
                  :conversation  conversation
                  :tool-registry
                  (agent-test-concurrency-registry (list tool))
                  :worker        ':unused)))
           (agent-run-user-turn agent "run failing calls")
           (let ((outputs (agent-test-tool-outputs conversation)))
             (test-assert
              (= (length outputs) 2)
              "both parallel calls persist outputs after one fails")
             (test-assert
              (search "requested test failure" (first outputs))
              "the failed call persists its failure output")
             (test-assert
              (string= (second outputs) "completed: sibling")
              "the successful sibling result remains available")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-parallel-fatal-propagation () null)
(defun test-agent-parallel-fatal-propagation ()
  "Test propagated tool conditions persist unknown outcomes before propagation."
  (dolist (case '(("rollback" rollback-requested)
                  ("corruption" active-image-corruption)
                  ("job-aborted" job-aborted)))
    (destructuring-bind (fatal expected-type) case
      (let* ((configuration (test-configuration))
             (root          (test-configuration-root configuration))
             (conversation
               (conversation-create
                configuration
                :identifier (format nil "agent-fatal-~A" fatal)))
             (state         (make-instance 'agent-test-concurrency-state))
             (tool          (agent-test-concurrency-tool state "fatal"))
             (provider
               (make-instance
                'scripted-provider
                :results
                (list
                 (agent-test-result
                  "fatal-calls"
                  (list
                   (agent-test-call
                    :call-id   "fatal"
                    :namespace "concurrency"
                    :name      "fatal"
                    :arguments
                    (format nil
                            "{\"label\":\"fatal\",\"await_peer\":true,\"fatal\":\"~A\"}"
                            fatal))
                   (agent-test-call
                    :call-id   "sibling"
                    :namespace "concurrency"
                    :name      "fatal"
                    :arguments
                    "{\"label\":\"sibling\",\"await_peer\":true}")))))))
        (unwind-protect
             (let* ((agent
                      (agent-create
                       :configuration configuration
                       :provider      provider
                       :conversation  conversation
                       :tool-registry
                       (agent-test-concurrency-registry (list tool))
                       :worker        ':unused))
                    (condition
                      (handler-case
                          (progn
                            (agent-run-user-turn agent "run fatal calls")
                            nil)
                        (rollback-requested (failure)
                          failure)
                        (active-image-corruption (failure)
                          failure)
                        (job-aborted (failure)
                          failure))))
               (test-assert
                (typep condition expected-type)
                "the original fatal tool condition reaches the agent caller")
               (test-assert
                (agent-test-concurrency-state-overlap-observed-p state)
                "the fatal call executes concurrently with its sibling")
               (test-assert
                (equal (agent-test-tool-outputs conversation)
                       (list *conversation-interrupted-tool-output*
                             "completed: sibling"))
                "unknown and sibling results persist before fatal propagation"))
          (platform-delete-directory-tree
           *platform*
           root :validate t :if-does-not-exist ':ignore)))))
  nil)

(-> test-agent-shell-authorization-unavailable () null)
(defun test-agent-shell-authorization-unavailable ()
  "Test unavailable shell approval returns a failed tool result and continues the turn."
  (let* ((base-configuration (test-configuration))
         (root               (test-configuration-root base-configuration))
         (configuration
           (configuration-copy base-configuration :working-directory root))
         (marker             (merge-pathnames "unavailable-shell-ran" root))
         (conversation
           (conversation-create configuration :identifier "shell-authorization"))
         (call
           (json-object
            "type" "function_call"
            "call_id" "shell-authorization-call"
            "namespace" "shell"
            "name" "run"
            "arguments"
            (json-encode
             (json-object
              "command"
              (format nil "printf ran > ~A"
                      (uiop:escape-shell-token (namestring marker)))
              "directory" (namestring root)))))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result "shell-authorization-1" (list call))
             (agent-test-result
              "shell-authorization-2"
              (list (agent-test-message "approval failure handled"))))))
         (agent              nil))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (setf agent
                 (agent-create
                  :configuration configuration
                  :provider      provider
                  :conversation  conversation
                  :tool-registry (make-default-tool-registry)
                  :worker        ':unused))
           (let* ((observer
                    (callback-agent-observer-create
                     :command-authorization-callback
                     (lambda (command directory)
                       (error 'command-authorization-unavailable
                              :message
                              "Command approval requires an interactive terminal."
                              :command   command
                              :directory directory))))
                  (result
                    (agent-run-user-turn
                     agent "run a command" :observer observer))
                  (outputs (agent-test-tool-outputs conversation)))
             (test-assert
              (string= (provider-result-response-id result)
                       "shell-authorization-2")
              "the agent performs the provider round after unavailable approval")
             (test-assert
              (and (= (length outputs) 1)
                   (search "interactive terminal" (first outputs)))
              "the next provider round receives the failed shell tool result")
             (test-assert (not (probe-file marker))
                          "unavailable approval never starts the subprocess")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))
   nil))

(-> test-agent-portable-value () null)
(defun test-agent-portable-value ()
  "Test portable provider metadata preserves strings and projects vectors."
  (test-assert
   (equal (agent--portable-value (vector "label" 7))
          '("label" 7))
   "portable provider metadata preserves strings inside vectors")
  nil)

(-> test-agent-tool-search-round () null)
(defun test-agent-tool-search-round ()
  "Test that a client tool search is answered locally and replayed on the follow-up."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "agent-tool-search"))
         (search
           (json-object
            "type" "tool_search_call"
            "call_id" "search-1"
            "status" "completed"
            "execution" "client"
            "arguments" (json-object "query" "echo")))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result "response-1" (list search))
             (agent-test-result "response-2" (list (agent-test-message "done"))))))
         (statuses      nil))
    (unwind-protect
         (let* ((agent
                  (agent-create
                   :configuration configuration
                   :provider      provider
                   :conversation  conversation
                   :tool-registry (agent-test-registry)
                   :worker        ':unused))
                (result
                  (agent-run-user-turn
                   agent "find the echo tool"
                   :observer (callback-agent-observer-create
                              :status-callback
                              (lambda (status details)
                                (declare (ignore details))
                                (push status statuses)))))
                (snapshots (reverse (scripted-provider-input-snapshots provider)))
                (follow-up (second snapshots))
                (output    (find-if (lambda (item)
                                      (and (json-object-p item)
                                           (json-string= (json-get item "type")
                                                         "tool_search_output")))
                                    follow-up))
                (namespace (and output
                                (plusp (length (json-get output "tools")))
                                (aref (json-get output "tools") 0)))
                (child     (and namespace (aref (json-get namespace "tools") 0))))
           (test-assert
            (string= (provider-result-response-id result) "response-2")
            "the search round is followed by one more provider request")
           (test-assert
            (and output
                 (json-string= (json-get output "call_id") "search-1")
                 (json-string= (json-get output "execution") "client")
                 (json-string= (json-get namespace "name") "test")
                 (json-string= (json-get child "name") "echo")
                 (eq (json-get child "defer_loading") t)
                 (json-object-p (json-get child "parameters")))
            "the follow-up request replays the locally answered expansion")
           (test-assert
            (let ((position (position output follow-up)))
              (and position
                   (eq (nth (1- position) follow-up) search)))
            "the expansion directly follows its search call in history")
           (test-assert
            (and (member :provider-follow-up statuses)
                 (not (member :tool-call-completed statuses)))
            "a tool search is neither a tool call nor a completed turn")
           (test-assert
            (let ((records (conversation--read-records
                            (conversation-pathname conversation))))
              (= (count-if (lambda (record)
                             (and (eq (first record) :provider-item)
                                  (search "tool_search_output"
                                          (getf (rest record) :wire-json))))
                           records)
                 1))
            "the expansion is journaled once as a durable provider item"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-tool-wave-retained-cleanup () null)
(defun test-agent-tool-wave-retained-cleanup ()
  "Inspect a terminal task's interrupt-resistant wave until its worker exits."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((agent        (task-tests--primary-agent configuration "retained-wave"))
           (orchestrator (task-orchestrator-create))
           (definition   (task-agent-definition-create
                          :name "held-wave" :description "Held wave test."
                          :instructions "Run held tools." :source ':test))
           (entered      (sb-thread:make-semaphore))
           (release      (sb-thread:make-semaphore))
           (started      0)
           (job          nil))
      (unwind-protect
           (test-call-with-function-replacements
            (list
             (list 'agent--execute-tool-plan
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (incf started)
                     (sb-sys:without-interrupts
                       (sb-thread:signal-semaphore entered)
                       (sb-thread:wait-on-semaphore release)))))
            (lambda ()
              (setf job
                    (first
                     (job-pool-submit-batch
                      (task-orchestrator-pool orchestrator)
                      (list
                       (list :root-identifier (conversation-identifier (agent-conversation agent))
                             :initargs
                             (list :orchestrator orchestrator :parent-agent agent
                                   :definition definition :item (list :task "Run held tools.")
                                   :completion-owner-conversation
                                   (conversation-identifier (agent-conversation agent))
                                   :execution-identifier (make-identifier))
                             :function
                             (lambda (job)
                               (declare (ignore job))
                               (let ((*agent-maximum-concurrent-tool-workers* 1)
                                     (cl-jobpond:*shutdown-timeout-seconds*   0.05))
                                 (agent--run-tool-wave
                                  agent
                                  (loop for index below 3
                                        collect (list :call
                                                      (agent-test-call
                                                       :call-id   (format nil "held-~D" index)
                                                       :namespace "concurrency" :name "run"
                                                       :arguments "{}")))
                                  :observer (callback-agent-observer-create)))))))))
              (test-assert (sb-thread:wait-on-semaphore entered :timeout 5)
                           "A wave worker enters interrupt-resistant tool work.")
              (job-cancel job :reason ':test-cancel :cascade-p t)
              (multiple-value-bind (snapshot terminal-p) (job-await job :timeout-seconds 2)
                (test-assert (and terminal-p (eq ':aborted (getf snapshot :state)))
                             "The parent becomes terminal before the held worker is released."))
              (let* ((snapshot    (session-job-native-record job (task-job-snapshot job) agent))
                     (waves       (getf snapshot :cleanup-tool-waves))
                     (descendants (getf snapshot :cleanup-descendants)))
                (test-assert (and waves (every (lambda (wave) (getf wave :closing-p)) waves))
                             "Task inspection retains the closing wave pool after its bounded close expires.")
                (test-assert (and descendants
                                  (some (lambda (wave)
                                          (some (lambda (execution)
                                                  (member (getf execution :id) descendants :test #'equal))
                                                (getf wave :jobs)))
                                        waves))
                             "The terminal task exposes its cancelled wave cleanup descendants.")
                (test-assert (task--write-readable-sexp snapshot)
                             "Cleanup inspection is portable task/job data."))
              (sb-thread:signal-semaphore release)
              (test-assert
               (task-tests--wait-until
                (lambda () (null (getf (task-job-snapshot job) :cleanup-tool-waves))) 5)
               "After release, inspection completes pool shutdown and retires retained ownership.")
              (test-assert (= started 1) "Cancelled queued wave tools are never claimed.")))
        (sb-thread:signal-semaphore release)
        (when job
          (job-cancel job :reason ':test-cleanup :cascade-p t)
          (job-await job :timeout-seconds 5)
          (task-tests--wait-until
           (lambda () (null (agent-tool-wave-cleanup-snapshot job))) 5))
        (task-orchestrator-close orchestrator))))
  nil)


;;;; -- Completion Snapshot Baseline --

(defclass agent-test-completion-result (tool-result)
  ((reads :initform 0 :accessor agent-test-completion-result-reads
          :documentation "Number of details snapshots requested.")
   (threads :initform nil :accessor agent-test-completion-result-threads
            :documentation "Threads requesting the owned test details.")
   (payload :initarg :payload :reader agent-test-completion-result-payload
            :documentation "Small tagged details returned by identity."))
  (:documentation "An owned result with observable generic details access."))

(defmethod tool-result-details ((result agent-test-completion-result))
  "Count snapshots of the test-owned payload without changing it."
  (incf (agent-test-completion-result-reads result))
  (push (current-thread) (agent-test-completion-result-threads result))
  (agent-test-completion-result-payload result))

(defclass agent-test-completion-tool (tool)
  ((results :initarg :results :reader agent-test-completion-tool-results
            :documentation "Results indexed by each registered call.")
   (next :initarg :next :accessor agent-test-completion-tool-next
         :documentation "Next permitted finish index, counting down.")
   (lock :initform      (make-lock "Completion fixture")
         :reader        agent-test-completion-tool-lock
         :documentation "Protects finish order.")
   (conditions :initarg :conditions :reader agent-test-completion-tool-conditions
               :documentation "One condition variable for each finish index.")
   (finished :initform nil :accessor agent-test-completion-tool-finished
             :documentation "Reverse chronological finish indices."))
  (:documentation "A real registry tool with barrier-controlled return order."))

(defmethod tool-execute ((tool agent-test-completion-tool)
                         (context tool-context) (arguments hash-table))
  "Return the indexed result after its successor finishes, without sleeps."
  (declare (ignore context))
  (let ((index (parse-integer (tool-argument arguments "index" :required t))))
    (with-lock-held ((agent-test-completion-tool-lock tool))
      (loop until (= index (agent-test-completion-tool-next tool))
            do (unless (condition-wait
                        (aref (agent-test-completion-tool-conditions tool) index)
                        (agent-test-completion-tool-lock tool) :timeout 5)
                 (error "Completion barrier timed out for ~D" index)))
      (push index (agent-test-completion-tool-finished tool))
      (decf (agent-test-completion-tool-next tool))
      (when (plusp index)
        (condition-notify
         (aref (agent-test-completion-tool-conditions tool) (1- index)))))
    (aref (agent-test-completion-tool-results tool) index)))

(-> agent-test-completion-baseline (&key (:count (integer 1 3))) null)
(defun agent-test-completion-baseline (&key count)
  "Exercise actual registry dispatch and completion with finite owned results."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation  (conversation-create configuration))
         (owner-thread  (current-thread))
         (results
           (map 'vector
                (lambda (index)
                  (make-instance 'agent-test-completion-result
                                 :content (format nil "result~D" index)
                                 :success-p t :payload (list :tag index)))
                (loop for index below count collect index)))
         (tool          (make-instance 'agent-test-completion-tool
                                       :namespace "completion" :name "probe"
                                       :description "Return finite owned completion results."
                                       :parameters
                                       (tool-object-schema
                                        (json-object "index" (tool-string-property "Index."))
                                        '("index"))
                                       :results results :next (1- count)
                                       :conditions  (map 'vector (lambda (item)
                                                                   (declare (ignore item))
                                                                   (make-condition-variable))
                                                         results)))
         (registry      (make-instance 'tool-registry))
         (calls         (loop for index below count
                              collect (agent-test-call
                                       :call-id   (format nil "completion~D" index)
                                       :namespace "completion" :name "probe"
                                       :arguments (format nil "{\"index\":\"~D\"}" index))))
         (provider      (make-instance 'scripted-provider
                                       :results (list (agent-test-result "calls" calls)
                                                      (agent-test-result
                                                       "done" (list (agent-test-message "done")))))))
    (unwind-protect
         (let ((*telemetry-run*                         nil)
               (*agent-maximum-concurrent-tool-workers* 3))
           (tool-registry-register registry tool)
           (agent-run-user-turn
            (agent-create :configuration configuration :provider provider
                          :conversation conversation :tool-registry registry :worker ':unused)
            "Exercise finite completion controls.")
           (test-assert (equal (agent-test-tool-outputs conversation)
                               (loop for index below count collect (format nil "result~D" index)))
                        "actual results persist in wire order")
           (test-assert (equal (reverse (agent-test-completion-tool-finished tool))
                               (loop for index downfrom (1- count) to 0 collect index))
                        "barriers enforce reverse finish order")
           (loop for result across results
                 do (test-assert (= 2 (agent-test-completion-result-reads result))
                                 "baseline requests details for persistence and observer")
                    (test-assert (every (lambda (thread) (eq thread owner-thread))
                                        (agent-test-completion-result-threads result))
                                 "baseline snapshots occur on the completion thread")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-agent-completion-accessors () null)
(defun test-agent-completion-accessors ()
  "Characterize a single real-registry result with telemetry disabled."
  (agent-test-completion-baseline :count 1))

(-> test-agent-completion-reverse-order () null)
(defun test-agent-completion-reverse-order ()
  "Characterize three actual registry results finishing in reverse order."
  (agent-test-completion-baseline :count 3))

(-> test-agent-completion-failure-priority () null)
(defun test-agent-completion-failure-priority ()
  "Characterize original condition priority and early consumer failures."
  (dolist (mode '(:original :missing :observer :persistence))
    (let* ((configuration   (test-configuration))
           (root            (test-configuration-root configuration))
           (conversation    (conversation-create configuration))
           (agent           (agent-create :configuration configuration
                                          :provider      (make-instance 'scripted-provider :results nil)
                                          :conversation conversation :tool-registry (agent-test-registry)
                                          :worker        ':unused))
           (original        (make-condition 'job-aborted :message "owned original"
                                            :identifier "completion" :reason ':test))
           (consumer        (make-condition 'simple-error :format-control "owned consumer failure"))
           (executions
             (loop for index below 3
                   collect (list :plan      (list :call        (agent-test-call
                                                                :call-id (format nil "failure~D" index))
                                                  :persistence ':durable)
                                 :result    (cond ((and (= index 1) (eq mode ':missing))
                                                   nil)
                                                  ((= index 1)
                                                   (tool-failure "failed"))
                                                  (t
                                                   (tool-success "ok")))
                                 :condition (cond ((and (= index 1) (not (eq mode ':missing)))
                                                   original)
                                                  ((and (= index 2) (eq mode ':original))
                                                   consumer)))))
           (observer        (callback-agent-observer-create
                             :status-callback
                             (lambda (status details)
                               (declare (ignore details))
                               (when (and (eq mode ':observer) (eq status ':tool-call-completed))
                                 (error consumer)))))
           (escaped         nil)
           (*telemetry-run* nil))
      (unwind-protect
           (progn
             (handler-case
                 (if (eq mode ':persistence)
                     (test-call-with-function-replacements
                      (list (list 'conversation-append-tool-result
                                  (lambda (&rest arguments)
                                    (declare (ignore arguments))
                                    (error consumer))))
                      (lambda ()
                        (agent--complete-tool-executions agent executions
                                                         :observer observer :tool-round 1)))
                     (agent--complete-tool-executions agent executions
                                                      :observer observer :tool-round 1))
               (serious-condition (condition) (setf escaped condition)))
             (test-assert
              (case mode
                (:original
                 (eq escaped original))
                (:missing
                 (and (typep escaped 'agent-loop-error)
                      (eq escaped (getf (second executions) :condition))))
                (t
                 (eq escaped consumer)))
              "the exact original or first consumer condition escapes")
             (test-assert
              (= (length (agent-test-tool-outputs conversation))
                 (case mode (:persistence
                             0)
                            (:observer
                             1)
                            (t
                             3)))
              "consumer failure cuts off completion; stored conditions wait for siblings")
             (when (eq mode ':missing)
               (test-assert (equal (second (agent-test-tool-outputs conversation))
                                   *conversation-interrupted-tool-output*)
                            "missing result persists an explicit unknown outcome")))
        (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))))
  nil)


;;;; -- Completion Ownership --

(defclass agent-test-blocked-result (tool-result)
  ((entered :initform (sb-thread:make-semaphore) :reader agent-test-blocked-result-entered
            :documentation "Signals entry into the selected projection.")
   (await-ready :initform (sb-thread:make-semaphore) :reader agent-test-blocked-result-await-ready
                :documentation "Allows an owner await fault only after projection starts.")
   (release :initform (sb-thread:make-semaphore) :reader agent-test-blocked-result-release
            :documentation "Releases interrupt-resistant projection in fixture cleanup.")
   (thread :initform nil :accessor agent-test-blocked-result-thread
           :documentation "The producer thread whose whole publication path is joined.")
   (calls :initform 0 :accessor agent-test-blocked-result-calls
          :documentation "Projection calls for this result, excluding the sibling plan."))
  (:documentation "A real open-generic result that holds projection until released."))

(defmethod tool-result-completion-projection ((result agent-test-blocked-result))
  "Hold only this result's projection, with a finite emergency release bound."
  (sb-sys:without-interrupts
    (incf (agent-test-blocked-result-calls result))
    (setf (agent-test-blocked-result-thread result) (current-thread))
    (sb-thread:signal-semaphore (agent-test-blocked-result-entered result))
    (sb-thread:signal-semaphore (agent-test-blocked-result-await-ready result))
    (unless (sb-thread:wait-on-semaphore (agent-test-blocked-result-release result) :timeout 30)
      (error "Blocked projection fixture exceeded its emergency release bound.")))
  nil)

(defclass agent-test-blocked-projection-tool (tool)
  ((result :initarg :result :reader agent-test-blocked-projection-tool-result
           :documentation "The selected result whose projection blocks."))
  (:documentation "A registry tool returning one blocked subclass and an ordinary sibling."))

(defmethod tool-execute ((tool agent-test-blocked-projection-tool)
                         (context tool-context) (arguments hash-table))
  "Return the subclass through the real registry rather than replacing projection."
  (declare (ignore context))
  (if (equal "blocked" (tool-argument arguments "kind" :required t))
      (agent-test-blocked-projection-tool-result tool)
      (tool-success "ordinary sibling")))

(-> test-agent-tool-wave-projection-outside-publication () null)
(defun test-agent-tool-wave-projection-outside-publication ()
  "Return cancelled and failed owners before releasing an open-generic projection."
  (dolist (mode '(:cancel :await-fault))
    (with-test-configuration (configuration)
      (let* ((agent                     (task-tests--primary-agent configuration "projection-boundary"))
             (orchestrator              (task-orchestrator-create))
             (definition                (task-agent-definition-create
                                         :name "projection-boundary" :description "Projection boundary test."
                                         :instructions "Run local fixture tools." :source ':test))
             (result                    (make-instance 'agent-test-blocked-result :content "blocked" :success-p t))
             (tool                      (make-instance 'agent-test-blocked-projection-tool
                                                       :namespace "projection-boundary" :name "probe"
                                                       :description "Produce local blocked projection fixtures."
                                                       :parameters  (tool-object-schema
                                                                     (json-object "kind" (tool-string-property "Result kind."))
                                                                     '("kind"))
                                                       :result      result))
             (submit                    (symbol-function 'job-pool-submit-batch))
             (await                     (symbol-function 'job-await))
             (release                   (symbol-function 'agent--release-tool-executions))
             (publish                   (symbol-function 'agent--publish-tool-execution))
             (producer-exited           (sb-thread:make-semaphore))
             (owner                     nil)
             (owner-thread              nil)
             (producers                 nil)
             (captured                  nil)
             (closed-p                  nil)
             (late-installs             0)
             (fault-p                   nil)
             (returned-before-release-p nil))
        (tool-registry-register (agent-tool-registry agent) tool)
        (unwind-protect
             (test-call-with-function-replacements
              (list
               (list 'job-pool-submit-batch
                     (lambda (pool specifications)
                       (if (eq pool (task-orchestrator-pool orchestrator))
                           (funcall submit pool specifications)
                           (setf producers
                                 (funcall
                                  submit pool
                                  (mapcar
                                   (lambda (specification)
                                     (let* ((copy     (copy-list specification))
                                            (function (getf copy :function)))
                                       (setf (getf copy :function)
                                             (lambda (job)
                                               (unwind-protect (funcall function job)
                                                 ;; This surrounds the whole producer, including publication.
                                                 (sb-sys:without-interrupts
                                                   (when (eq (current-thread) (agent-test-blocked-result-thread result))
                                                     (sb-thread:signal-semaphore producer-exited))))))
                                       copy))
                                   specifications))))))
               (list 'job-await
                     (lambda (job &rest arguments)
                       (when (and (eq mode ':await-fault) (eq (current-thread) owner-thread) (not fault-p))
                         (unless (sb-thread:wait-on-semaphore (agent-test-blocked-result-await-ready result) :timeout 5)
                           (error "Projection did not enter before the await fault."))
                         (setf fault-p t)
                         (error "Injected wave await failure after projection entry."))
                       (apply await job arguments)))
               (list 'agent--publish-tool-execution
                     (lambda (vector index execution)
                       (when closed-p (incf late-installs))
                       (funcall publish vector index execution)))
               (list 'agent--release-tool-executions
                     (lambda (vector)
                       (funcall release vector)
                       (setf captured vector closed-p t))))
              (lambda ()
                (unwind-protect
                     (progn
                       (setf owner
                             (first
                              (job-pool-submit-batch
                               (task-orchestrator-pool orchestrator)
                               (list
                                (list :root-identifier (conversation-identifier (agent-conversation agent))
                                      :initargs        (list :orchestrator orchestrator :parent-agent agent
                                                             :definition definition :item (list :task "Run projection fixtures.")
                                                             :completion-owner-conversation
                                                             (conversation-identifier (agent-conversation agent))
                                                             :execution-identifier (make-identifier))
                                      :function
                                      (lambda (job)
                                        (declare (ignore job))
                                        (setf owner-thread (current-thread))
                                        (let ((*agent-maximum-concurrent-tool-workers* 2)
                                              (cl-jobpond:*shutdown-timeout-seconds*   0.05))
                                          (agent--run-tool-wave
                                           agent
                                           (loop for kind in '("blocked" "sibling")
                                                 collect (list :call (agent-test-call
                                                                      :call-id kind :namespace "projection-boundary" :name "probe"
                                                                      :arguments (json-encode (json-object "kind" kind)))
                                                               :tool tool :persistence ':durable))
                                           :observer (callback-agent-observer-create)))))))))
                       (test-assert (sb-thread:wait-on-semaphore (agent-test-blocked-result-entered result) :timeout 5)
                                    (format nil "~A enters the real open-generic projection" mode))
                       (when (eq mode ':cancel)
                         (job-cancel owner :reason ':test-cancel :cascade-p t))
                       (setf returned-before-release-p
                             (nth-value 1 (funcall await owner :timeout-seconds 2))))
                  (sb-thread:signal-semaphore (agent-test-blocked-result-release result))
                  (test-assert (sb-thread:wait-on-semaphore producer-exited :timeout 5)
                               (format nil "~A joins the retained producer's whole publication path" mode))
                  (when owner
                    (test-assert (nth-value 1 (funcall await owner :timeout-seconds 5))
                                 (format nil "~A owner returns after fixture cleanup" mode)))
                  (dolist (producer producers)
                    (test-assert (nth-value 1 (funcall await producer :timeout-seconds 5))
                                 "each wave producer is terminal after release"))
                  (when (agent-test-blocked-result-thread result)
                    (test-assert (not (eq ':timed-out
                                          (sb-thread:join-thread (agent-test-blocked-result-thread result)
                                                                 :timeout 5 :default ':timed-out)))
                                 "the retained producer thread has exited")))
                (format t "PROJECTION-BOUNDARY mode=~S returned-before-release=~S calls=~D late-installs=~D~%"
                        mode returned-before-release-p (agent-test-blocked-result-calls result) late-installs)
                (test-assert returned-before-release-p
                             (format nil "~A owner becomes terminal while projection is still blocked" mode))
                (test-assert (= 1 (agent-test-blocked-result-calls result))
                             "the selected blocked execution projects exactly once")
                (test-assert (and captured (every #'null captured))
                             "cleanup releases all vector roots without late repopulation")
                (test-assert (zerop late-installs) "no execution is installed after wave closure")
                (when (eq mode ':await-fault)
                  (test-assert fault-p "the owner encountered the intended await failure"))))
          (task-orchestrator-close orchestrator)))))
  nil)

(defclass agent-test-projection-tool (tool)
  ((next :initarg :next :accessor agent-test-projection-tool-next
         :documentation "Next reverse body finish index.")
   (reverse-p :initarg :reverse-p :initform t :reader agent-test-projection-tool-reverse-p
              :documentation "Whether the fixture requires reverse tool-body order.")
   (lock :initform (make-lock "Projection fixture") :reader agent-test-projection-tool-lock
         :documentation "Protects fixture counters and weak observations.")
   (conditions :initarg :conditions :reader agent-test-projection-tool-conditions
               :documentation "Per-index reverse body barriers.")
   (weak-results :initform nil :accessor agent-test-projection-tool-weak-results
                 :documentation "Weak observations, never fixture-owned raw results.")
   (weak-backing :initform nil :accessor agent-test-projection-tool-weak-backing
                 :documentation "Weak observations of oversized producer arrays.")
   (finished :initform nil :accessor agent-test-projection-tool-finished
             :documentation "Reverse chronological tool-body finish indices."))
  (:documentation "A real registry producer with no retained raw result owner."))

(defmethod tool-execute ((tool agent-test-projection-tool)
                         (context tool-context) (arguments hash-table))
  "Produce an exact builtin result over a large privately owned backing array."
  (declare (ignore context))
  (let* ((index   (parse-integer (tool-argument arguments "index" :required t)))
         (text    (format nil "owned~D" index))
         (backing (make-string (* 128 1024) :initial-element #\x))
         (view    (make-array (length text) :element-type 'character :displaced-to backing))
         (result  nil))
    (replace view text)
    (setf result (make-instance 'tool-result :content view :success-p t
                               :content-blocks (list view)))
    (with-lock-held ((agent-test-projection-tool-lock tool))
      (when (agent-test-projection-tool-reverse-p tool)
        (loop until (= index (agent-test-projection-tool-next tool))
              do (unless (condition-wait
                          (aref (agent-test-projection-tool-conditions tool) index)
                          (agent-test-projection-tool-lock tool) :timeout 5)
                   (error "Projection reverse barrier timed out"))))
      (push (sb-ext:make-weak-pointer result) (agent-test-projection-tool-weak-results tool))
      (push (sb-ext:make-weak-pointer backing) (agent-test-projection-tool-weak-backing tool))
      (push index (agent-test-projection-tool-finished tool))
      (decf (agent-test-projection-tool-next tool))
      (when (plusp index)
        (condition-notify (aref (agent-test-projection-tool-conditions tool) (1- index)))))
    result))

(-> agent-test-projection-wave
    (&key (:count (integer 1 3)) (:waves (integer 1 4)) (:persistence keyword)) null)
(defun agent-test-projection-wave (&key (count 3) (waves 1) (persistence ':durable))
  "Check the original published vector before joins and its cleanup after transfer."
  (with-test-configuration (configuration)
    (let* ((conversation                            (conversation-create configuration))
           (registry                                (make-instance 'tool-registry))
           (tool                                    (make-instance 'agent-test-projection-tool
                                                                   :namespace "ownership" :name "probe"
                                                                   :description "Produce privately owned completion payloads."
                                                                   :parameters  (tool-object-schema
                                                                                 (json-object "index" (tool-string-property "Index."))
                                                                                 '("index"))
                                                                   :next        (1- count)
                                                                   :conditions  (map 'vector (lambda (index)
                                                                                               (declare (ignore index))
                                                                                               (make-condition-variable))
                                                                                     (make-list count))))
           (agent                                   (agent-create :configuration configuration :conversation conversation
                                                                  :provider (make-instance 'scripted-provider :results nil)
                                                                  :tool-registry registry :worker ':unused))
           (plans                                   (loop for index below count
                                                          collect (list :call (agent-test-call
                                                                               :call-id   (format nil "owned~D" index)
                                                                               :namespace "ownership" :name "probe"
                                                                               :arguments (format nil "{\"index\":\"~D\"}" index))
                                                                        :tool tool :persistence persistence)))
           (publish                                 (symbol-function 'agent--publish-tool-execution))
           (await                                   (symbol-function 'job-await))
           (lock                                    (make-lock "Published vector fixture"))
           (ready                                   (make-condition-variable))
           (published                               0)
           (released-p                              (= count 1))
           (vectors                                 nil)
           (plists                                  nil)
           (jobs                                    nil)
           (owner                                   (current-thread))
           (*agent-maximum-concurrent-tool-workers* 3)
           (*telemetry-run*                         nil))
      (tool-registry-register registry tool)
      (dotimes (wave waves)
        (setf (agent-test-projection-tool-next tool) (1- count)
              (agent-test-projection-tool-finished tool) nil
              published 0 released-p (= count 1) vectors nil plists nil jobs nil)
       (test-call-with-function-replacements
        (list
         (list 'agent--publish-tool-execution
               (lambda (vector index execution)
                 (funcall publish vector index execution)
                 ;; Retain only the already projected execution for explicit cleanup checks.
                 (with-lock-held (lock)
                   (pushnew vector vectors :test #'eq)
                   (push (aref vector index) plists)
                   (incf published)
                   (condition-notify ready)
                   (when (= count 1)
                     (test-assert (typep (getf (aref vector index) :result) 'tool-completion-result)
                                 "single-call publication contains an owned envelope")))
                 nil))
         (list 'job-await
               (lambda (job &rest arguments)
                 (when (eq (current-thread) owner)
                   (with-lock-held (lock)
                     (loop until (= published count)
                           do (unless (condition-wait ready lock :timeout 5)
                                (error "Projection pre-join barrier timed out")))
                     (unless released-p
                       (test-assert (= (length vectors) 1) "workers publish into one original vector")
                       (test-assert
                        (every (lambda (execution)
                                 (typep (getf execution :result) 'tool-completion-result))
                               (first vectors))
                        "all completed slots contain only bounded envelopes before join")
                      (test-assert
                       (every (lambda (execution)
                                (null (array-displacement
                                       (tool-result-content (getf execution :result)))))
                              (first vectors))
                       "published text owns storage before the owner's job wait")
                      (setf released-p t))
                     (pushnew job jobs :test #'eq)))
                 (apply await job arguments))))
        (lambda ()
          (agent--run-tool-wave agent plans
                               :observer   (callback-agent-observer-create)
                               :tool-round 1)))
       (test-assert (every (lambda (vector) (every #'null vector)) vectors)
                    "original vector slots are cleared after completion")
       (test-assert (every (lambda (execution)
                             (loop for tail on execution by (lambda (tail) (rest (rest tail)))
                                   always (null (second tail))))
                           plists)
                    "shared execution plist values do not retain plans or results")
       (test-assert (every #'job-terminal-p jobs)
                    "all observed wave jobs are terminal before root release")
       (test-assert (equal (reverse (agent-test-projection-tool-finished tool))
                           (loop for index downfrom (1- count) to 0 collect index))
                    "actual tool bodies finish in reverse order")
       (test-assert (equal (agent-test-tool-outputs conversation)
                           (loop repeat (1+ wave)
                                 append (loop for index below count collect (format nil "owned~D" index))))
                    "genuine persistence retains wire-ordered values")
      ;; This supplements explicit roots; producer frames and fixture owners are gone.
       (sb-ext:gc :full t)
       (test-assert (every (lambda (pointer) (null (sb-ext:weak-pointer-value pointer)))
                           (agent-test-projection-tool-weak-results tool))
                    "raw results are collectible after joined producer frames exit")
       (test-assert (every (lambda (pointer) (null (sb-ext:weak-pointer-value pointer)))
                           (agent-test-projection-tool-weak-backing tool))
                    "tiny projected text does not retain oversized backing arrays"))))
  nil)

(-> test-agent-completion-bounded-ownership () null)
(defun test-agent-completion-bounded-ownership ()
  "Exercise direct and reverse parallel registry publication and owned storage."
  (agent-test-projection-wave :count 1)
  (agent-test-projection-wave :count 3 :persistence ':next-response)
  nil)

(-> test-agent-completion-four-wave-release () null)
(defun test-agent-completion-four-wave-release ()
  "Exercise four waves on one agent and conversation, plus storm failure ownership transfer."
  (agent-test-projection-wave :count 3 :waves 4)
  (let* ((state (agent-tool-storm-state-create))
         (plan  (list :storm-state state :signature "owned-call")))
    (agent--tool-retry-output plan (tool-result-completion-projection (tool-failure "owned failure")))
    (test-assert (equal (gethash "owned-call" (agent-tool-storm-state-failures state)) "owned failure")
                 "storm state owns the bounded failure text semantically")
    (agent--tool-retry-output plan (tool-result-completion-projection (tool-success "ok")))
    (test-assert (zerop (hash-table-count (agent-tool-storm-state-failures state)))
                 "successful retry removes the storm failure owner"))
  nil)

(defclass agent-test-unsafe-result (tool-result) ()
  (:documentation "An extension inheriting builtin readers without projection authority."))

(-> test-agent-completion-projection-compatibility () null)
(defun test-agent-completion-projection-compatibility ()
  "Verify bounded builtin values, recursive contracts and unsafe method fallback."
  (let* ((base       (tool-success "ok"))
         (skill      (make-instance 'skill-load-result :content "selected" :success-p t
                                    :name "local-development" :newly-selected-p t))
         (shell      (shell-command-result "exit" (list :exit-code 7) nil))
         (task       (make-instance 'task-tool-result :content "snapshot" :success-p t
                                    :details (list :job (list :state ':completed))))
         (mission    (make-instance 'mission-tool-result :content "snapshot" :success-p t
                                    :details (list :goal (list :status ':active))))
         (executable (make-instance 'executable-skill-result :content "executed" :success-p t
                                    :record (list :status ':completed)))
         (mcp        (make-instance 'mcp-content-result :content "text" :success-p t
                                    :sections (list (list ':text 0 4))))
         (contract   (tool-contract--recording-result mcp (list (json-object "message" "mapping"))))
         (extension  (make-instance 'agent-test-unsafe-result :content "extension" :success-p t)))
    (dolist (result (list base skill shell task mission executable mcp contract))
      (let ((projection (tool-result-completion-projection result)))
        (test-assert (typep projection 'tool-completion-result) "reviewed bounded builtin opts in")
        (when projection
          (test-assert (equal (tool-result-details projection) (tool-result-details result))
                       "builtin metadata values survive pure snapshotting")
          (test-assert (and (equal (tool-result-content projection) (tool-result-content result))
                            (not (eq (tool-result-content projection) (tool-result-content result))))
                       "snapshot content has equal values and independent ownership"))))
    (test-assert (null (tool-result-completion-projection extension)) "inherited extensions stay legacy")
    (test-assert (null (tool-result-completion-projection
                        (tool-contract--recording-result extension (list (json-object "message" "mapping")))))
                 "contract over an extension stays legacy")
    (test-assert (null (tool-result-completion-projection
                        (shell-command-result "big" (list :text (make-string 70000)) t)))
                 "oversized raw observer metadata stays legacy")
    (let* ((method (eval `(defmethod tool-result-details :around ((result (eql ,base)))
                           (call-next-method)))))
      (unwind-protect
           (test-assert (null (tool-result-completion-projection base))
                        "EQL accessor methods prevent worker-thread snapshotting")
        (remove-method #'tool-result-details method)))
    (let ((method (eval '(defmethod tool-result-details :after ((result shell-command-tool-result))
                          (declare (ignore result)) nil))))
      (unwind-protect
           (test-assert (null (tool-result-completion-projection shell))
                        "auxiliary methods prevent worker-thread snapshotting")
        (remove-method #'tool-result-details method)))
    (let ((details (tool-result-details skill)))
      (test-assert (eq details (conversation--bounded-tool-details details))
                   "the signed accepted metadata helper preserves value identity")))
  nil)

(-> test-agent-completion-delivery-ownership () null)
(defun test-agent-completion-delivery-ownership ()
  "Reuse genuine jobpond delivery, receipt recovery and unknown-outcome owners."
  (test-job-completion-durable-delivery)
  (test-job-completion-delivery-crash-points)
  (test-job-completion-terminal-truth-and-reconstruction)
  (let ((*task-completion-capacity*      2)
        (*task-completion-history-limit* 2))
    (job-completion-tests--fixture
     (lambda (parent runtime configuration)
       (declare (ignore configuration))
       (loop repeat 4 do
         (let* ((job        (job-completion-tests--start parent runtime))
                (identifier (session-job-execution-identifier job)))
           (test-assert (= 1 (length (task-completion-deliver parent)))
                        "actual detached delivery consumes one completed notice")
           (test-assert (task-completion--delivered-p (agent-conversation parent) identifier)
                        "durable append receipt exists before acknowledgment pruning")
           (task-completion-watch job parent)
           (test-assert (null (task-completion-pending parent))
                        "a repeated genuine watch does not redeliver an acknowledged result")))
       (test-assert (null (task-completion--messages (task-completion--service parent)))
                    "receipt-backed subscription history is pruned after actual delivery"))))
  nil)

(-> test-agent-completion-owned-failures () null)
(defun test-agent-completion-owned-failures ()
  "Verify original vector cleanup and exact condition identity through finite failures."
  (dolist (mode '(:submission :wait :observer :persistence))
    (with-test-configuration (configuration)
      (let* ((conversation                            (conversation-create configuration))
             (registry                                (make-instance 'tool-registry))
             (tool                                    (make-instance 'agent-test-projection-tool
                                                                     :namespace "ownership" :name "failure"
                                                                     :description "Produce owned failure controls."
                                                                     :parameters  (tool-object-schema
                                                                                   (json-object "index" (tool-string-property "Index."))
                                                                                   '("index"))
                                                                     :reverse-p nil :next 2
                                                                     :conditions  (vector (make-condition-variable)
                                                                                          (make-condition-variable)
                                                                                          (make-condition-variable))))
             (agent                                   (agent-create :configuration configuration :conversation conversation
                                                                    :provider (make-instance 'scripted-provider :results nil)
                                                                    :tool-registry registry :worker ':unused))
             (plans                                   (loop for index below 3
                                                            collect (list :call (agent-test-call
                                                                                 :call-id   (format nil "failure~D" index)
                                                                                 :namespace "ownership" :name "failure"
                                                                                 :arguments (format nil "{\"index\":\"~D\"}" index))
                                                                          :tool tool :persistence ':durable)))
             (fault                                   (make-condition 'simple-error :format-control "owned wave fault"))
             (observer                                (callback-agent-observer-create
                                                       :status-callback
                                                       (lambda (status details)
                                                         (declare (ignore details))
                                                         (when (and (eq mode ':observer) (eq status ':tool-call-completed))
                                                           (error fault)))))
             (publish                                 (symbol-function 'agent--publish-tool-execution))
             (submit                                  (symbol-function 'job-pool-submit-batch))
             (await                                   (symbol-function 'job-await))
             (persist                                 (symbol-function 'conversation-append-tool-result))
             (lock                                    (make-lock "Owned failure observations"))
             (vectors                                 nil)
             (plists                                  nil)
             (jobs                                    nil)
             (wait-failed-p                           nil)
             (escaped                                 nil)
             (*agent-maximum-concurrent-tool-workers* 3)
             (*telemetry-run*                         nil))
        (tool-registry-register registry tool)
        (test-call-with-function-replacements
         (list
          (list 'agent--publish-tool-execution
                (lambda (vector index execution)
                  (funcall publish vector index execution)
                  (with-lock-held (lock)
                    (pushnew vector vectors :test #'eq)
                    (push (aref vector index) plists))))
          (list 'job-pool-submit-batch
                (lambda (pool entries &rest options)
                  (setf jobs (apply submit pool entries options))
                  (when (eq mode ':submission)
                    (dolist (job jobs) (funcall await job))
                    (error fault))
                  jobs))
          (list 'job-await
                (lambda (job &rest arguments)
                  (prog1 (apply await job arguments)
                    (when (and (eq mode ':wait) (not wait-failed-p))
                      (setf wait-failed-p t)
                      (dolist (sibling jobs) (funcall await sibling))
                      (error fault)))))
          (list 'conversation-append-tool-result
                (lambda (&rest arguments)
                  (when (eq mode ':persistence) (error fault))
                  (apply persist arguments))))
         (lambda ()
           (handler-case
               (agent--run-tool-wave agent plans :observer observer :tool-round 1)
             (serious-condition (condition) (setf escaped condition)))))
        (test-assert (eq escaped fault) "the exact injected condition escapes the genuine wave")
        (test-assert (= (length plists) 3) "all finite producers published before completion failed")
        (test-assert (every (lambda (vector) (every #'null vector)) vectors)
                     "the original vector is cleared on every tested unwind")
        (test-assert (every (lambda (execution)
                             (loop for tail on execution by (lambda (tail) (rest (rest tail)))
                                   always (null (second tail))))
                           plists)
                     "the published execution plist roots are cleared on every tested unwind")
        (test-assert (and (= (length jobs) 3) (every #'job-terminal-p jobs))
                     "all actual finite producer jobs are terminal after cleanup")
        (test-assert (= (length (agent-test-tool-outputs conversation))
                       (if (eq mode ':observer) 1 0))
                     "consumer faults cut off transfer; failed infrastructure prevents completion"))))
  nil)

(-> test-agent-completion-telemetry () null)
(defun test-agent-completion-telemetry ()
  "Verify completion consumers with isolated telemetry capture, including emission failure."
  (dolist (mode '(:off :on :failure))
    (with-test-configuration (configuration)
      (let* ((conversation    (conversation-create configuration))
             (agent           (agent-create :configuration configuration :conversation conversation
                                            :provider (make-instance 'scripted-provider :results nil)
                                            :tool-registry (agent-test-registry) :worker ':unused))
             (result          (tool-result-completion-projection
                               (shell-command-result "exit evidence" (list :exit-code 7) nil)))
             (execution       (list :plan (list :call        (agent-test-call :call-id "exit-evidence")
                                                :persistence ':durable)
                                    :result result :telemetry-duration-ms 12))
             (observed        nil)
             (notes           nil)
             (observer        (callback-agent-observer-create
                               :status-callback
                               (lambda (status details)
                                 (when (eq status ':tool-call-completed) (setf observed details)))))
             (*telemetry-run* (not (eq mode ':off))))
        (test-call-with-function-replacements
         (list (list 'telemetry-note-tool
                     (lambda (&rest arguments)
                       (push arguments notes)
                       (when (eq mode ':failure) (error "isolated telemetry failure")))))
         (lambda ()
           (agent--complete-tool-executions agent (vector execution)
                                            :observer observer :tool-round 1)))
        (test-assert (equal (agent-test-tool-outputs conversation) '("exit evidence"))
                     "telemetry presence or failure does not interrupt actual result persistence")
        (test-assert (= 7 (getf (getf observed :details) :exit-code))
                     "the observer receives the bounded shell exit metadata")
        (test-assert (= (length notes) (if (eq mode ':off) 0 1))
                     "completion invokes isolated telemetry only when enabled")
        (when notes
          (test-assert (= 7 (getf (first notes) :exit-code))
                       "telemetry receives the shell exit code before optional emission failure")))))
  nil)

(defclass agent-test-faulting-projection-result (tool-result)
  ((condition :initarg :condition :reader agent-test-faulting-projection-result-condition
              :documentation "The exact condition signalled by open projection."))
  (:documentation "A genuine extension whose projection signals a selected condition."))

(defmethod tool-result-completion-projection ((result agent-test-faulting-projection-result))
  "Signal from real generic dispatch, including non-ERROR control conditions."
  (error (agent-test-faulting-projection-result-condition result)))

(-> agent-test-projection-fallback-wave (integer condition) null)
(defun agent-test-projection-fallback-wave (count condition)
  "Observe raw object identity at publication and ordinary delivery in a real wave."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (registry     (make-instance 'tool-registry))
           (raw          (make-instance 'agent-test-faulting-projection-result
                                        :content "legacy projection" :success-p t :condition condition))
           (tool         (make-instance 'agent-test-blocked-projection-tool
                                        :namespace "fallback" :name "probe"
                                        :description "Return a faulting projection extension."
                                        :parameters  (tool-object-schema
                                                      (json-object "kind" (tool-string-property "Kind.")) '("kind"))
                                        :result      raw))
           (agent        (agent-create :configuration configuration :conversation conversation
                                       :provider (make-instance 'scripted-provider :results nil)
                                       :tool-registry registry :worker ':unused))
           (publish      (symbol-function 'agent--publish-tool-execution))
           (seen         nil)
           (lock         (make-lock "Fallback publication identities")))
      (tool-registry-register registry tool)
      (test-call-with-function-replacements
       (list (list 'agent--publish-tool-execution
                   (lambda (vector index execution)
                     (with-lock-held (lock)
                       (push (eq raw (getf execution :result)) seen))
                     (funcall publish vector index execution))))
       (lambda ()
         (agent--run-tool-wave
          agent (loop for index below count
                      collect (list :call (agent-test-call
                                           :call-id (format nil "fallback~D" index)
                                           :namespace "fallback" :name "probe" :arguments "{\"kind\":\"blocked\"}")
                                    :tool tool :persistence ':durable))
          :observer (callback-agent-observer-create) :tool-round 1)))
      (test-assert (and (= count (length seen)) (every #'identity seen))
                   "ordinary projection failure publishes the identical raw result")
      (test-assert (= count (length (agent-test-tool-outputs conversation)))
                   "every raw fallback is delivered by the ordinary completion path")))
  nil)

(-> test-agent-projection-fallback-single () null)
(defun test-agent-projection-fallback-single ()
  "Exercise ordinary open-generic failure on the direct wave path."
  (agent-test-projection-fallback-wave 1 (make-condition 'simple-error :format-control "projection failed"))
  (agent-test-projection-fallback-wave 1 (make-condition 'type-error :datum 7 :expected-type 'string))
  nil)

(-> test-agent-projection-fallback-parallel () null)
(defun test-agent-projection-fallback-parallel ()
  "Exercise ordinary open-generic failure on genuine concurrent producers."
  (agent-test-projection-fallback-wave 3 (make-condition 'simple-error :format-control "projection failed"))
  (agent-test-projection-fallback-wave 3 (make-condition 'type-error :datum 7 :expected-type 'string))
  nil)

(-> test-agent-projection-control-identity () null)
(defun test-agent-projection-control-identity ()
  "Propagate administrative errors and actual non-ERROR controls without substitution."
  (dolist (name '(conversation-invariant-error agent-loop-error active-image-corruption
                  application-turn-cancelled cl-jobpond:job-aborted))
    (let* ((condition (make-condition name))
           (raw       (make-instance 'agent-test-faulting-projection-result
                                     :content "control" :success-p t :condition condition))
           (execution (list :result raw))
           (escaped   nil))
      (handler-case (agent--project-tool-execution execution)
        (serious-condition (caught) (setf escaped caught)))
      (test-assert (eq condition escaped) "the identical administrative or cancellation condition propagates")
      (test-assert (eq raw (getf execution :result)) "control propagation preserves the raw result")
      (when (member name '(application-turn-cancelled cl-jobpond:job-aborted))
        (test-assert (not (typep condition 'error)) "actual cancellation controls are not ERROR"))))
  nil)
