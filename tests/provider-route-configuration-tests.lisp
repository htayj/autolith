(in-package #:autolith)

;;;; -- Provider Route Configuration Tests --

(-> route-tests--with-providers (function) null)
(defun route-tests--with-providers (function)
  "Run FUNCTION with local metadata and factories that must never run."
  (let ((snapshot (provider--registry-snapshot)))
    (unwind-protect
         (progn
           (dolist (name '("route-a" "route-b"))
             (register-provider
              name :family ':codex :models '("shared" "other")
              :factory       (lambda (&rest arguments)
                               (declare (ignore arguments))
                               (error 'configuration-error :message "Unexpected factory call."))
              :authenticator (lambda (&rest arguments)
                               (declare (ignore arguments))
                               (error 'configuration-error :message "Unexpected login call."))))
           (test-call-with-function-replacements
            (loop for name in '(provider-create provider-authentication-provider provider-refresh-models)
                  collect (list name (lambda (&rest arguments)
                                       (declare (ignore arguments))
                                       (error 'configuration-error
                                              :message "Unexpected provider boundary call."))))
            function))
      (provider--registry-restore snapshot)))
  nil)

(-> test-provider-route-defaults () null)
(defun test-provider-route-defaults ()
  "Empty defaults leave a single primary route and threshold three."
  (route-tests--with-providers
   (lambda ()
     (with-test-configuration (configuration)
       (let ((copy (configuration-copy configuration :model-provider "route-a" :model "shared")))
         (test-assert (string= "" (config :provider-fallback-mappings copy)) "empty mapping default")
         (multiple-value-bind (routes threshold) (provider-routes-resolve copy)
           (test-assert (= threshold 3) "default threshold")
           (test-assert (= 1 (length routes)) "primary only")
           (test-assert (string= "route-a" (provider-route-provider (first routes))) "explicit primary")
           (test-assert (string= "shared" (provider-route-model (first routes))) "exact model")))))))

(-> test-provider-route-settings-validation () null)
(defun test-provider-route-settings-validation ()
  "Reject malformed, ambiguous, duplicate and self mappings and invalid thresholds."
  (with-test-configuration (configuration)
    (dolist (text '("{}" "null" "false" "[1]" "[[[\"a\",\"m\"]]]"
                    "[[[\"a\"],[\"b\",\"m\"]]]"
                    "[[[\"a\",\"m\"],[\"\",\"m\"]]]"
                    "[[[\"a\",\"m\"],[\"a\",\"m\"]]]"
                    "[[[\"a\",\"m\"],[\"b\",\"m\"]],[[\"A\",\"m\"],[\"c\",\"m\"]]]"
                    "[[[\"a\",\"m\",\"extra\"],[\"b\",\"m\"]]]"))
      (test-assert (handler-case
                      (progn (setf (config :provider-fallback-mappings configuration) text) nil)
                    (setting-invalid () t))
                   "invalid mapping rejected at settings boundary"))
    (dolist (value '(0 -1 1.5 nil :unlimited))
      (test-assert (handler-case
                      (progn (setf (config :provider-fallback-threshold configuration) value) nil)
                    (setting-invalid () t))
                   "threshold must be a positive integer"))
    (setf (config :provider-fallback-threshold configuration) 7)
    (test-assert (= 7 (config :provider-fallback-threshold configuration)) "explicit threshold")
    (setf (config :provider-fallback-mappings configuration) "[]")
    (test-assert (null (configuration--fallback-mappings
                        (config :provider-fallback-mappings configuration))) "empty JSON array"))
  nil)

(-> test-provider-route-explicit-order-and-isolation () null)
(defun test-provider-route-explicit-order-and-isolation ()
  "Resolve same/different model mappings without construction or durable selection changes."
  (route-tests--with-providers
   (lambda ()
     (with-test-configuration (configuration)
       (let* ((copy (configuration-copy configuration :model-provider "route-a" :model "shared"))
              (text "[[[\"route-a\",\"shared\"],[\"route-b\",\"shared\"],[\"route-a\",\"other\"],[\"route-b\",\"other\"]]]"))
         (setf (config :provider-fallback-mappings copy) text)
         (let* ((saved     (uiop:read-file-string (configuration-preferences-path copy)))
                (routes    (provider-routes-resolve copy))
                (pairs     (mapcar #'provider-route--key routes))
                (alternate (provider-route-configuration (second routes) copy)))
           (test-assert (equal pairs '(("route-a" "shared") ("route-b" "shared")
                                       ("route-a" "other") ("route-b" "other"))) "configured serial order")
           (test-assert (string= "route-b" (config :model-provider alternate)) "alternate configuration")
           (test-assert (string= "route-a" (config :model-provider copy)) "primary selection untouched")
           (test-assert (string= "shared" (config :model copy)) "primary model untouched")
           (test-assert (string= "" (config :provider-fallback-mappings configuration)) "parent config isolated")
           (test-assert (string= saved (uiop:read-file-string (configuration-preferences-path copy)))
                        "resolution and route copy leave durable settings untouched")
           (setf (config :provider-fallback-mappings alternate) "[]")
           (test-assert (string= text (config :provider-fallback-mappings copy)) "copy setting isolated")
           (let ((name (provider-route-provider (second routes))))
             (setf (char name 0) #\X)
             (test-assert (string= "route-b" (provider-route-provider (second routes))) "immutable name view"))
           (let ((model (provider-route-model (second routes))))
             (setf (char model 0) #\X)
             (test-assert (string= "shared" (provider-route-model (second routes))) "immutable model view"))
           (setf (config :provider-fallback-mappings copy) "[]")
           (test-assert (= 4 (length routes)) "resolved request snapshot independent")
           (test-assert (= 1 (length (provider-routes-resolve copy))) "new resolution starts primary")))))))

(-> test-provider-route-terminal-resolution () null)
(defun test-provider-route-terminal-resolution ()
  "Unknown providers, models, alias collisions and implicit selections fail terminally."
  (route-tests--with-providers
   (lambda ()
     (with-test-configuration (configuration)
       (let ((copy (configuration-copy configuration :model-provider "route-a" :model "shared")))
         (dolist (text '("[[[\"route-a\",\"shared\"],[\"missing\",\"shared\"]]]"
                         "[[[\"route-a\",\"shared\"],[\"route-b\",\"missing\"]]]"
                         "[[[\"route-a\",\"shared\"],[\"route-b\",\"route-b/shared\"]]]"
                         "[[[\"chatgpt\",\"gpt-6.1-sol\"],[\"openai\",\"gpt-6.1-sol\"]]]"))
           (setf (config :provider-fallback-mappings copy) text)
           (test-assert (handler-case (progn (provider-routes-resolve copy) nil)
                          (configuration-error () t)) "terminal configuration error")))))))


;;;; -- Request-local Runtime Routing --

(-> route-tests--call-with-runtime (function &key (:threshold (integer 1))) null)
(defun route-tests--call-with-runtime (function &key (threshold 3))
  "Use ordinary local factories and fixture-only credentials for serial dispatch."
  (let ((snapshot (provider--registry-snapshot)))
    (unwind-protect
         (with-test-configuration (configuration)
           (dolist (name '("runtime-a" "runtime-b"))
             (let ((name name))
               (register-provider
                name :family ':openai :protocol ':chat-completions
                :models   '("runtime-one" "runtime-two")
                :endpoint "http://127.0.0.1:1/v1/chat/completions"
                :factory  (lambda (configuration &key reasoning-summaries-p)
                            (declare (ignore reasoning-summaries-p))
                            (let ((provider (openai-compatible-provider-create
                                             configuration :name name :family ':openai)))
                              (api-key-credential-manager-save-key
                               (provider-credential-manager provider)
                               (format nil "owned-runtime-fixture-~A" name))
                              provider)))))
           (let* ((copy         (configuration-copy configuration
                                                    :model-provider "runtime-a" :model "runtime-one"))
                  (conversation (conversation-create copy :identifier "runtime-routing")))
             (setf (config :provider-fallback-mappings copy)
                   "[[[\"runtime-a\",\"runtime-one\"],[\"runtime-b\",\"runtime-two\"],[\"runtime-a\",\"runtime-two\"]]]"
                   (config :provider-fallback-threshold copy) threshold)
             (funcall function (provider-create copy) conversation copy)))
      (provider--registry-restore snapshot)))
  nil)

(-> test-provider-runtime-routing-cycle () null)
(defun test-provider-runtime-routing-cycle ()
  "Exercise actual credentials and projection on each serial route, then restart primary."
  (route-tests--call-with-runtime
   (lambda (provider conversation configuration)
     (let ((attempts nil) (requests nil) (waits nil) (count 0)
           (result (make-instance 'provider-result :response-id "owned-runtime-success"
                                  :output-items nil :tool-calls nil :usage nil :turn-state nil))
           (saved  (uiop:read-file-string (configuration-preferences-path configuration))))
       (let ((*bounded-retry-sleep-function* (lambda (seconds) (push seconds waits))))
         (test-call-with-function-replacements
          (list
           (list 'cl-llm-provider-api::provider-execute-request
                 (lambda (adapter request &key secrets event-callback cleanup transport completion)
                   (declare (ignore event-callback cleanup transport completion))
                   (test-assert (member (format nil "owned-runtime-fixture-~A"
                                                (provider-account-label adapter))
                                        secrets :test #'equal)
                                "ordinary selected-route credential resolution")
                   (push (list (provider-account-label adapter) (json-get request "model")) attempts)
                   (push request requests)
                   (if (<= (incf count) 9)
                       (error 'provider-transport-error :message "owned routing reset")
                      (values result :second nil :fourth)))))
          (lambda ()
            (test-assert
             (equal (list result :second nil :fourth)
                    (multiple-value-list
                     (provider-stream-turn provider conversation :tool-namespaces #()
                                           :event-callback                        #'identity)))
             "success preserves every value without another health dispatch")
            (test-assert (= count 10) "three failures per selected route then primary")
            (test-assert
             (equal (reverse attempts)
                    '(("runtime-a" "runtime-one") ("runtime-a" "runtime-one")
                      ("runtime-a" "runtime-one") ("runtime-b" "runtime-two")
                      ("runtime-b" "runtime-two") ("runtime-b" "runtime-two")
                      ("runtime-a" "runtime-two") ("runtime-a" "runtime-two")
                      ("runtime-a" "runtime-two") ("runtime-a" "runtime-one")))
             "explicit ordered route cycle")
            (test-assert (= 10 (length (remove-duplicates requests :test #'eq)))
                         "fresh ordinary wire projection on every attempt")
            (test-assert (and (= 9 (length waits)) (> (reduce #'+ waits) 300)
                              (every (lambda (delay) (<= delay 300)) waits))
                         "backoff persists across route changes")
            (provider-stream-turn provider conversation :tool-namespaces #() :event-callback #'identity)
            (test-assert (equal (first attempts) '("runtime-a" "runtime-one"))
                         "each new request starts primary")
            (test-assert (= count 11) "completed request is never replayed"))))
       (test-assert (and (string= "runtime-one" (config :model configuration))
                         (string= "runtime-a" (config :model-provider configuration))
                         (string= saved (uiop:read-file-string
                                         (configuration-preferences-path configuration))))
                    "dispatch never changes durable provider preferences"))))
  nil)

(-> test-provider-runtime-routing-terminal-and-generation () null)
(defun test-provider-runtime-routing-terminal-and-generation ()
  "Keep terminal errors and cumulative generation safety outside fallback."
  (route-tests--call-with-runtime
   (lambda (provider conversation configuration)
     (declare (ignore configuration))
     (dolist (mode '(:authentication :projection :observer :generation :cancel))
       (let ((attempts nil) (count 0) (failure nil)
             (*provider-connection-owner* (list :runtime-owner)))
        (let ((*bounded-retry-sleep-function*
                (lambda (seconds)
                  (declare (ignore seconds))
                  (when (> count 12)
                    (error 'configuration-error :message "Owned fixture attempt safety stop.")))))
           (test-call-with-function-replacements
            (list
             (list 'cl-llm-provider-api::provider-execute-request
                   (lambda (adapter request &key secrets event-callback cleanup transport completion)
                     (declare (ignore request secrets cleanup transport completion))
                     (push (provider-account-label adapter) attempts)
                     (incf count)
                     (cond
                       ((eq mode :authentication)
                        (error 'provider-unauthorized :message "owned unauthorized" :status 401))
                       ((eq mode :projection)
                        (error 'configuration-error :message "owned incompatible request"))
                       ((eq mode :observer)
                        (funcall event-callback :owned-observer-event))
                       ((eq mode :cancel)
                        (provider-cancel-connection-owner *provider-connection-owner*)
                        (error 'provider-transport-error :message "owned cancelled opening"))
                       (t
                        ;; Switch once before generation; later failures must not switch again.
                        (unless (= count 1)
                          (funcall event-callback (make-instance 'assistant-delta-event :text "partial")))
                        (error 'provider-transport-error :message "owned generated reset"))))))
            (lambda ()
              (handler-case
                  (provider-stream-turn
                   provider conversation :tool-namespaces #()
                   :event-callback                        (lambda (event)
                                                            (when (eq event :owned-observer-event)
                                                             (error 'provider-transport-error :message "owned observer failure"))))
                (error (condition) (setf failure condition))))))
        (test-assert failure "terminal failure escapes")
        (if (eq mode :generation)
            (test-assert (and (typep failure 'provider-stream-abandoned)
                              (= count 4)
                              (equal (reverse attempts) '("runtime-a" "runtime-b" "runtime-b" "runtime-b")))
                         "generation history survives route selection and blocks later switching")
            (test-assert (and (= count 1) (equal attempts '("runtime-a")))
                         "authentication, incompatibility, observer and cancellation do not fall back")))))
   :threshold 1)
  nil)

(-> test-provider-runtime-throttling () null)
(defun test-provider-runtime-throttling ()
  "Retry throttling beyond the imported finite policy while leaving quota terminal."
  (provider-recovery-tests--call-with-provider
   1
   (lambda (provider conversation)
     (declare (ignore conversation))
     (dolist (code '(nil "rate_limit_error" "overloaded_error"))
       (let ((count 0) (waits 0) (result nil))
         (let ((*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds)) (incf waits))))
           (setf result
                 (provider--call-with-bounded-retries
                  provider
                  (lambda (refresh callback)
                    (declare (ignore refresh callback))
                    (if (< (incf count) 12)
                        (error 'provider-retryable-error :message "owned throttle"
                               :status (and (null code) 429) :code code)
                        :healthy))
                  #'identity)))
         (test-assert (and (eq result :healthy) (= count 12) (= waits 11))
                      "HTTP and streamed throttling use caller lifetime")))))
  nil)

(-> test-provider-runtime-routing-projection-rejection () null)
(defun test-provider-runtime-routing-projection-rejection ()
  "A terminal alternate preparation failure escapes before transmission or another route."
  (route-tests--call-with-runtime
   (lambda (provider conversation configuration)
     (declare (ignore configuration))
     (let ((original (symbol-function 'provider-request-object))
           (projections nil) (transmissions 0) (failure nil)
           (rejected (make-condition 'configuration-error :message "Owned unsupported content or tool.")))
       (let ((*bounded-retry-sleep-function*
               (lambda (seconds)
                 (declare (ignore seconds))
                 (when (> transmissions 12)
                   (error 'provider-owner-ended :message "Owned fixture attempt safety stop.")))))
         (test-call-with-function-replacements
          (list
           (list 'provider-request-object
                 (lambda (adapter owner tools &rest arguments)
                   (when (typep owner 'conversation)
                     (test-assert (eq owner conversation) "alternate uses the original logical conversation")
                     (push (provider-account-label adapter) projections)
                     (when (string= "runtime-b" (provider-account-label adapter)) (error rejected)))
                   (apply original adapter owner tools arguments)))
           (list 'cl-llm-provider-api::provider-execute-request
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (incf transmissions)
                   (error 'provider-transport-error :message "Owned primary unavailable."))))
          (lambda ()
            (handler-case
                (provider-stream-turn provider conversation :tool-namespaces #() :event-callback #'identity)
              (error (condition) (setf failure condition))))))
       (test-assert (eq failure rejected) "ordinary preparation rejection preserves the exact terminal condition")
       (test-assert (and (= transmissions 1) (equal (reverse projections) '("runtime-a" "runtime-b")))
                    "incompatible alternate is not transmitted, skipped or retried")))
   :threshold 1)
  nil)


;;;; -- Route Accounting --

(-> test-provider-route-cost-attribution () null)
(defun test-provider-route-cost-attribution ()
  "Price the successful exact route, preserving exports, values and request isolation."
  (route-tests--call-with-runtime
   (lambda (provider conversation configuration)
     (let* ((*provider-cost-ledger*         (make-cost-ledger))
            (*llm-usage-events-file*        (merge-pathnames "route-usage.jsonl"
                                                           (uiop:pathname-directory-pathname
                                                            (configuration-preferences-path configuration))))
            (*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds))))
            (attempts                       0)
            (records                        nil)
            (record                         (symbol-function 'provider-cost-record))
            (result                         (agent-test-result "route-priced" nil
                                                               :usage (json-object "input_tokens" 1000000 "output_tokens" 100000))))
       (setf (config :model-prices configuration)
             "{\"runtime-one\":{\"input\":1,\"output\":2},\"runtime-two\":{\"input\":7,\"output\":11}}")
       (test-call-with-function-replacements
        (list
         (list 'provider-cost-record
               (lambda (actual response)
                 (let ((entry (funcall record actual response)))
                   (push entry records)
                   entry)))
         (list 'cl-llm-provider-api::provider-execute-request
               (lambda (adapter request &key secrets &allow-other-keys)
                 (test-assert (member (format nil "owned-runtime-fixture-~A"
                                              (provider-account-label adapter)) secrets :test #'equal)
                              "selected route owns credentials")
                 (test-assert (string= (json-get request "model")
                                      (config :model (provider-configuration adapter)))
                              "actual ordinary projection uses selected model")
                 (if (= 1 (incf attempts))
                     (error 'provider-transport-error :message "Owned primary reset.")
                     (values result :extra)))))
        (lambda ()
          (test-assert (equal (multiple-value-list
                               (provider-stream-turn provider conversation :tool-namespaces #()
                                                     :event-callback                        #'identity))
                             (list result :extra))
                       "successful route keeps multiple results")
          (test-assert (= 81/10 (cost-ledger-total *provider-cost-ledger*))
                       "alternate model ordinary price, not primary price")
          (test-assert (and (= 1 (length records))
                            (string= "runtime-two" (getf (first records) :model))
                            (string= "runtime-b" (getf (first records) :route)))
                       "one invocation records the successful route once")
          (test-assert (string= "runtime-two"
                               (json-get (first (usage-export-tests--read *llm-usage-events-file*)) "model"))
                       "export names the actual successful model")
          (provider-stream-turn provider conversation :tool-namespaces #() :event-callback #'identity)
          (test-assert (= 93/10 (cost-ledger-total *provider-cost-ledger*))
                       "independent request starts primary without attribution leakage")
          (test-assert (string= "runtime-one" (getf (first records) :model))
                       "primary success is attributed independently")))))
   :threshold 1)
  nil)


;;;; -- Ordinary Cross-family Preparation --

(-> route-tests--call-with-families (function &key (:alternate-window (integer 1))) null)
(defun route-tests--call-with-families (function &key (alternate-window 200000))
  "Use actual Chat and Messages factories with private fixture keys and local metadata."
  (let ((snapshot (provider--registry-snapshot)))
    (unwind-protect
         (with-test-configuration (configuration)
           (register-provider
            "compat-chat" :family :openai :protocol :chat-completions
            :models   '((:name "compat-large" :context-window 1000000))
            :endpoint "http://127.0.0.1:1/chat"
            :factory  (lambda (copy &key reasoning-summaries-p)
                        (declare (ignore reasoning-summaries-p))
                        (let ((provider (openai-compatible-provider-create copy :name "compat-chat" :family :openai)))
                          (api-key-credential-manager-save-key (provider-credential-manager provider) "owned-chat-key")
                          provider)))
           (register-provider
            "compat-messages" :family :anthropic :protocol :messages
            :models   (list (list :name "claude-haiku-4-5" :context-window alternate-window))
            :endpoint "http://127.0.0.1:1/messages"
            :factory  (lambda (copy &key reasoning-summaries-p)
                        (declare (ignore reasoning-summaries-p))
                        (let ((provider (anthropic-provider-create copy)))
                          (api-key-credential-manager-save-key (provider-credential-manager provider) "owned-messages-key")
                          provider)))
           (let* ((copy         (configuration-copy configuration :model-provider "compat-chat" :model "compat-large"))
                  (conversation (conversation-create copy)))
             (setf (config :provider-fallback-threshold copy) 1
                   (config :provider-fallback-mappings copy)
                   "[[[\"compat-chat\",\"compat-large\"],[\"compat-messages\",\"claude-haiku-4-5\"]]]")
             (funcall function (provider-create copy) conversation copy)))
      (provider--registry-restore snapshot)))
  nil)

(-> route-tests--messages-stream () string)
(defun route-tests--messages-stream ()
  "Return a valid local Messages stream with paid usage and a successful result."
  (with-output-to-string (stream)
    (dolist (event
             (list
              (json-object "type" "message_start" "message"
                           (json-object "id" "owned-messages-response" "type" "message" "role" "assistant"
                                        "model" "claude-haiku-4-5" "content" #()
                                        "usage" (json-object "input_tokens" 1000000 "output_tokens" 0)))
              (json-object "type" "content_block_start" "index" 0
                           "content_block" (json-object "type" "text" "text" ""))
              (json-object "type" "content_block_delta" "index" 0
                           "delta" (json-object "type" "text_delta" "text" "Owned alternate answer."))
              (json-object "type" "content_block_stop" "index" 0)
              (json-object "type" "message_delta" "delta" (json-object "stop_reason" "end_turn")
                           "usage" (json-object "output_tokens" 100000))
              (json-object "type" "message_stop")))
      (write-string (test-sse-event-string event) stream))))

(-> test-provider-route-cross-family-complete-path () null)
(defun test-provider-route-cross-family-complete-path ()
  "Open ordinary projected Chat and Messages requests, consume SSE, and price the alternate."
  (route-tests--call-with-families
   (lambda (provider conversation configuration)
     (declare (ignore configuration))
     (let ((*provider-cost-ledger*           (make-cost-ledger))
           (*bounded-retry-sleep-function*   (lambda (seconds) (declare (ignore seconds))))
           (*provider-maximum-output-tokens* 512)
           (bodies nil) (headers-seen nil) (streams nil) (events nil))
       (conversation-append-provider-item
        conversation (json-object "type" "message" "role" "user" "content"
                                  (json-array (json-object "type" "input_text" "text" "Inspect this image.")
                                              (json-object "type" "input_image"
                                                           "image_url" "data:image/png;base64,aGVsbG8="))))
       (conversation-append-provider-item
        conversation (agent-test-call :call-id "owned-call-17" :namespace "search" :name "content"
                                     :arguments "{\"query\":\"owned needle\"}"))
       (conversation-append-provider-item
        conversation (json-object "type" "function_call_output" "call_id" "owned-call-17" "output" "owned tool result"))
       (conversation-append-provider-item conversation (agent-test-message "Prior ordinary answer."))
       (conversation-append-user-message conversation "Continue with the same tool result and image.")
       (test-call-with-function-replacements
        (list (list 'provider-post-event-stream
                    (lambda (url body &key headers &allow-other-keys)
                      (declare (ignore url))
                      (push (json-decode (babel:octets-to-string body :encoding :utf-8)) bodies)
                      (push headers headers-seen)
                      (if (= (length bodies) 1)
                          (error 'provider-transport-error :message "Owned primary refused.")
                          (let ((stream (make-string-input-stream (route-tests--messages-stream))))
                            (push stream streams)
                            (values stream 200 nil))))))
        (lambda ()
          (let* ((schemas (provider-tests--search-filter-schemas))
                 (result  (provider-stream-turn provider conversation :tool-namespaces schemas
                                                :event-callback                        (lambda (event) (push event events))))
                 (messages (first bodies)) (chat (second bodies))
                 (encoded (json-encode messages)))
            (test-assert (= (length bodies) 2) "only primary failure then alternate success is transmitted")
            (test-assert (and (json-get chat "messages") (json-get chat "stream_options")
                              (json-get messages "system") (null (json-get messages "stream_options")))
                         "each family uses its own ordinary wire body")
            (dolist (text '("owned-call-17" "owned tool result" "Prior ordinary answer."
                            "Continue with the same tool result and image." "aGVsbG8=" "search__content"))
              (test-assert (search text encoded) "alternate preserves portable conversation, image and tool semantics"))
            (test-assert (and (= (json-get messages "max_tokens") 512)
                              (string= (json-get messages "model") "claude-haiku-4-5"))
                         "alternate has its own model and ordinary output cap")
            (test-assert (equal (rest (assoc "x-api-key" (first headers-seen) :test #'string-equal)) "owned-messages-key")
                         "alternate transmission uses its own ordinary credentials")
            (test-assert (search "owned-chat-key" (or (rest (assoc "Authorization" (second headers-seen)
                                                                 :test #'string-equal)) ""))
                         "primary transmission uses its own ordinary credentials")
            (test-assert (and (string= "owned-messages-response" (provider-result-response-id result))
                              (every (lambda (stream) (not (open-stream-p stream))) streams))
                         "ordinary stream result succeeds and transport cleanup completes")
            (test-assert (= 3/2 (cost-ledger-total *provider-cost-ledger*))
                         "successful genuinely different model uses ordinary Anthropic list price")
            (test-assert (find-if (lambda (event) (typep event 'provider-response-metadata-event)) events)
                         "ordinary metadata notifications are preserved")))))))
  nil)

(-> test-provider-route-context-output-incompatibility () null)
(defun test-provider-route-context-output-incompatibility ()
  "Reject actual alternate context/output overflow before its transmission."
  (dolist (mode '(:context :output))
    (route-tests--call-with-families
     (lambda (provider conversation configuration)
       (declare (ignore configuration))
       (let ((*bounded-retry-sleep-function*   (lambda (seconds) (declare (ignore seconds))))
             (*provider-maximum-output-tokens* (if (eq mode :output) 30000 128))
             (transmissions 0) (failure nil))
         (conversation-append-user-message conversation
                                           (if (eq mode :context) (make-string 100000 :initial-element #\x) "Small prompt."))
         (test-call-with-function-replacements
          (list (list 'provider-post-event-stream
                      (lambda (&rest arguments)
                        (declare (ignore arguments))
                        (incf transmissions)
                        (error 'provider-transport-error :message "Owned primary refused."))))
          (lambda ()
            (handler-case
                (provider-stream-turn provider conversation :tool-namespaces #() :event-callback #'identity)
              (error (condition) (setf failure condition)))))
         (test-assert (typep failure 'provider-context-budget-error) "ordinary actual final encoded alternate is incompatible")
         (test-assert (= 1 transmissions) "context and explicit output overflow are terminal before alternate transmission")))
     :alternate-window 20000))
  nil)

(-> test-provider-route-real-preparation-rejection () null)
(defun test-provider-route-real-preparation-rejection ()
  "Reject an actual unrepresentable image source without discarding semantic content."
  (route-tests--call-with-families
   (lambda (provider conversation configuration)
     (declare (ignore configuration))
     (let ((*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds))))
           (transmissions 0) (failure nil))
       (conversation-append-user-message conversation "Keep this semantic content.")
       (conversation-append-provider-item
        conversation (json-object "type" "message" "role" "user" "content"
                                  (json-array (json-object "type" "input_image" "image_url" "file:///owned-fixture.png"))))
       (test-call-with-function-replacements
        (list (list 'provider-post-event-stream
                    (lambda (&rest arguments)
                      (declare (ignore arguments))
                      (incf transmissions)
                      (error 'provider-transport-error :message "Owned primary refused."))))
        (lambda ()
          (handler-case
              (provider-stream-turn provider conversation :tool-namespaces #() :event-callback #'identity)
            (error (condition) (setf failure condition)))))
       (test-assert (and (typep failure 'provider-error)
                         (string= (provider-error-code failure) "unsupported_content"))
                    "ordinary Messages serializer rejects actual unrepresentable content")
       (test-assert (= transmissions 1) "terminal incompatibility is detected before alternate transmission"))))
  nil)

(-> test-provider-route-mission-accounting () null)
(defun test-provider-route-mission-accounting ()
  "Keep one admission and settlement over fallback, including terminal observed-usage uncertainty."
  (dolist (mode '(:success :unknown :terminal))
    (route-tests--call-with-runtime
     (lambda (provider unused configuration)
       (declare (ignore unused))
       (let* ((application                    (mission-inference-test--application configuration provider))
              (conversation                   (application-conversation application))
              (*provider-cost-ledger*         (make-cost-ledger))
              (*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds))))
              (attempts 0) (metadata-seen nil) (failure nil) (result nil))
         (application-mission-start application (mission-test--specification :turns 1 :tokens 100))
         (let* ((context (mission-context-find conversation))
                (goal    (mission-context-goal context)))
           (test-call-with-function-replacements
            (list (list 'cl-llm-provider-api::provider-execute-request
                        (lambda (adapter request &key event-callback &allow-other-keys)
                          (declare (ignore adapter request))
                          (incf attempts)
                          (test-assert (and (= (getf goal :turns-used) 1)
                                            (= (getf goal :requests-outstanding) 1))
                                       "all socket attempts share the one ordinary reservation")
                          (when (= attempts 1)
                            (error 'provider-transport-error :message "Owned primary reset."))
                          (funcall event-callback
                                   (make-instance 'provider-response-metadata-event
                                                  :metadata (make-instance 'provider-response-metadata
                                                                           :model            "runtime-two"
                                                                           :usage-observed-p t)))
                          (if (eq mode :terminal)
                              (error 'provider-error :message "Owned terminal stream failure." :code "invalid_request_error")
                              (make-instance 'provider-result :response-id "owned-mission-route"
                                             :output-items nil :tool-calls nil :turn-state nil
                                             :usage                        (when (eq mode :success)
                                                                             (json-object "input_tokens" 2 "output_tokens" 3)))))))
            (lambda ()
              (handler-case
                  (setf result
                        (provider-stream-turn provider conversation :tool-namespaces #()
                                              :event-callback                        (lambda (event)
                                                                                       (when (typep event 'provider-response-metadata-event)
                                                                                         (setf metadata-seen t)))))
                (error (condition) (setf failure condition)))
              (test-assert (and (= attempts 2) metadata-seen) "alternate metadata is delivered before terminal outcome")
              (test-assert (and (= 1 (getf goal :turns-used))
                                (zerop (getf goal :requests-outstanding))
                                (zerop (getf goal :tokens-reserved)))
                           "logical invocation settles once and clears its reservation")
              (if (eq mode :success)
                  (test-assert (and result (null failure) (= 5 (getf goal :tokens-used))
                                    (zerop (getf goal :unknown-usage)))
                               "ordinary successful alternate usage is charged once")
                  (test-assert (and (plusp (getf goal :unknown-usage))
                                    (mission--unknown-usage-blocks-p goal)
                                    (if (eq mode :terminal) (and failure (null result)) (and result (null failure))))
                               "observed metadata is not invented as exact usage; missing usage quarantines"))
              (let ((before attempts))
                (test-assert (handler-case
                                 (progn (mission--admit context :new-work-p t) nil)
                               (mission-error () t))
                             "exhausted or quarantined admission refuses another request")
                (test-assert (= before attempts) "refused admission performs no further route dispatch")))))))
     :threshold 1))
  nil)

(-> test-provider-route-terminal-stream-usage () null)
(defun test-provider-route-terminal-stream-usage ()
  "Retain actual preterminal usage metadata and quarantine failed native-stream accounting."
  (route-tests--call-with-families
   (lambda (provider unused configuration)
     (declare (ignore unused))
     (let* ((application                    (mission-inference-test--application configuration provider))
            (conversation                   (application-conversation application))
            (*provider-cost-ledger*         (make-cost-ledger))
            (*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds))))
            (transmissions 0) (events nil) (streams nil) (failure nil))
       (application-mission-start application (mission-test--specification :turns 2 :tokens 100))
       (test-call-with-function-replacements
        (list (list 'provider-post-event-stream
                    (lambda (&rest arguments)
                      (declare (ignore arguments))
                      (if (= (incf transmissions) 1)
                          (error 'provider-transport-error :message "Owned primary refused.")
                          (let ((stream
                                  (make-string-input-stream
                                   (concatenate
                                    'string
                                    (test-sse-event-string
                                     (json-object "type" "message_start" "message"
                                                  (json-object "id" "owned-preterminal" "type" "message"
                                                               "role" "assistant" "model" "claude-haiku-4-5" "content" #()
                                                               "usage" (json-object "input_tokens" 7 "output_tokens" 0))))
                                    (test-sse-event-string
                                     (json-object "type" "error" "error"
                                                  (json-object "type" "invalid_request_error" "message" "Owned terminal refusal.")))))))
                            (push stream streams)
                            (values stream 200 nil))))))
        (lambda ()
          (handler-case
              (provider-stream-turn provider conversation :tool-namespaces #()
                                    :event-callback                        (lambda (event) (push event events)))
            (error (condition) (setf failure condition)))))
       (let ((goal (mission-context-goal (mission-context-find conversation))))
         (test-assert (and failure (= transmissions 2)) "native terminal response is not retried or routed again")
         (test-assert (find-if
                       (lambda (event)
                         (and (typep event 'provider-response-metadata-event)
                              (cl-llm-provider-api:provider-response-metadata-usage-observed-p
                               (provider-response-metadata-event-metadata event)))) events)
                      "actual stream usage metadata is delivered before its terminal error")
         (test-assert (notany (lambda (event) (typep event 'assistant-delta-event)) events)
                      "this witness has usage but no terminal output")
         (test-assert (and (= 1 (getf goal :unknown-usage))
                           (= 1 (getf goal :turns-used))
                           (zerop (getf goal :requests-outstanding))
                           (zerop (getf goal :tokens-reserved)))
                      "failed complete invocation settles once as unknown, not free or a reported total")
         (test-assert (zerop (cost-ledger-total *provider-cost-ledger*))
                      "the successful-request ledger does not fabricate a completed charge")
         (test-assert (every (lambda (stream) (not (open-stream-p stream))) streams)
                      "terminal ordinary native stream cleanup completes")))))
  nil)


;;;; -- Suspended Invocation Admission --

(-> test-provider-operation-agent-resume () null)
(defun test-provider-operation-agent-resume ()
  "Rebuild an interrupted ordinary request with current history and no duplicate user input."
  (route-tests--call-with-runtime
   (lambda (provider conversation configuration)
     (let* ((agent                            (agent-create :configuration configuration :conversation conversation
                                                           :provider provider :tool-registry (make-instance 'tool-registry)))
            (operation                        (make-provider-operation :conversation conversation :goal conversation :epoch 0
                                                                       :request-number 1))
            (owner                            (make-provider-operation-owner :operation operation))
            (*provider-suspended-connections* (make-hash-table :test 'eq))
            (state                            (make-provider-connection-recovery))
            (seen                             nil))
       (setf (provider-operation-owner-scope operation) owner
             (provider-connection-recovery-owner state) conversation
             (provider-connection-recovery-owner-generation state) 0
             (provider-connection-recovery-attempt state) 3
             (provider-connection-recovery-route-index state) 1)
       (conversation-append-user-message conversation "Original ordinary question.")
       (let ((*provider-connection-operation* operation))
         (provider--remember-suspension conversation state configuration))
       (conversation-append-user-message conversation "Current durable steering.")
       (test-call-with-function-replacements
        (list (list 'cl-llm-provider-api::provider-execute-request
                    (lambda (adapter request &rest arguments)
                      (declare (ignore arguments))
                      (push (json-encode request) seen)
                      (test-assert (equal (provider-account-label adapter) "runtime-b") "resume restores the exact retired route")
                      (mission-test--result "Reconstructed result."))))
        (lambda ()
          (provider-call-with-operation-owner
           owner (lambda () (agent-resume-provider-operation agent operation :owner owner)))))
       (test-assert (= 1 (length seen)) "one restored request transmits")
       (test-assert (search "Current durable steering." (first seen)) "resume prepares current durable history")
       (test-assert (= 2 (count-if (lambda (item) (json-string= (json-get item "role") "user"))
                                   (conversation-input-items conversation))) "resume appends no fresh user message")
       (test-assert (eq (provider-operation-state operation) ':disposed) "terminal outer scope disposes the operation")
       (test-assert (zerop (hash-table-count *provider-suspended-connections*)) "terminal dispatch consumes retry metadata"))))
  nil)

(-> test-provider-operation-single-consumer () null)
(defun test-provider-operation-single-consumer ()
  "Race identical explicit tokens at the real public provider boundary with modeled authority."
  (route-tests--call-with-runtime
   (lambda (provider conversation configuration)
     (let* ((goal      (list :fixture-goal))
            (operation (make-provider-operation :conversation conversation :goal goal :epoch 0))
            (owner     (make-provider-operation-owner :operation operation :active-p t))
            (table     (make-hash-table :test 'eq))
            (arrived   (bordeaux-threads:make-semaphore))
            (release   (bordeaux-threads:make-semaphore))
            (lock      (make-lock "Same token transmissions"))
            (transmissions 0) (cells nil))
       (setf (provider-operation-owner-scope operation) owner)
       (let ((*provider-connection-operation* operation) (*provider-suspended-connections* table)
             (state (make-provider-connection-recovery)))
         (setf (provider-connection-recovery-owner state) goal
               (provider-connection-recovery-owner-generation state) 0
               (provider-connection-recovery-attempt state) 3
               (provider-connection-recovery-route-index state) 1)
         (provider--remember-suspension conversation state configuration))
       (unwind-protect
            (test-call-with-function-replacements
             (list (list 'cl-llm-provider-api::provider-execute-request
                         (lambda (adapter request &rest arguments)
                           (declare (ignore request arguments))
                           (with-lock-held (lock) (incf transmissions))
                           (test-assert (equal (provider-account-label adapter) "runtime-b") "winner uses exact retired history")
                           (mission-test--result "Single consumer."))))
             (lambda ()
               (dotimes (index 2)
                 (declare (ignorable index))
                 (push (mission-inference-test--start
                        (lambda ()
                          (let ((*provider-connection-operation*  operation)
                                (*provider-suspended-connections* table)
                                (*provider-connection-owner*      goal))
                            (bordeaux-threads:signal-semaphore arrived)
                            (unless (bordeaux-threads:wait-on-semaphore release :timeout 10)
                              (error "Same-token release was not received."))
                            (provider-stream-turn provider conversation :tool-namespaces #() :event-callback #'identity)))) cells))
               (dotimes (index 2)
                 (declare (ignorable index))
                 (test-assert (bordeaux-threads:wait-on-semaphore arrived :timeout 10) "both token consumers reach the barrier"))
               (mission-inference-test--join cells release 2)
               (test-assert (= 1 transmissions) "exactly one claim transmits; loser never falls back to fresh primary")
               (test-assert (= 1 (count-if (lambda (cell) (typep (mission-inference-test-thread-condition cell) 'provider-operation-refused)) cells))
                            "losing simultaneous consumer receives a typed refusal")
               (test-assert (= 1 (count-if (lambda (cell) (mission-inference-test-thread-values cell)) cells)) "one consumer returns a result")
               (test-assert (zerop (hash-table-count table)) "history is consumed once")))
         (dolist (cell cells)
           (bordeaux-threads:signal-semaphore release)
           (sb-thread:join-thread (mission-inference-test-thread-thread cell) :timeout 12 :default nil))
         (let ((*provider-suspended-connections* table)) (provider-operation-dispose operation))))))
  nil)

(-> test-provider-operation-terminal-guards () null)
(defun test-provider-operation-terminal-guards ()
  "Exercise exact-owner, deadline, configuration and one-consumer guards through public dispatch."
  (route-tests--call-with-runtime
   (lambda (provider conversation configuration)
     (declare (ignore provider))
     (let ((unrelated (make-instance 'mission-context :application nil :goal nil)))
       (test-call-with-function-replacements
        (list (list 'mission--conversation-context (lambda (value) (declare (ignore value)) unrelated))
              (list 'mission--request-owned-p (lambda (context value) (declare (ignore context value)) nil)))
        (lambda ()
          (mission-call-with-provider-operation
           conversation
           (lambda ()
             (test-assert (eq *provider-connection-owner* conversation)
                          "An unrelated mission does not own an ordinary request."))
           :configuration configuration))))))
  (dolist (mode '(:inactive-owner :no-owner :epoch :deadline :route :disposed :valid))
    (route-tests--call-with-runtime
     (lambda (provider conversation configuration)
       (let* ((goal                             (list :fixture-goal))
              (operation                        (make-provider-operation :conversation conversation :goal goal :epoch 0))
              (owner                            (make-provider-operation-owner :operation operation :active-p t))
              (other                            (make-provider-operation))
              (*provider-suspended-connections* (make-hash-table :test 'eq))
              (state                            (make-provider-connection-recovery))
              (transmissions nil) (failure nil))
         (setf (provider-operation-owner-scope operation) (unless (eq mode :no-owner) owner)
               (provider-operation-owner-active-p owner) (not (eq mode :inactive-owner))
               (provider-connection-recovery-owner state) goal
               (provider-connection-recovery-owner-generation state) 0
               (provider-connection-recovery-attempt state) 3
               (provider-connection-recovery-route-index state) 1
               (provider-connection-recovery-deadline state)
               (if (eq mode :deadline) -1 (+ 10 (/ (get-internal-real-time) internal-time-units-per-second)))
               (gethash other *provider-suspended-connections*) :other-operation)
         (let ((*provider-connection-operation* operation))
           (provider--remember-suspension conversation state configuration))
         (case mode
           (:epoch
            (provider-cancel-connection-owner goal))
           (:route
            (setf (config :provider-fallback-threshold configuration) 2))
           (:disposed
            (provider-operation-dispose operation)))
         (test-call-with-function-replacements
          (list (list 'cl-llm-provider-api::provider-execute-request
                      (lambda (adapter request &rest arguments)
                        (declare (ignore request arguments))
                        (push (provider-account-label adapter) transmissions)
                        (mission-test--result "Guarded current result."))))
          (lambda ()
            (let ((*provider-connection-operation* operation)
                  (*provider-connection-owner*     goal))
              (handler-case
                  (provider-stream-turn provider conversation :tool-namespaces #() :event-callback #'identity)
                (error (condition) (setf failure condition))))
            (if (eq mode :valid)
                (test-assert (and (null failure) (equal transmissions '("runtime-b"))) "eligible exact operation alone restores and transmits")
                (progn
                  (test-assert (typep failure '(or provider-error configuration-error)) "ineligible explicit resume is a typed refusal")
                  (test-assert (null transmissions) "expired, cancelled, changed or unauthorized operations never dispatch")))
            (provider-operation-dispose operation)
            (test-assert (and (= 1 (hash-table-count *provider-suspended-connections*))
                              (eq :other-operation (gethash other *provider-suspended-connections*)))
                         "terminal disposal removes only its own descriptor history")
            (setf transmissions nil)
            (provider-stream-turn provider conversation :tool-namespaces #() :event-callback #'identity)
            (test-assert (equal transmissions '("runtime-a")) "new work after disposal starts primary")))))))
  nil)
