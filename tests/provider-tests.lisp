(in-package #:autolith)

;;;; -- Subsystem Tests --

(-> test-provider-rate-limits () null)
(defun test-provider-rate-limits ()
  "Test rate limit header parsing into portable snapshots."
  (let ((snapshot (provider-rate-limit-snapshot
                   '(("x-codex-primary-used-percent" . "28.5")
                     ("x-codex-primary-window-minutes" . "300")
                     ("x-codex-primary-reset-at" . "1783000000")
                     ("x-codex-secondary-used-percent" . "45")
                     ("x-codex-secondary-window-minutes" . "10080")))))
    (test-assert (= (getf (getf snapshot :primary) :window-minutes) 300)
                 "primary rate limit windows parse their minutes")
    (test-assert (= (round (* 10 (getf (getf snapshot :primary)
                                       :used-percent)))
                    285)
                 "decimal used percents parse without the Lisp reader")
    (test-assert (= (getf (getf snapshot :primary) :resets-at)
                    (unix-time->universal-time 1783000000))
                 "reset times convert from the POSIX epoch")
    (test-assert (= (getf (getf snapshot :secondary) :used-percent) 45)
                 "secondary rate limit windows parse")
    (test-assert (null (getf (getf snapshot :secondary) :resets-at))
                 "missing reset headers stay absent"))
  (test-assert (null (provider-rate-limit-snapshot
                      '(("content-type" . "text/event-stream"))))
               "absent rate limit headers produce no snapshot")
  (test-assert
   (search "model_not_found means this model is unavailable"
           (provider--http-error-message
            404
            "{\"error\":{\"message\":\"model_not_found means this model is unavailable\"}}"))
   "HTTP errors surface the provider's own explanation")
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (provider      (provider-create configuration))
         (condition
           (make-condition
            'http-request-failed
            :body    "rate limited"
            :status  429
            :headers '(("x-request-id" . "request-429")
                       ("x-codex-primary-used-percent" . "100")
                       ("x-codex-primary-window-minutes" . "300"))
            :uri     nil
            :method  ':post)))
    (unwind-protect
         (progn
           (test-assert
            (handler-case
                (progn
                  (provider-signal-http-failure provider condition)
                  nil)
              (provider-retryable-error (error)
                (and (= (provider-error-status error) 429)
                     (string= (provider-error-request-id error)
                              "request-429"))))
            "HTTP 429 is eligible for bounded retry")
           (test-assert
            (and (handler-case
                     (progn
                       (provider--signal-http-status-failure
                        provider 408
                        :headers '(("Retry-After" . "3")))
                       nil)
                   (provider-retryable-error (error)
                     (= (provider-error-status error) 408)))
                 (not (provider-retryable-status-p provider 408 nil)))
            "Retry-After makes an otherwise terminal status retryable")
           (test-assert
            (= (getf (getf (provider-rate-limits provider) :primary)
                     :used-percent)
               100)
            "HTTP error headers refresh the visible rate limit snapshot")
           (let ((stream-condition
                   (make-condition
                    'http-request-failed
                    :body    (make-string-input-stream
                              "{\"error\":{\"message\":\"input item is not supported\"}}")
                    :status  400
                    :headers '(("request-id" . "request-400"))
                    :uri     nil
                    :method  ':post)))
             (test-assert
              (handler-case
                  (progn
                    (provider-signal-http-failure provider stream-condition)
                    nil)
                (provider-error (error)
                  (and (= (provider-error-status error) 400)
                       (string= (provider-error-request-id error)
                                "request-400")
                       (search "input item is not supported"
                               (format nil "~A" error))
                       (search "input item is not supported"
                               (or (provider-error-response error) ""))
                       t)))
              "a streamed failure body reaches both the message and the response"))
           (dolist (status '(500 503))
             (let ((transient-condition
                     (make-condition
                      'http-request-failed
                      :body    "temporary provider failure"
                      :status  status
                      :headers nil
                      :uri     nil
                      :method  ':post)))
               (test-assert
                (handler-case
                    (progn
                      (provider-signal-http-failure provider transient-condition)
                      nil)
                  (provider-retryable-error (error)
                    (= (provider-error-status error) status)))
                (format nil "HTTP ~D is eligible for bounded retry" status)))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> provider-tests--request-tools (json-object) vector)
(defun provider-tests--request-tools (request)
  "Return REQUEST's top-level standard Responses tools."
  (json-get request "tools"))

(-> provider-tests--request-tool-of-type
    (json-object string) (option json-object))
(defun provider-tests--request-tool-of-type (request type)
  "Return REQUEST's first standard Responses tool whose type equals TYPE."
  (find type
        (provider-tests--request-tools request)
        :key  (lambda (tool)
                (and (json-object-p tool)
                     (json-get tool "type")))
        :test #'equal))

(-> provider-tests--search-filter-schemas () vector)
(defun provider-tests--search-filter-schemas ()
  "Return one ordinary namespace and the local web namespace."
  (json-array
   (json-object
    "type" "namespace"
    "name" "resource"
    "tools" (json-array
             (json-object "name" "read"
                          "description" "Read a resource."
                          "parameters" (json-object "type" "object"))))
   (json-object
    "type" "namespace"
    "name" "web"
    "tools" (json-array
             (json-object "name" "run"
                          "description" "Search the web."
                          "parameters" (json-object "type" "object"))
             (json-object "name" "gist"
                          "description" "Retrieve one page as Markdown."
                          "parameters" (json-object "type" "object"))))))

(-> test-provider-reserved-web-schema () null)
(defun test-provider-reserved-web-schema ()
  "Exercise reserved web parameters through registered eager and deferred requests."
  (with-test-configuration (configuration)
    (let* ((registry     (make-default-tool-registry :configuration configuration))
           (conversation (conversation-create configuration)))
      (unwind-protect
           (dolist (model '("gpt-5.3-codex" "gpt-6.1-sol"))
             (let* ((provider
                      (provider-family-create
                       ':codex
                       (configuration-copy configuration :model   model
                                           :provider-validation-p nil)))
                    (request
                      (provider-request-object
                       provider conversation (tool-registry-provider-schemas registry)))
                    (tools (provider-tests--request-tools request))
                    (web
                      (if (provider-deferred-tool-loading-p provider)
                          (let ((namespace
                                  (find "web" tools
                                        :key  (lambda (entry) (json-get entry "name"))
                                        :test #'equal)))
                            (and namespace
                                 (find "run" (json-get namespace "tools")
                                       :key  (lambda (entry) (json-get entry "name"))
                                       :test #'equal)))
                          (find (provider-wire-tool-name provider "web" "run") tools
                                :key  (lambda (entry) (json-get entry "name"))
                                :test #'equal))))
               (test-assert
                (and web (equalp (json-get web "parameters") (web-run-parameters)))
                "The provider-reserved web schema reaches the request without client metadata.")))
        (tool-registry-close-runtime-state registry))))
  nil)

(-> test-provider-tool-choice () null)
(defun test-provider-tool-choice ()
  "Disable tool calls on every wire format while keeping the same tool definitions."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry      (make-default-tool-registry :configuration configuration))
         (conversation  (conversation-create configuration :identifier "tool-choice")))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "Answer now.")
           (loop for (label provider choice-of) in
                 (list (list "Responses" (provider-create configuration)
                             (lambda (request) (json-get request "tool_choice")))
                       (list "Chat Completions"
                             (openai-compatible-provider-create
                              configuration :name                "choice-openai"
                                            :family              ':openai
                                            :headers             nil
                                            :reasoning-parameter nil
                                            :stream-usage-p      t)
                             (lambda (request) (json-get request "tool_choice")))
                       (list "Messages" (anthropic-provider-create configuration)
                             (lambda (request)
                               (json-get (json-get request "tool_choice") "type")))
                       (list "GenerateContent"
                             (gemini-code-assist-provider-create
                              configuration
                              :credential-manager (credential-manager-create configuration))
                             (lambda (request)
                               (let ((tool-configuration
                                       (json-get (json-get request "request") "toolConfig")))
                                 (if tool-configuration
                                     (string-downcase
                                      (json-get (json-get tool-configuration
                                                          "functionCallingConfig")
                                                "mode"))
                                     "auto")))))
                 do (flet ((encode (choice)
                             "Encode PROVIDER's request with tool CHOICE."
                             (let ((*provider-tool-choice* choice))
                               (provider-request-object
                                provider conversation
                                (tool-registry-provider-schemas registry)))))
                      (let ((automatic (encode ':auto))
                            (disabled  (encode ':none)))
                        (test-assert
                         (and (equal (funcall choice-of automatic) "auto")
                              (equal (funcall choice-of disabled) "none"))
                         (format nil "~A requests carry the bound tool choice" label))
                        (test-assert
                         (string= (json-encode (json-get (or (json-get disabled "request")
                                                             disabled)
                                                         "tools"))
                                  (json-encode (json-get (or (json-get automatic "request")
                                                             automatic)
                                                         "tools")))
                         (format nil "~A requests keep their tool definitions" label))))))
      (tool-registry-close-runtime-state registry)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-provider-prompt-cache-ttl () null)
(defun test-provider-prompt-cache-ttl ()
  "Request the configured cache lifetime on Claude routes and nowhere else."
  (with-test-environment (("AUTOLITH_OPENROUTER_PROVIDER_ROUTING" nil))
    (with-test-configuration (configuration)
      (setf (configuration-validation-deferred-p configuration) t)
      (let ((conversation (conversation-create configuration :identifier "cache-ttl")))
        (conversation-append-user-message conversation "Cache this history.")
        (labels ((controls (value)
                   "Return every cache_control object inside the JSON VALUE."
                   (cond
                     ((json-object-p value)
                      (append (let ((control (json-get value "cache_control")))
                                (and control (list control)))
                              (loop for child being the hash-values of value
                                    append (controls child))))
                     ((and (vectorp value) (not (stringp value)))
                      (loop for child across value
                            append (controls child)))
                     (t
                      nil)))

                 (request (route model ttl)
                   "Encode a request through ROUTE to MODEL with cache lifetime TTL."
                   (setf (config :prompt-cache-ttl configuration) ttl
                         (config :model configuration)
                         (if (eq route ':openrouter) (openrouter--model-name model) model))
                   (provider-request-object (if (eq route ':openrouter)
                                                (openrouter-provider-create configuration)
                                                (anthropic-provider-create configuration))
                                            conversation (json-array))))
          (dolist (case '((:anthropic  "claude-haiku-4-5"           "5m" (nil))
                          (:anthropic  "claude-haiku-4-5"           "1h" ("1h"))
                          (:openrouter "anthropic/claude-haiku-4.5" "5m" (nil))
                          (:openrouter "anthropic/claude-haiku-4.5" "1h" ("1h"))
                          (:openrouter "openai/gpt-5.1"             "1h" (nil))))
            (destructuring-bind (route model ttl expected) case
              (test-assert (equal expected
                                  (remove-duplicates
                                   (mapcar (lambda (control) (json-get control "ttl"))
                                           (controls (request route model ttl)))
                                   :test #'equal))
                           (format nil "~A ~A markers request lifetime ~A" route model ttl))))
          (test-assert (equal (conversation-prompt-cache-key conversation)
                              (json-get (request ':openrouter "anthropic/claude-haiku-4.5" "5m")
                                        "session_id"))
                       "OpenRouter requests carry the conversation's session identifier")))))
  nil)

(-> test-provider-shared-items-breakpoint () null)
(defun test-provider-shared-items-breakpoint ()
  "End shared leading items at their own cache marker on Claude routes."
  (with-test-environment (("AUTOLITH_OPENROUTER_PROVIDER_ROUTING" nil))
    (with-test-configuration (configuration)
      (setf (configuration-validation-deferred-p configuration) t)
      (let ((conversation (conversation-create configuration :identifier "shared-items")))
        (conversation-append-user-message conversation "Shared views.")
        (conversation-append-user-message conversation "Task: one question.")
        (flet ((breakpoints (route count)
                 "Return the cache marker paths of a request through ROUTE sharing COUNT items."
                 (setf (config :model configuration)
                       (if (eq route ':openrouter)
                           (openrouter--model-name "anthropic/claude-haiku-4.5")
                           "claude-haiku-4-5"))
                 (let ((*provider-shared-item-count* count))
                   (mapcar #'first
                           (getf (provider-cache-wire-plan
                                  (provider-request-object
                                   (if (eq route ':openrouter)
                                       (openrouter-provider-create configuration)
                                       (anthropic-provider-create configuration))
                                   conversation (json-array)))
                                 :breakpoints)))))
          (test-assert (and (member '("messages" 0 "content" 0) (breakpoints ':anthropic 1)
                                    :test #'equal)
                            (not (member '("messages" 0 "content" 0) (breakpoints ':anthropic 0)
                                         :test #'equal)))
                       "Messages marks the shared block inside the merged user message")
          (test-assert (and (member '("messages" 1 "content" 0) (breakpoints ':openrouter 1)
                                    :test #'equal)
                            (not (member '("messages" 1 "content" 0) (breakpoints ':openrouter 0)
                                         :test #'equal)))
                       "Chat marks the end of the shared message")
          (conversation-append-provider-item
           conversation
           (json-object "type" "function_call" "namespace" "skill" "name" "load"
                        "call_id" "volatile-call" "arguments" "{}")
           :persistence ':next-response)
          (conversation-append-tool-result conversation "volatile-call"
                                           :tool-name   "skill.load"
                                           :output      "Blocked."
                                           :success-p   nil
                                           :persistence ':next-response)
          (test-assert (member '("messages" 1 "content" 0) (breakpoints ':openrouter 1)
                               :test #'equal)
                       "Chat keeps the shared marker beside one-response exchanges")))))
  nil)

(defclass cost-test-provider (subscription-provider)
  ((usage
    :initarg       :usage
    :reader        cost-test-provider-usage
    :documentation "The usage every response reports."))
  (:documentation "A configured provider answering every request with fixed usage."))

(defmethod provider-stream-turn
    ((provider cost-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Answer with the provider's fixed usage."
  (declare (ignore tool-namespaces event-callback goal-context compaction-p))
  (agent-test-result (make-identifier) (list (agent-test-message "priced"))
                     :usage (cost-test-provider-usage provider)))

(-> test-provider-cost () null)
(defun test-provider-cost ()
  "Price requests from settings or list prices, count unpriced ones, and enforce the limit."
  (with-test-configuration (configuration)
    (setf (configuration-validation-deferred-p configuration) t)
    (let* ((*provider-cost-ledger* (make-cost-ledger))
           (conversation           (conversation-create configuration :identifier "cost"))
           (provider
             (make-instance 'cost-test-provider
                            :configuration configuration
                            :usage         (json-object "input_tokens"        1000000
                                                        "cached_input_tokens" 500000
                                                        "output_tokens"       100000))))
      (flet ((list-price ()
               "Return the direct Anthropic route's price for the configured model."
               (provider-model-price (anthropic-provider-create configuration) configuration)))
        (setf (config :model configuration) "claude-opus-5-5")
        (test-assert (and (= (model-price-input (list-price)) 4)
                          (= (model-price-cache-write (list-price)) 5))
                     "Anthropic list prices charge five-minute cache writes at 1.25 times input")
        (setf (config :prompt-cache-ttl configuration) "1h")
        (test-assert (= (model-price-cache-write (list-price)) 8)
                     "a one-hour cache lifetime doubles the write price"))
      (setf (config :model configuration) "local-model")
      (provider-stream-turn provider conversation)
      (test-assert (equal (getf (cost-ledger-summary *provider-cost-ledger*) :counts)
                          '(:reported 0 :estimated 0 :unpriced 1 :unknown 0))
                   "a request without a price is counted as unpriced")
      (setf (config :model-prices configuration)
            "{\"local-model\": {\"input\": 2, \"cache_read\": 0.2, \"output\": 10}}")
      (provider-stream-turn provider conversation)
      (test-assert (= (cost-ledger-total *provider-cost-ledger*) 21/10)
                   "configured prices estimate each token category")
      (test-assert (search "$2.1000 this process, 1 request unpriced"
                           (application--cost-description configuration))
                   "status reports spending next to unpriced requests")
      (setf (config :cost-limit configuration) 2)
      (test-assert (handler-case (progn (provider-stream-turn provider conversation) nil)
                     (cost-limit-reached ()
                       t))
                   "requests past the cost limit are refused")
      (test-assert (= (cost-ledger-total *provider-cost-ledger*) 21/10)
                   "a refused request records nothing")))
  nil)

(-> test-provider-request-tool-filtering () null)
(defun test-provider-request-tool-filtering ()
  "Test local web schemas are sent only when the request can execute them."
  (let* ((base-configuration (test-configuration))
         (root               (test-configuration-root base-configuration))
         (disabled-configuration
           (configuration-copy base-configuration :web-search-mode "disabled"))
         (schemas            (provider-tests--search-filter-schemas)))
    (unwind-protect
         (progn
           (let ((filtered
                   (provider-request-tool-namespaces
                    base-configuration schemas :hosted-web-search-p t))
                 (web (find-if
                       (lambda (entry)
                         (json-string= (json-get entry "name") "web"))
                       (coerce schemas 'list))))
             (test-assert
              (= (length filtered) 2)
              "hosted search suppresses only the provider-backed web.run tool")
             (test-assert
              (and web
                   (= (length (json-get web "tools")) 2))
              "the unfiltered fixture advertises both web tools")
             (let ((entry (find-if
                           (lambda (candidate)
                             (json-string= (json-get candidate "name") "web"))
                           (coerce filtered 'list))))
               (test-assert
                (and entry
                     (= (length (json-get entry "tools")) 1)
                     (json-string=
                      (json-get (aref (json-get entry "tools") 0) "name")
                      "gist"))
                "hosted search keeps the independent web_extra.gist tool")))
           (test-assert
            (and (provider-hosted-web-search-tools-p
                  (list (json-object "type" "web_search_preview")))
                 (not (provider-hosted-web-search-tools-p
                       (list (json-object "type" "code_interpreter")))))
            "only actual hosted search declarations suppress local web tools")
           (let* ((conversation
                    (conversation-create disabled-configuration
                                         :identifier "responses-search-filter"))
                  (provider  (provider-create disabled-configuration))
                  (request
                    (progn
                      (conversation-append-user-message conversation "inspect")
                      (provider-request-object provider conversation schemas)))
                  (tools     (json-get request "tools"))
                  (namespace (aref tools 0))
                  (web       (aref tools 1)))
             (test-assert
              (and (= (length tools) 3)
                   (json-string= (json-get namespace "type") "namespace")
                   (json-string= (json-get namespace "name") "resource")
                   (json-string= (json-get web "name") "web")
                   (= (length (json-get web "tools")) 1)
                   (json-string=
                    (json-get (aref (json-get web "tools") 0) "name")
                    "gist")
                   (json-string= (json-get (aref tools 2) "type")
                                 "tool_search"))
              "Responses keeps web_extra.gist but omits web.run when search is disabled"))
           (let* ((conversation
                    (conversation-create disabled-configuration
                                         :identifier "chat-search-filter"))
                  (provider
                    (openai-compatible-provider-create
                     disabled-configuration
                     :name                "search-filter"
                     :family              ':codex
                     :headers             nil
                     :reasoning-parameter nil))
                  (request
                    (progn
                      (conversation-append-user-message conversation "inspect")
                      (provider-request-object provider conversation schemas)))
                  (tools (json-get request "tools")))
             (test-assert
              (and (= (length tools) 2)
                   (string= (json-get
                             (json-get (aref tools 0) "function")
                             "description")
                            "Read a resource.")
                   (string= (json-get
                             (json-get (aref tools 1) "function")
                             "description")
                            "Retrieve one page as Markdown."))
              "Chat Completions keeps web_extra.gist but omits web.run when search is disabled"))
           (let* ((configuration
                    (configuration-copy
                     (configuration-copy
                      disabled-configuration :model "claude-haiku-4-5-20251001")
                     :web-search-mode "disabled"))
                  (conversation
                    (conversation-create configuration
                                         :identifier "anthropic-search-filter"))
                  (provider (anthropic-provider-create configuration))
                  (request
                    (progn
                      (conversation-append-user-message conversation "inspect")
                      (provider-request-object provider conversation schemas)))
                  (tools    (json-get request "tools")))
             (test-assert
              (and (= (length tools) 2)
                   (string= (json-get (aref tools 0) "description")
                            "Read a resource.")
                   (string= (json-get (aref tools 1) "description")
                            "Retrieve one page as Markdown."))
              "Anthropic keeps web_extra.gist but omits web.run when search is disabled"))
           (dolist (configuration (list base-configuration disabled-configuration))
             (let* ((filtered
                      (provider-responses-request-namespaces
                       (grok-provider-create configuration) schemas))
                    (web (find "web" filtered
                               :key  (lambda (entry) (json-get entry "name"))
                               :test #'equal)))
               (test-assert
                (and web
                     (= (length (json-get web "tools")) 1)
                     (json-string=
                      (json-get (aref (json-get web "tools") 0) "name") "gist"))
                "Grok advertises page retrieval with backend search enabled or disabled")))
           (let ((search-only
                   (json-array
                    (json-object "type" "namespace" "name" "web"
                                 "tools" (json-array (json-object "name" "run"))))))
             (test-assert
              (and (zerop (length (provider-request-tool-namespaces
                                   disabled-configuration search-only)))
                   (zerop (length (provider-responses-request-namespaces
                                   (grok-provider-create base-configuration)
                                   search-only))))
              "filtering the sole search tool omits the empty namespace")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-provider-deferred-tool-models () null)
(defun test-provider-deferred-tool-models ()
  "Test the deferred-tool capability gate over registered and hypothetical names."
  (dolist (registered-model
           (provider-registration-models (provider-registration-find "chatgpt")))
    (test-assert
     (provider-deferred-tool-model-p (provider-model-name registered-model))
     (format nil "registered Codex model ~A defers tool loading"
             (provider-model-name registered-model))))
  (dolist (entry '(("gpt-6.1-sol" . t)
                   ("gpt-6.1-terra" . t)
                   ("gpt-6-astra" . t)
                   ("gpt-6-sol" . t)
                   ("gpt-6-luna" . t)
                   ("gpt-6" . t)
                   ("gpt-7.2-nova" . t)
                   ("gpt-5.6-terra" . t)
                   ("gpt-5.4-codex" . t)
                   ("gpt-5.7-codex" . t)
                   ("gpt-5.3-codex" . nil)
                   ("gpt-5-codex" . nil)
                   ("gpt-5." . nil)
                   ("gpt-" . nil)
                   ("gpt-x-sol" . nil)
                   ("grok-4.5" . nil)
                   ("accounts/fireworks/models/kimi-k3" . nil)))
    (test-assert
     (eq (provider-deferred-tool-model-p (first entry)) (rest entry))
     (format nil "model ~A ~:[keeps eager tools~;defers tool loading~]"
             (first entry) (rest entry))))
  nil)

(-> test-provider-deferred-tool-loading () null)
(defun test-provider-deferred-tool-loading ()
  "Test Codex native namespace discovery and exact eager fallback."
  (let* ((base-configuration (test-configuration))
         (root               (test-configuration-root base-configuration))
         (schemas
           (json-array
            (json-object
             "type" "namespace"
             "name" "resource"
             "description" "Read and edit model-addressable resources."
             "tools"
             (json-array
              (json-object
               "name" "read"
               "description" "Read one resource."
               "parameters"
               (json-object
                "type" "object"
                "properties" (json-object
                              "uri" (json-object "type" "string")))))))))
    (unwind-protect
         (let* ((configuration
                  (configuration-copy base-configuration
                                        :model             "gpt-5.6-terra"
                                        :working-directory root))
                (provider  (provider-create configuration))
                (conversation
                  (conversation-create configuration
                                       :identifier "deferred-tools"))
                (request   (provider-request-object provider conversation schemas))
                (tools     (json-get request "tools"))
                (namespace (aref tools 0))
                (child     (aref (json-get namespace "tools") 0))
                (call      (json-object "type" "function_call"
                                        "call_id" "call-1"
                                        "namespace" "resource"
                                        "name" "read"
                                        "arguments" "{}")))
           (test-assert
            (and (provider-deferred-tool-loading-p provider)
                 (= (length tools) 2)
                 (json-string= (json-get namespace "type") "namespace")
                 (json-string= (json-get namespace "name") "resource")
                 (json-string= (json-get child "name") "read")
                 (eq (json-get child "defer_loading") t)
                 (json-string= (json-get (aref tools 1) "type") "tool_search"))
            "capable Codex requests defer tools inside native namespaces")
           (test-assert
            (eq (provider-wire-input-item provider call) call)
            "native namespaced calls replay without flattening")
           (let* ((future-configuration
                    (configuration-copy configuration
                                        :model                 "gpt-5.7-codex"
                                        :provider-validation-p nil))
                  (future-provider (provider-create future-configuration)))
             (test-assert
              (provider-deferred-tool-loading-p future-provider)
              "future GPT models retain documented deferred-tool support"))
           (let* ((fallback-configuration
                    (configuration-copy configuration :model   "gpt-5.3-codex"
                                        :provider-validation-p nil))
                  (fallback-provider (provider-create fallback-configuration))
                  (fallback-tools    (provider-wire-tools fallback-provider schemas))
                  (fallback-call
                    (provider-wire-input-item fallback-provider call)))
             (test-assert
              (and (not (provider-deferred-tool-loading-p fallback-provider))
                   (= (length fallback-tools) 1)
                   (json-string= (json-get (aref fallback-tools 0) "type")
                                 "function")
                   (null (find "tool_search" fallback-tools
                               :key  (lambda (tool) (json-get tool "type"))
                               :test #'string=))
                   (null (json-get fallback-call "namespace"))
                   (not (string= (json-get fallback-call "name") "read")))
              "unsupported Codex models retain the exact eager wire fallback"))
           (let* ((eager-characters
                    (length (json-encode
                             (provider-wire-tools
                              (provider-create
                               (configuration-copy configuration :model "gpt-5.3-codex"
                                        :provider-validation-p          nil))
                              schemas))))
                  (visible-characters
                    (length
                     (json-encode
                      (json-array
                       (json-object "type" "namespace"
                                    "name" (json-get namespace "name")
                                    "description"
                                    (json-get namespace "description"))
                       (json-object "type" "tool_search"))))))
             (test-assert
              (< visible-characters eager-characters)
              "deferred discovery reduces model-visible schema characters"))
           (let ((search-tool (aref tools 1)))
             (test-assert
              (and (json-string= (json-get search-tool "execution") "client")
                   (json-object-p (json-get (json-get search-tool "parameters")
                                            "properties"))
                   (json-object-p
                    (json-get (json-get (json-get search-tool "parameters")
                                        "properties")
                              "query")))
              "tool search is declared client-executed with a query parameter"))
           (let* ((search-output
                    (json-object
                     "type" "tool_search_output"
                     "status" "completed"
                     "call_id" "search-1"
                     "execution" "client"
                     "tools" (json-array
                              (json-object
                               "type" "namespace"
                               "name" "plan"
                               "description" "Plans."
                               "tools" (json-array
                                        (json-object
                                         "type" "function"
                                         "name" "update"
                                         "description" "Update the plan."
                                         "defer_loading" t
                                         "parameters"
                                         (json-object "type" "object"
                                                      "properties" (json-object))
                                         "output_schema" nil)
                                        (json-object
                                         "type" "function"
                                         "name" "broken"
                                         "defer_loading" t
                                         "parameters" nil
                                         "output_schema" nil))))))
                  (replayed           (provider-wire-input-item provider search-output))
                  (replayed-namespace (aref (json-get replayed "tools") 0))
                  (replayed-child     (aref (json-get replayed-namespace "tools") 0))
                  (trimmed            (let ((*provider-history-trimming-p* t))
                                        (provider-wire-input-item provider search-output))))
             (test-assert
              (and (json-string= (json-get replayed "type") "tool_search_output")
                   (= (length (json-get replayed "tools")) 1)
                   (= (length (json-get replayed-namespace "tools")) 1)
                   (json-string= (json-get replayed-child "name") "update")
                   (eq (json-get replayed-child "defer_loading") t)
                   (not (nth-value 1 (json-get-present replayed-child "output_schema")))
                   (nth-value 1 (json-get-present replayed-child "strict"))
                   (json-object-p (json-get replayed-child "parameters")))
              "tool search expansions replay intact in the deferred wire shape")
             (test-assert
              (and (json-string= (json-get trimmed "type") "tool_search_output")
                   (zerop (length (json-get trimmed "tools")))
                   (= (length (json-get search-output "tools")) 1))
              "history trimming replays tool search expansions empty")
             (test-assert
              (and (family-private-item-p search-output)
                   (family-private-item-p
                    (json-object "type" "tool_search_call"
                                 "status" "completed")))
              "tool search items never replay into another provider family")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-provider-request () null)
(defun test-provider-request ()
  "Test the standard Codex Responses request shape after a date rollover."
  (let* ((*system-prompt-session-date* (cons (current-process-id) "2000-01-01"))
         (base-configuration           (test-configuration))
         (root                         (test-configuration-root base-configuration))
         (configuration
           (configuration-copy base-configuration
                                 :working-directory root
                                 :reasoning-effort  "ultra")))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "request-shape"))
                (provider     (provider-create configuration))
                (schemas      (json-array
                               (json-object
                                "type" "namespace"
                                "name" "test"
                                "description" "Test tools."
                                "tools"
                                (json-array
                                 (json-object
                                  "name" "inspect"
                                  "description" "Inspect a value."
                                  "parameters" (json-object "type" "object"))))))
                (request      nil))
           (conversation-append-user-message conversation "hello")
           (setf request (provider-request-object provider conversation schemas))
           (test-assert (provider-child-reference-history-p provider)
                        "Codex enables inherited child reference history")
           (test-assert (null (json-get request "service_tier"))
                        "standard Codex requests omit the service tier")
           (let* ((model (first *codex-fast-mode-models*))
                  (fast-configuration
                    (configuration-copy
                     (configuration-copy configuration :model model) :codex-fast-mode-p t))
                  (fast-request
                    (provider-request-object
                     (provider-create fast-configuration) conversation schemas)))
             (test-assert
              (and (configuration-codex-fast-mode-active-p fast-configuration)
                   (string= (json-get fast-request "service_tier") "priority"))
              "a supported Codex model requests the Fast service tier"))
           (let* ((unknown-configuration
                    (configuration-copy configuration
                                        :model                 "gpt-future-unknown"
                                        :codex-fast-mode-p     t
                                        :provider-validation-p nil))
                  (unknown-provider (provider-create unknown-configuration))
                  (unknown-request
                    (provider-request-object
                     unknown-provider conversation schemas)))
             (test-assert
              (and (config :codex-fast-mode-p unknown-configuration)
                   (not (configuration-codex-fast-mode-active-p
                         unknown-configuration))
                   (null (json-get unknown-request "service_tier")))
              "unknown Codex models use the standard service tier"))
           (test-assert (null (json-get request "max_output_tokens"))
                        "requests omit the output ceiling when none is bound")
           (let ((input (json-get request "input")))
             (test-assert
              (and (> (length input) 1)
                   (string= (json-get (aref input 0) "role") "user")
                   (every (lambda (item)
                            (string= (json-get item "role") "developer"))
                          (subseq input 1))
                   (some (lambda (item)
                           (search "Current workspace agenda"
                                   (context--message-text item)))
                         (subseq input 1)))
              "standard Responses input delivers mutable context after user history"))
           (test-assert
            (non-empty-string-p (json-get request "instructions"))
            "standard Responses uses top-level stable instructions")
           (let* ((goal-text  "<goal_context>persist</goal_context>")
                  (goal-request
                    (provider-request-object
                     provider conversation schemas :goal-context goal-text))
                  (goal-input (json-get goal-request "input")))
             (test-assert
              (and (> (length goal-input) 2)
                   (string= (json-get (aref goal-input 0) "role") "user")
                   (every (lambda (item)
                            (string= (json-get item "role") "developer"))
                          (subseq goal-input 1))
                   (search goal-text
                           (context--message-text (aref goal-input 1)))
                   (some (lambda (item)
                           (search "Current workspace agenda"
                                   (context--message-text item)))
                         (subseq goal-input 2)))
              "goal and mutable context follow durable conversation input")
             (test-assert
              (not (search goal-text (json-get goal-request "instructions")))
              "goal context stays outside the stable instructions"))
           (with-recursive-lock-held (*agenda-lock*)
             (let ((state (agenda-load configuration)))
               (agenda-add :configuration      configuration
                           :state              state
                           :text               "cache-prefix mutation"
                           :status             ':doing
                           :memory-identifiers nil)))
           (let* ((mutated-request
                    (provider-request-object provider conversation schemas))
                  (mutated-input (json-get mutated-request "input")))
             (test-assert
              (string= (json-get request "instructions")
                       (json-get mutated-request "instructions"))
              "an agenda mutation preserves byte-identical stable instructions")
             (test-assert
              (some (lambda (item)
                      (search "cache-prefix mutation" (context--message-text item)))
                    (subseq mutated-input 1))
              "an agenda mutation appears in trailing request context"))
           (test-assert
            (null (provider-web-search-tool configuration))
            "the nonfunctional native web search tool stays disabled")
           (test-assert
            (null (provider-tests--request-tool-of-type request "web_search"))
            "requests omit the nonfunctional native web search tool")
           (test-assert
            (string= (json-get (json-get request "reasoning") "effort") "max")
            "the provider request maps Ultra reasoning to Max")
           (multiple-value-bind (value present-p)
               (gethash "summary" (json-get request "reasoning"))
             (declare (ignore value))
             (test-assert (not present-p)
                          "hidden traces do not request reasoning summaries"))
           (let* ((trace-provider
                    (provider-create configuration :reasoning-summaries-p t))
                  (trace-request
                    (provider-request-object
                     trace-provider conversation schemas))
                  (trace-reasoning (json-get trace-request "reasoning")))
             (test-assert
              (string= (json-get trace-reasoning "summary") "auto")
              "visible traces request the best supported reasoning summary")
             (let ((compaction-reasoning
                     (json-get
                      (provider-request-object
                       trace-provider conversation schemas :compaction-p t)
                      "reasoning")))
               (multiple-value-bind (value present-p)
                   (gethash "summary" compaction-reasoning)
                 (declare (ignore value))
                 (test-assert
                  (not present-p)
                  "side-channel compaction does not request unused summaries")))
             (setf (provider-rate-limits trace-provider)
                   '((:primary (:used-percent 42))))
             (let* ((reconfiguration
                      (configuration-copy configuration :reasoning-effort "high"))
                    (reconfigured
                      (provider-with-configuration
                       trace-provider reconfiguration)))
               (test-assert
                (and (eq (class-of reconfigured) (class-of trace-provider))
                     (eq (provider-configuration reconfigured) reconfiguration)
                     (eq (model-provider-registration reconfigured)
                         (model-provider-registration trace-provider))
                     (eq (provider-credential-manager reconfigured)
                         (provider-credential-manager trace-provider))
                     (string= (provider-session-id reconfigured)
                              (provider-session-id trace-provider))
                     (provider-reasoning-summaries-p reconfigured)
                     (equal (provider-rate-limits reconfigured)
                            (provider-rate-limits trace-provider))
                     (not (eq (provider-rate-limits reconfigured)
                              (provider-rate-limits trace-provider))))
                "Codex reconfiguration preserves copied protocol and session state")))
           (test-assert
            (and (string= (json-get request "tool_choice") "auto")
                 (eq (json-get request "parallel_tool_calls") t)
                 (json-false-p (gethash "store" request))
                 (eq (json-get request "stream") t))
            "standard Responses requests carry their transport controls")
           (test-assert
            (equalp (json-get request "include")
                    (json-array "reasoning.encrypted_content"))
            "the provider request retains encrypted reasoning for replay")
           (test-assert
            (string= (json-get request "prompt_cache_key")
                     (conversation-identifier conversation))
            "the root conversation is the stable prompt cache key")
           (let* ((child-conversation
                    (conversation-create
                     configuration
                     :identifier "child-request-shape"
                     :prompt-cache-key
                     (conversation-prompt-cache-key conversation)))
                  (child-provider
                    (provider-with-configuration provider configuration)))
             (conversation-append-user-message child-conversation "inspect")
             (let ((child-request
                     (provider-request-object
                      child-provider child-conversation schemas)))
               (test-assert
                (not (string= (conversation-identifier child-conversation)
                              (conversation-identifier conversation)))
                "parent and child conversations keep distinct thread identities")
               (test-assert
                (string= (json-get child-request "prompt_cache_key")
                         (json-get request "prompt_cache_key"))
                "reconfigured child providers share the parent prompt cache key")))
           (let* ((other-conversation
                    (conversation-create configuration
                                         :identifier "other-request-shape"))
                  (other-request
                    (provider-request-object provider other-conversation schemas)))
             (test-assert
              (not (string= (json-get other-request "prompt_cache_key")
                            (json-get request "prompt_cache_key")))
              "independent root conversations keep isolated prompt cache keys"))
           (test-assert
            (string= (json-get (json-get request "text") "verbosity") "low")
            "the provider request asks for restrained text verbosity")
           (let* ((fallback-configuration
                    (configuration-copy configuration :model   "gpt-5.3-codex"
                                        :provider-validation-p nil))
                  (fallback-provider (provider-create fallback-configuration))
                  (tools             (provider-wire-tools fallback-provider schemas))
                  (wire-name
                    (provider-wire-tool-name fallback-provider "test" "inspect")))
             (test-assert
              (and (= (length tools) 1)
                   (string= (json-get (aref tools 0) "name") wire-name)
                   (provider-wire-function-name--valid-p wire-name))
              "eager Responses fallback uses the shared grammar-safe tool codec"))
           (let ((compaction-request
                   (provider-request-object
                    provider conversation schemas :compaction-p t)))
             (test-assert
              (and (json-false-p (gethash "parallel_tool_calls" compaction-request))
                   (zerop (length (json-get compaction-request "tools")))
                   (search "context checkpoint compaction"
                           (json-get compaction-request "instructions"))
                   (search "Do not call tools"
                           (json-get compaction-request "instructions")))
              "portable compaction fallback explicitly requires tool-free text"))
           (let* ((fallback-configuration
                    (configuration-copy configuration :model  "gpt-5.3-codex"
                                       :provider-validation-p nil))
                  (fallback-provider (provider-create fallback-configuration))
                  (local-call
                    (json-object
                     "type" "function_call"
                     "namespace" "test"
                     "name" "inspect"
                     "call_id" "call-standard"))
                  (wire-call
                    (provider-wire-input-item fallback-provider local-call))
                  (normalized
                    (provider-normalize-output-item
                     fallback-provider (json-object-copy wire-call))))
             (test-assert
              (and (null (json-get wire-call "namespace"))
                   (provider-wire-function-name--valid-p
                    (json-get wire-call "name"))
                   (string= (json-get normalized "namespace") "test")
                   (string= (json-get normalized "name") "inspect"))
              "eager Codex wire hooks round-trip the local tool namespace")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-provider-native-compaction () null)
(defun test-provider-native-compaction ()
  "Test the Codex native checkpoint request, transport, and fallback boundary."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "native-request"))
         (provider      (provider-create configuration))
         (credentials   (provider-tests--credentials configuration))
         (schemas
           (json-array
            (json-object "type" "namespace"
                         "name" "test"
                         "description" "Test tools."
                         "tools" (json-array)))))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "retain this work")
           (conversation-append-provider-item
            conversation
            (json-object "type" "reasoning"
                         "encrypted_content" "retained-reasoning"
                         "summary" (json-array)))
           (conversation-append-provider-item
            conversation
            (json-object "type" "function_call"
                         "call_id" "ephemeral-call"
                         "name" "test"
                         "arguments" "{}")
            :persistence ':next-response)
           (let* ((request
                    (provider-native-compaction-request-object
                     provider conversation schemas))
                  (input (json-get request "input")))
             (test-assert
              (and (= (length input) 2)
                   (string= (json-get (aref input 0) "role") "user"))
              "native compaction carries durable standard Responses input")
             (test-assert
              (and (reasoning-item-p (aref input 1))
                   (string= (json-get (aref input 1) "encrypted_content")
                            "retained-reasoning"))
              "native compaction carries retained encrypted reasoning")
             (test-assert
              (non-empty-string-p (json-get request "instructions"))
              "native compaction uses top-level instructions")
             (test-assert
              (string= (json-get request "prompt_cache_key")
                       (conversation-prompt-cache-key conversation))
              "native compaction shares the root conversation cache key")
             (test-assert (null (json-get request "service_tier"))
                          "standard native compaction omits a service tier")
             (let* ((fast-configuration
                      (configuration-copy configuration :codex-fast-mode-p t))
                    (fast-provider (provider-create fast-configuration))
                    (fast-request
                      (provider-native-compaction-request-object
                       fast-provider conversation schemas)))
               (test-assert
                (string= (json-get fast-request "service_tier") "priority")
                "Codex Fast mode applies to native compaction"))
             (let* ((unknown-configuration
                      (configuration-copy configuration
                                          :model                 "gpt-future-unknown"
                                          :codex-fast-mode-p     t
                                          :provider-validation-p nil))
                    (unknown-provider (provider-create unknown-configuration))
                    (unknown-request
                      (provider-native-compaction-request-object
                       unknown-provider conversation schemas)))
               (test-assert
                (null (json-get unknown-request "service_tier"))
                "unknown Codex models omit Fast mode during compaction"))
             (dolist (name '("stream" "store" "include" "tool_choice"
                             "parallel_tool_calls" "reasoning" "text" "tools"))
               (multiple-value-bind (value present-p) (gethash name request)
                 (declare (ignore value))
                 (test-assert (not present-p)
                              (format nil "native compaction omits ~A" name)))))
           (let ((captured-url     nil)
                 (captured-headers nil)
                 (captured-content nil))
             (test-call-with-function-replacements
              (list
               (list
                'dexador:post
                (lambda (url &key headers content &allow-other-keys)
                  (setf captured-url url
                        captured-headers headers
                        captured-content content)
                  (values "{\"output\":[]}" 200 nil))))
              (lambda ()
                (provider-open-native-compaction
                 provider
                 (json-object "model" *default-model*)
                 :credentials  credentials
                 :conversation conversation)))
             (flet ((header (name)
                      (rest (assoc name captured-headers :test #'string-equal))))
               (test-assert
                (string= captured-url
                         "https://chatgpt.com/backend-api/codex/responses/compact")
                "native compaction derives the Responses compact endpoint")
               (test-assert (string= (header "Accept") "application/json")
                            "native compaction requests one JSON response")
               (test-assert
                (typep captured-content '(array (unsigned-byte 8) (*)))
                "native compaction sends JSON as direct UTF-8 octets")
               (test-assert
                (string=
                 (json-get
                  (json-decode
                   (sb-ext:octets-to-string
                    captured-content :external-format ':utf-8))
                  "model")
                 *default-model*)
                "direct UTF-8 request content retains its JSON payload")
               (test-assert
                (string= (header "Authorization")
                         "Bearer provider-test-access-7f386d")
                "native compaction uses the ordinary subscription credentials")))
           (credential-source-save
            (credential-manager-primary-source
             (provider-credential-manager provider))
            credentials)
           (let ((result nil))
             (test-call-with-function-replacements
              (list
               (list
                'dexador:post
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (values
                   (json-encode
                    (json-object
                     "output"
                       (json-array
                        (json-object "type" "message"
                                     "role" "user"
                                     "content"
                                     (json-array
                                      (json-object "type" "input_text"
                                                   "text" "retained user")))
                        (json-object "id" "cmp_123"
                                     "type" "compaction"
                                     "encrypted_content" "opaque-checkpoint")
                        (json-object "type" "message"
                                     "role" "assistant"
                                     "content"
                                     (json-array
                                      (json-object "type" "output_text"
                                                   "text" "retained assistant"))))))
                   200
                   '(("x-request-id" . "native-success"))))))
              (lambda ()
                (setf result
                      (provider-native-compact-conversation
                       provider conversation
                       :tool-namespaces schemas
                       :event-callback  #'identity))))
             (test-assert
              (and (listp result)
                   (= (length result) 3)
                   (string= (json-get (first result) "role") "user")
                   (native-compaction-item-p (second result))
                   (string= (json-get (third result) "role") "assistant"))
              "native compaction returns the complete ordered output window")
             (multiple-value-bind (value present-p) (gethash "id" (second result))
               (declare (ignore value))
               (test-assert (not present-p)
                            "native compaction drops transient server item identifiers")))
           (let ((result
                   (provider--decode-native-compaction-response
                    provider
                    (json-encode
                     (json-object
                      "output"
                      (json-array
                       (json-object "type" "compaction_summary"
                                    "encrypted_content" "legacy-checkpoint")
                       (json-object "type" "context_compaction"
                                    "encrypted_content" "current-checkpoint")))))))
              (test-assert
               (and (listp result)
                    (= (length result) 2)
                    (native-compaction-item-p (second result))
                    (string= (json-get (second result) "type") "context_compaction")
                    (string= (json-get (second result) "encrypted_content")
                             "current-checkpoint"))
               "native compaction keeps the complete window and newest checkpoint encoding"))
           (let ((result
                   (provider--decode-native-compaction-response
                    provider
                    (json-encode
                     (json-object
                      "output"
                      (json-array
                       (json-object "type" "compaction_summary"
                                    "encrypted_content" "legacy-checkpoint")))))))
               (test-assert
                (and (listp result)
                     (= (length result) 1)
                     (string= (json-get (first result) "type") "compaction"))
                "native compaction canonicalizes Codex's legacy checkpoint alias"))
           (test-assert
            (null
             (provider--decode-native-compaction-response
              provider (json-encode (json-object "output" (json-array)))))
            "an opaque-free compact transcript falls back to the portable summary")
           (test-call-with-function-replacements
            (list
             (list
              'dexador:post
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (values "not found" 404 nil))))
            (lambda ()
              (test-assert
               (null
                (provider-native-compact-conversation
                 provider conversation
                 :tool-namespaces schemas
                 :event-callback  #'identity))
               "an unavailable native endpoint falls back without failing compaction"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-sse-event-string (json-object) string)
(defun test-sse-event-string (event)
  "Encode EVENT as one complete server-sent event."
  (format nil "data: ~A~%~%" (json-encode event)))

(defclass test-character-input-stream
    (trivial-gray-streams:fundamental-character-input-stream)
  ((source
    :initarg       :source
    :reader        test-character-input-stream-source
    :type          string
    :documentation "The deterministic character source.")
   (position
    :initform      0
    :accessor      test-character-input-stream-position
    :type          integer
    :documentation "The next source character offset."))
  (:documentation "A test stream implementing character reads but not line reads."))

(defmethod trivial-gray-streams:stream-read-char ((stream test-character-input-stream))
  "Read one character from STREAM, returning the Gray-stream EOF marker at its end."
  (let ((position (test-character-input-stream-position stream))
        (source   (test-character-input-stream-source stream)))
    (if (< position (length source))
        (prog1 (char source position)
          (incf (test-character-input-stream-position stream)))
        :eof)))

(defclass test-tls-eof-stream (test-character-input-stream)
  ()
  (:documentation "A provider stream that signals an unclean TLS EOF after its source."))

(defmethod trivial-gray-streams:stream-read-char ((stream test-tls-eof-stream))
  "Read the scripted input, then signal the OpenSSL unexpected-EOF condition."
  (let ((character (call-next-method)))
    (if (eq character ':eof)
        (error (provider-tests--tls-eof-condition))
        character)))

(defclass test-failing-close-stream (test-character-input-stream)
  ((close-abort-p
    :initform      nil
    :accessor      test-failing-close-stream-close-abort-p
    :type          boolean
    :documentation "Whether the attempted close was abortive."))
  (:documentation "A deterministic provider stream whose close operation fails."))

(defmethod close ((stream test-failing-close-stream) &key abort)
  "Record ABORT and inject the low-level TLS cleanup failure under test."
  (setf (test-failing-close-stream-close-abort-p stream)
        (not (null abort)))
  (error 'ssl-error-syscall
         :queue         nil
         :printed-queue nil
         :ret           -1
         :handle        nil
         :syscall       'close))

(define-condition test-provider-tls-error (cl+ssl-error)
  ((detail
    :initarg       :detail
    :reader        test-provider-tls-error-detail
    :type          string
    :documentation "The synthetic TLS diagnostic shown by the condition."))
  (:report
   (lambda (condition stream)
     (write-string (test-provider-tls-error-detail condition) stream)))
  (:documentation "A TLS failure with deterministic model-visible detail."))

(defclass test-transport-provider (codex-subscription-provider)
  ((outcomes
    :initarg       :outcomes
    :accessor      test-transport-provider-outcomes
    :type          list
    :documentation "The connection outcomes returned or signaled in order.")
   (attempt-count
    :initform      0
    :accessor      test-transport-provider-attempt-count
    :type          integer
    :documentation "The number of response streams requested."))
  (:documentation "A subscription provider injecting transport boundary outcomes."))

(defmethod provider-open-response-stream
    ((provider test-transport-provider)
     (request hash-table)
     &key credentials conversation)
  "Return or signal the next scripted transport outcome for PROVIDER."
  (declare (ignore request credentials conversation))
  (incf (test-transport-provider-attempt-count provider))
  (let ((outcome (pop (test-transport-provider-outcomes provider))))
    (cond
      ((typep outcome 'condition)
       (error outcome))
      ((eq outcome ':syscall)
       (error 'ssl-error-syscall
              :queue         nil
              :printed-queue nil
              :ret           -1
              :handle        nil
              :syscall       'connect))
      ((eq outcome ':tls)
       (error 'test-provider-tls-error
              :detail "certificate verification failed"))
      ((and (consp outcome) (eq (first outcome) ':tls))
       (error 'test-provider-tls-error :detail (second outcome)))
      ((eq outcome ':name-service)
       ;; SBCL's addrinfo temporary failure has this subtype, not generic errno1.
       (error 'sb-bsd-sockets:try-again-error :syscall "getaddrinfo"))
      ((eq outcome ':simple-error)
       (error "Synthetic provider transport failure."))
      ((consp outcome)
       (values-list outcome))
      (t
       (values outcome 200 nil)))))

(defmethod provider-open-native-compaction
    ((provider test-transport-provider)
     (request hash-table)
     &key credentials conversation)
  "Signal the next scripted transport outcome for native compaction."
  (declare (ignore request credentials conversation))
  (incf (test-transport-provider-attempt-count provider))
  (let ((outcome (pop (test-transport-provider-outcomes provider))))
    (cond
      ((typep outcome 'condition)
       (error outcome))
      ((eq outcome ':name-service)
       (error 'sb-bsd-sockets:try-again-error :syscall "getaddrinfo"))
      ((eq outcome ':tls)
       (error 'test-provider-tls-error
              :detail "compaction certificate verification failed"))
      ((and (consp outcome) (eq (first outcome) ':tls))
       (error 'test-provider-tls-error :detail (second outcome)))
      (t
       (values outcome 200 nil)))))

(-> provider-tests--tls-eof-condition () cl+ssl::ssl-error-ssl)
(defun provider-tests--tls-eof-condition ()
  "Return the pinned cl+ssl condition for an OpenSSL 3+ unclean peer EOF."
  (make-condition 'cl+ssl::ssl-error-ssl
                  :handle        nil
                  :ret           1
                  :queue         nil
                  :printed-queue "00000000:error:0A000126:SSL routines::unexpected eof while reading:ssl/record/example.c:1:"))

(-> provider-tests--completed-sse-source (string) string)
(defun provider-tests--completed-sse-source (response-id)
  "Return a minimal successful SSE response carrying RESPONSE-ID."
  (concatenate
   'string
   (test-sse-event-string
    (json-object
     "type" "response.created"
     "response" (json-object "id" response-id)))
   (test-sse-event-string
    (json-object
     "type" "response.completed"
     "response" (json-object
                 "id" response-id
                 "usage" (json-object "input_tokens" 1))))))

(-> provider-tests--transport-provider
    (configuration list)
    test-transport-provider)
(defun provider-tests--transport-provider (configuration outcomes)
  "Return a test provider yielding transport OUTCOMES."
  (make-instance
   'test-transport-provider
   :configuration      configuration
   :credential-manager (credential-manager-create configuration)
   :session-id         (make-identifier)
   :outcomes           outcomes))

(-> test-provider-context-presentation () null)
(defun test-provider-context-presentation ()
  "Exercise presentation events through authenticated ordinary and compaction requests."
  (with-test-configuration (configuration)
    (let* ((*context-contributors*        nil)
           (*context-resolver*            (cl-llm-provider-api:make-context-resolver))
           (*context-last-deliveries*     (make-hash-table :test #'equal))
           (*context-last-delivery-order* nil)
           (conversation                  (conversation-create configuration))
           (credentials                   (provider-tests--credentials configuration)))
      (memory-remember configuration
                       :title   "Granite reference"
                       :content "Inspect this reference before use."
                       :scope   ':workspace
                       :tags    '("granite"))
      (register-context-contributor "related-memories" 'memory-related-context
                                    :source ':built-in)
      (conversation-append-user-message conversation "granite")
      (dolist (compaction-p '(nil t))
        (let* ((response-id    (if compaction-p "compaction-presentation" "ordinary-presentation"))
               (stream         (make-instance 'test-character-input-stream
                                              :source (provider-tests--completed-sse-source response-id)))
               (provider       (provider-tests--transport-provider configuration (list stream)))
               (memory-notices 0))
          (credential-source-save
           (credential-manager-primary-source (provider-credential-manager provider))
           credentials)
          (let ((result (provider-attempt-turn
                         provider conversation
                         :tool-namespaces #()
                         :event-callback  (lambda (event)
                                            (when (typep event 'memory-context-event)
                                              (incf memory-notices)))
                         :force-refresh   nil
                         :compaction-p    compaction-p)))
            (test-assert (and (string= (provider-result-response-id result) response-id)
                              (= (test-transport-provider-attempt-count provider) 1))
                         "both request modes reach transport and complete with a callback")
            (test-assert (= memory-notices (if compaction-p 0 1))
                         "only ordinary requests announce their selected memories"))))))
  nil)

(-> provider-tests--connection-phases (list) list)
(defun provider-tests--connection-phases (events)
  "Read chronological connection phases without altering other event assertions."
  (loop for event in (reverse events)
        when (typep event 'provider-connection-event)
          collect (getf (provider-connection-event-details event) :phase)))

(-> provider-tests--assert-persistent-reconnect (list) null)
(defun provider-tests--assert-persistent-reconnect (events)
  "Require the precise pre-generation lifecycle and one same-route retry."
  (let* ((connections (reverse (remove-if-not
                                (lambda (event) (typep event 'provider-connection-event))
                                events)))
         (details     (mapcar #'provider-connection-event-details connections)))
    (test-assert (equal (provider-tests--connection-phases events)
                        '(:attempting :waiting :attempting :completed))
                 "opening recovery publishes exactly one wait and the next attempt")
    (test-assert
     (and (= (length details) 4)
          (equal (mapcar (lambda (detail) (getf detail :attempt)) details) '(1 1 2 2))
          (every (lambda (detail)
                   (and (equal (getf detail :episode) (getf (first details) :episode))
                        (eql (getf detail :route-index) (getf (first details) :route-index))))
                 details)
          (plusp (getf (second details) :delay)))
     "the wait belongs to the same episode and route before logical attempt two")
    (test-assert
     (equal (loop for event in (reverse events)
                  when (typep event '(or provider-connection-event provider-attempt-failed-event))
                    collect (if (typep event 'provider-connection-event)
                                (getf (provider-connection-event-details event) :phase)
                                :failure))
            '(:attempting :failure :waiting :attempting :completed))
     "the single transport failure precedes its wait and next attempt")
    (test-assert (notany (lambda (event) (typep event 'provider-retry-event)) events)
                 "pre-generation recovery never emits bounded retry events"))
  nil)

(-> test-provider-tls-eof-recovery () null)
(defun test-provider-tls-eof-recovery ()
  "Reconnect on TLS EOF across authenticated opens, SSE reads, and native compaction."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (credentials  (provider-tests--credentials configuration))
           (*bounded-retry-sleep-function*
             (lambda (seconds)
               (declare (ignore seconds)))))
      (conversation-append-user-message conversation "continue after reconnecting")
      (dolist (boundary '(:open :stream-before-output :stream-after-output :native))
        (let* ((native-p (eq boundary ':native))
               (output-p (eq boundary ':stream-after-output))
               (first-outcome
                 (if (member boundary '(:open :native))
                     (provider-tests--tls-eof-condition)
                     (make-instance
                      'test-tls-eof-stream
                      :source
                      (concatenate
                       'string
                       (test-sse-event-string
                        (json-object "type" "response.created"
                                     "response" (json-object "id" "failed-tls-response")))
                       (if output-p
                           (test-sse-event-string
                            (json-object "type" "response.output_text.delta" "delta" "partial"))
                           "")))))
               (success-outcome
                 (if native-p
                     (json-encode
                      (json-object "output"
                                   (json-array
                                    (json-object "type" "compaction"
                                                 "encrypted_content" "tls-checkpoint"))))
                     (make-instance
                      'test-character-input-stream
                      :source (provider-tests--completed-sse-source "tls-reconnected"))))
               (provider
                 (provider-tests--transport-provider
                  configuration (list first-outcome success-outcome)))
               (events   nil))
          (credential-source-save
           (credential-manager-primary-source (provider-credential-manager provider))
           credentials)
          (let ((result
                  (if native-p
                      (provider-native-compact-conversation
                       provider conversation
                       :tool-namespaces #()
                       :event-callback  (lambda (event) (push event events)))
                      (provider-stream-turn
                       provider conversation
                       :tool-namespaces #()
                       :event-callback  (lambda (event) (push event events))))))
            (test-assert
             (and (= (test-transport-provider-attempt-count provider) 2)
                  (if native-p
                      (and (= 1 (length result))
                           (native-compaction-item-p (first result))
                           (string= (json-get (first result) "encrypted_content") "tls-checkpoint"))
                      (string= (provider-result-response-id result) "tls-reconnected")))
             "each TLS EOF boundary reconnects and returns the next successful response"))
          (let ((failures
                  (remove-if-not
                   (lambda (event) (typep event 'provider-attempt-failed-event)) events)))
            (test-assert
             (and (= (length failures) 1)
                  (provider-attempt-failed-event-retryable-p (first failures))
                  (typep (provider-attempt-failed-event-condition (first failures))
                         'provider-transport-error)
                  (eq (provider-attempt-failed-event-output-received-p (first failures)) output-p))
             "TLS EOF retains its retryable transport class and observed output state"))
          (if output-p
              (let ((retries (reverse (remove-if-not
                                       (lambda (event) (typep event 'provider-retry-event))
                                       events))))
                (test-assert
                 (and (= (length retries) 2)
                      (= (provider-retry-event-attempt (first retries)) 1)
                      (= (provider-retry-event-maximum-attempts (first retries)) 6)
                      (<= 1 (provider-retry-event-delay (first retries)) 60)
                      (= (provider-retry-event-attempt (second retries)) 1)
                      (zerop (provider-retry-event-delay (second retries))))
                 "post-output TLS EOF schedules exactly one bounded reconnect pair")
                (test-assert
                 (equal (provider-tests--connection-phases events)
                        '(:attempting :attempting :completed))
                 "post-output recovery never enters persistent waiting"))
              (provider-tests--assert-persistent-reconnect events))))))
  nil)

(-> test-provider-transport-boundary () null)
(defun test-provider-transport-boundary ()
  "Test connection normalization and failure-proof provider stream cleanup."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier "provider-transport"))
         (credentials   (provider-tests--credentials configuration)))
    (unwind-protect
         (progn
           (let* ((success-stream
                    (make-instance
                     'test-character-input-stream
                     :source
                     (provider-tests--completed-sse-source
                      "transport-retry-success")))
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list :syscall success-stream))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (let ((*bounded-retry-sleep-function*
                     (lambda (seconds)
                       (declare (ignore seconds)))))
               (let ((result
                       (provider-stream-turn
                        provider
                        conversation
                        :tool-namespaces #()
                        :event-callback  #'identity)))
                 (test-assert
                  (string= (provider-result-response-id result)
                           "transport-retry-success")
                  "an open-time TLS syscall failure reconnects successfully")
                 (test-assert
                  (= (test-transport-provider-attempt-count provider) 2)
                  "a transient open failure consumes one bounded retry"))))
           (let* ((success-stream
                    (make-instance
                     'test-character-input-stream
                     :source
                     (provider-tests--completed-sse-source
                      "name-service-retry-success")))
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list ':name-service success-stream))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (let ((*bounded-retry-sleep-function*
                     (lambda (seconds)
                       (declare (ignore seconds)))))
               (let ((result
                       (provider-stream-turn
                        provider
                        conversation
                        :tool-namespaces #()
                        :event-callback  #'identity)))
                 (test-assert
                  (string= (provider-result-response-id result)
                           "name-service-retry-success")
                  "an SBCL name-service error reconnects successfully")
                 (test-assert
                  (= (test-transport-provider-attempt-count provider) 2)
                  "an SBCL name-service error remains inside the retry boundary"))))
           (let* ((*provider-active-credential-values*           '("tls-secret"))
                  (*provider-active-credential-redaction-marker* "[redacted]")
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list '(:tls "certificate rejected tls-secret")))))
             (test-assert
              (handler-case
                  (progn
                    (provider--open-response-stream
                     provider
                     (json-object)
                     :credentials  credentials
                     :conversation conversation)
                    nil)
                (provider-error (condition)
                  (and
                   (not (typep condition 'provider-retryable-error))
                   (string=
                    (autolith-error-message condition)
                    "The provider TLS connection could not be established: certificate rejected [redacted]"))))
              "TLS failures append useful credential-redacted condition detail"))
           (let ((provider
                   (provider-tests--transport-provider
                    configuration
                    (list '(:tls "compaction certificate expired")))))
             (test-assert
              (handler-case
                  (progn
                    (provider--open-native-compaction
                     provider
                     (json-object)
                     :credentials  credentials
                     :conversation conversation)
                    nil)
                (provider-error (condition)
                  (string=
                   (autolith-error-message condition)
                   "The provider TLS connection could not be established: compaction certificate expired")))
              "native compaction preserves TLS condition detail"))
           (let ((provider
                   (provider-tests--transport-provider
                    configuration
                    (list ':name-service))))
             (test-assert
              (handler-case
                  (progn
                    (provider--open-native-compaction
                     provider
                     (json-object)
                     :credentials  credentials
                     :conversation conversation)
                    nil)
                (provider-transport-error ()
                  t))
              "native compaction normalizes an SBCL name-service error"))
           (let ((provider
                   (provider-tests--transport-provider
                    configuration
                    (list ':simple-error))))
             (test-assert
              (handler-case
                  (progn
                    (provider--open-response-stream
                     provider
                     (json-object)
                     :credentials  credentials
                     :conversation conversation)
                    nil)
                (provider-error (condition)
                  (and (not (typep condition 'provider-retryable-error))
                       (string=
                        (autolith-error-message condition)
                        "The provider transport failed before a response was received: Synthetic provider transport failure."))))
              "a raw transport SIMPLE-ERROR becomes a terminal provider failure"))
           (let* ((stream
                    (make-instance
                     'test-failing-close-stream
                     :source
                     (provider-tests--completed-sse-source
                      "close-failure-success")))
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list stream))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (let ((result
                     (provider-attempt-turn
                      provider
                      conversation
                      :tool-namespaces #()
                      :event-callback  #'identity
                      :force-refresh   nil
                      :goal-context    nil
                      :compaction-p    nil)))
               (test-assert
                (string= (provider-result-response-id result)
                         "close-failure-success")
                "cleanup failure cannot replace a completed provider result")
               (test-assert
                (test-failing-close-stream-close-abort-p stream)
                "provider response streams close abortively")))
           (let* ((stream
                    (make-instance
                     'test-failing-close-stream
                     :source "data: {"))
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list stream))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (test-assert
              (handler-case
                  (progn
                    (provider-attempt-turn
                     provider
                     conversation
                     :tool-namespaces #()
                     :event-callback  #'identity
                     :force-refresh   nil
                     :goal-context    nil
                     :compaction-p    nil)
                    nil)
                (provider-protocol-error (condition)
                  (not (typep condition 'provider-retryable-error))))
              "cleanup failure preserves the terminal stream protocol failure")
             (test-assert
              (test-failing-close-stream-close-abort-p stream)
              "protocol-invalid provider streams also close abortively"))
           (let* ((stream
                    (make-instance
                     'test-failing-close-stream
                     :source "retry buffer failure"))
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list (list stream 507 nil)))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (test-assert
              (handler-case
                  (test-call-with-function-replacements
                   (list
                    (list
                     'provider--signal-http-status-failure
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (error "synthetic status normalization failure"))))
                   (lambda ()
                     (provider-attempt-turn
                      provider
                      conversation
                      :tool-namespaces #()
                      :event-callback  #'identity
                      :force-refresh   nil
                      :goal-context    nil
                      :compaction-p    nil)))
                (simple-error ()
                  t))
              "status normalization failures retain their original condition")
             (test-assert
              (test-failing-close-stream-close-abort-p stream)
              "non-success provider responses remain under stream cleanup"))
           (let ((provider
                   (provider-tests--transport-provider
                    configuration
                    (list :tls))))
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (test-assert
              (handler-case
                  (progn
                    (provider-attempt-turn
                     provider
                     conversation
                     :tool-namespaces #()
                     :event-callback  #'identity
                     :force-refresh   nil
                     :goal-context    nil
                     :compaction-p    nil)
                    nil)
                (provider-error (condition)
                  (not (typep condition 'provider-retryable-error))))
              "non-transient TLS setup failures remain typed and terminal")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> provider-tests--credentials (configuration) oauth-credentials)
(defun provider-tests--credentials (configuration)
  "Return four distinct synthetic credentials for provider containment tests."
  (make-instance
   'oauth-credentials
   :access-token  "provider-test-access-7f386d"
   :refresh-token "provider-test-refresh-a280c4"
   :id-token      "provider-test-identity-f969b1"
   :account-id    "provider-test-account-a0542e"
   :expires-at    nil
   :source-path   (configuration-auth-path configuration)))

(-> provider-tests--assert-credential-free (t list string) null)
(defun provider-tests--assert-credential-free (root secrets description)
  "Assert ROOT contains none of SECRETS, reporting DESCRIPTION."
  (dolist (secret secrets)
    (test-assert
     (not (test-object-contains-string-p root secret))
     description))
  nil)

(-> test-provider-credential-echo-containment () null)
(defun test-provider-credential-echo-containment ()
  "Test provider wire data cannot echo request credentials into retained state."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration
                                :identifier "provider-secret-echo"))
         (provider      (provider-create configuration))
         (credentials   (provider-tests--credentials configuration))
         (secrets       (oauth-credentials-secret-values credentials)))
    (labels
        ((attempt (response-function event-callback)
           "Run one real provider attempt against RESPONSE-FUNCTION."
           (test-call-with-function-replacements
            (list
             (list
              'provider-open-response-stream
              (lambda (active-provider request
                       &key active-credentials active-conversation
                       &allow-other-keys)
                (declare
                 (ignore active-provider request active-credentials
                         active-conversation))
                (funcall response-function))))
            (lambda ()
              (provider-attempt-turn
               provider
               conversation
               :tool-namespaces #()
               :event-callback  event-callback
               :force-refresh   nil
               :goal-context    nil
               :compaction-p    nil)))))
      (unwind-protect
           (progn
             (credential-source-save
              (credential-manager-primary-source
               (provider-credential-manager provider))
              credentials)
             (let* ((events nil)
                    (source
                      (concatenate
                       'string
                       (test-sse-event-string
                        (json-object
                         "type" "response.created"
                         "response"
                         (json-object
                          "id"
                          (oauth-credentials-access-token credentials))))
                       (test-sse-event-string
                        (json-object
                         "type" "response.output_text.delta"
                         "delta"
                         (oauth-credentials-refresh-token credentials)))
                       (test-sse-event-string
                        (json-object
                         "type" "response.reasoning_summary_text.delta"
                         "output_index" 0
                         "summary_index" 0
                         "delta"
                         (oauth-credentials-id-token credentials)))
                       (test-sse-event-string
                        (json-object
                         "type" "response.output_item.done"
                         "item"
                         (json-object
                          "type" "message"
                          "role" "assistant"
                          "content"
                          (json-array
                           (json-object
                            "type" "output_text"
                            "text"
                            (oauth-credentials-account-id credentials))))))
                       (test-sse-event-string
                        (json-object
                         "type" "response.completed"
                         "response"
                         (json-object
                          "id"
                          (oauth-credentials-access-token credentials)
                          "usage"
                          (json-object
                           "echo"
                           (coerce secrets 'vector)))))))
                    (headers
                      (list
                       (cons "x-request-id"
                             (oauth-credentials-account-id credentials))
                       (cons "x-codex-turn-state"
                             (format nil "~{~A~^/~}" secrets))))
                    (result
                      (attempt
                       (lambda ()
                         (values
                          (make-instance
                           'test-character-input-stream
                           :source source)
                          200
                          headers))
                       (lambda (event)
                         (push event events)))))
               (provider-tests--assert-credential-free
                (list result events)
                secrets
                "successful provider results and callbacks contain no credential")
               (test-assert
                (test-object-contains-string-p
                 (list result events)
                 *provider-credential-redaction-marker*)
                "successful credential echoes carry an explicit redaction marker")
               (test-assert
                (and (provider-result-response-id result)
                     (provider-result-usage result)
                     (provider-result-turn-state result)
                     (find-if
                      (lambda (event)
                        (typep event 'assistant-delta-event))
                      events)
                     (find-if
                      (lambda (event)
                        (typep event 'reasoning-delta-event))
                      events)
                     (find-if
                      (lambda (event)
                        (typep event 'provider-item-event))
                      events))
                "successful containment retains each semantic provider channel"))
             (let* ((source
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
                          (oauth-credentials-refresh-token credentials)
                          "request_id"
                          (oauth-credentials-account-id credentials))))))
                    (condition
                      (handler-case
                          (progn
                            (attempt
                             (lambda ()
                               (values
                                (make-instance
                                 'test-character-input-stream
                                 :source source)
                                200
                                nil))
                             #'identity)
                            nil)
                        (provider-error (failure)
                          failure))))
               (provider-tests--assert-credential-free
                condition
                secrets
                "structured provider failures contain no credential")
               (test-assert
                (test-object-contains-string-p
                 condition
                 *provider-credential-redaction-marker*)
                "structured provider failures retain a redaction marker"))
             (let* ((source
                      (format
                       nil
                       "data: {\"type\":\"~A~%~%"
                       (oauth-credentials-access-token credentials)))
                    (condition
                      (handler-case
                          (progn
                            (attempt
                             (lambda ()
                               (values
                                (make-instance
                                 'test-character-input-stream
                                 :source source)
                                200
                                (list
                                 (cons
                                  "x-request-id"
                                  (oauth-credentials-id-token credentials)))))
                             #'identity)
                            nil)
                        (provider-error (failure)
                          failure))))
               (provider-tests--assert-credential-free
                condition
                secrets
                "malformed provider events contain no credential"))
             (dolist (signaled-p '(nil t))
               (let ((condition
                       (handler-case
                           (progn
                             (attempt
                              (lambda ()
                                (let ((headers
                                        (list
                                         (cons
                                          "x-request-id"
                                          (oauth-credentials-id-token
                                           credentials))))
                                      (body
                                        (json-encode
                                         (json-object
                                          "error"
                                          (json-object
                                           "message"
                                           (oauth-credentials-refresh-token
                                            credentials))))))
                                  (if signaled-p
                                      (error
                                       (make-condition
                                        'http-request-failed
                                        :body    body
                                        :status  400
                                        :headers headers
                                        :uri     nil
                                        :method  ':post))
                                      (values
                                       (make-string-input-stream body)
                                       400
                                       headers))))
                              #'identity)
                             nil)
                         (provider-error (failure)
                           failure))))
                 (provider-tests--assert-credential-free
                  condition
                  secrets
                  "HTTP provider failures contain no credential")
                 (test-assert
                  (test-object-contains-string-p
                   condition
                   *provider-credential-redaction-marker*)
                  "HTTP credential echoes carry a redaction marker")))
             (let* ((collision "PROVIDER")
                    (marker
                      (safe-redaction-marker
                       *provider-credential-redaction-marker*
                       (list collision))))
               (test-assert
                (not (search collision marker))
                "credential collisions select a marker without the credential")))
        (platform-delete-directory-tree
         *platform*
         root :validate t :if-does-not-exist ':ignore)))
    (test-assert
     (and (null *provider-active-credential-values*)
          (null *provider-active-credential-redaction-marker*))
     "provider attempts retain no dynamic credential redaction state"))
  nil)

(defclass test-codex-provider (codex-subscription-provider)
  ((outcomes
    :initarg       :outcomes
    :accessor      test-codex-provider-outcomes
    :type          list
    :documentation "The attempt outcomes returned in order.")
   (refresh-flags
    :initform      nil
    :accessor      test-codex-provider-refresh-flags
    :type          list
    :documentation "The force-refresh values observed by attempts."))
  (:documentation "A direct-provider test double for bounded authentication retries."))

(defmethod provider-attempt-turn
    ((provider test-codex-provider)
     (conversation conversation)
     &key
     tool-namespaces
     event-callback
     force-refresh
     goal-context
     compaction-p)
  "Return the next scripted PROVIDER outcome and record FORCE-REFRESH."
  (declare (ignore conversation tool-namespaces event-callback goal-context
                   compaction-p))
  (push force-refresh (test-codex-provider-refresh-flags provider))
  (let ((outcome (pop (test-codex-provider-outcomes provider))))
    (cond
      ((typep outcome 'provider-result)
       outcome)
      ((typep outcome 'provider-unauthorized)
       (error outcome))
      ((eq outcome :unauthorized)
       (error 'provider-unauthorized
              :message    "The provider returned HTTP 401. Provider explanation: Upstream account unavailable."
              :status     401
              :code       "missing_upstream_credential"
              :request-id "request-authentication"
              :response   nil))
      ((eq outcome :stream-error)
       (error 'response-stream-error
              :message    "Injected stream interruption."
              :status     nil
              :request-id nil
              :response   nil))
      ((eq outcome :server-error)
       (error 'provider-retryable-error
              :message     "Injected transient server failure."
              :status      nil
              :code        "server_error"
              :request-id  "request-server-error"
              :response-id "response-server-error"
              :response    nil))
      ((eq outcome :overloaded)
       (error 'provider-retryable-error
              :message     "Injected provider overload."
              :status      nil
              :code        "server_is_overloaded"
              :request-id  "request-overloaded"
              :response-id "response-overloaded"
              :response    nil))
      ((eq outcome :slow-down)
       (error 'provider-retryable-error
              :message     "Injected provider slowdown."
              :status      nil
              :code        "slow_down"
              :request-id  "request-slow-down"
              :response-id "response-slow-down"
              :response    nil))
      ((eq outcome :resample)
       (error 'provider-resample-requested
              :message          "Injected provider loop report."
              :status           nil
              :request-id       nil
              :response         nil
              :triggers         (list "tail_repetition:4@thinking")
              :attempt          1
              :maximum-attempts 2))
      (t
       (error "Invalid scripted provider outcome ~S." outcome)))))

(-> test-codex-provider-create (configuration list) test-codex-provider)
(defun test-codex-provider-create (configuration outcomes)
  "Return a test direct provider yielding OUTCOMES."
  (make-instance 'test-codex-provider
                 :configuration      configuration
                 :credential-manager (credential-manager-create configuration)
                 :session-id         (make-identifier)
                 :outcomes           outcomes))

(-> provider-tests--authentication-hook (subscription-provider) null)
(defun provider-tests--authentication-hook (provider)
  "Exercise Dexador condition bodies at the host hook, including hostile fields."
  (let* ((body     "{\"error\":{\"message\":\"Account refused\",\"code\":\"invalid_api_key\"}}")
         (stream   (make-string-input-stream body))
         (fallback (format nil "The provider rejected the current ~A credentials."
                           (provider-account-label provider))))
    (dolist (case (list (list body "Account refused" "invalid_api_key")
                       (list (sb-ext:string-to-octets body) "Account refused" "invalid_api_key")
                       (list stream "Account refused" "invalid_api_key")
                       (list (format nil "{\"error\":{\"message\":~S,\"code\":\"invalid_api_key\"}}"
                                     fallback) fallback "invalid_api_key")
                       (list "{broken" nil nil) (list "{}" nil nil)
                       (list "{\"error\":{\"message\":[],\"code\":{}}}" nil nil)
                       (list "{\"error\":{\"code\":\"scope_missing\"}}" nil "scope_missing")))
      (let* ((condition (make-condition 'http-request-failed
                                       :status  401
                                       :body    (first case)
                                       :headers '(("x-request-id" . "auth-hook-request"))
                                       :uri     nil
                                       :method  ':post))
             (failure   (handler-case
                            (provider-signal-transport-failure provider condition)
                          (provider-unauthorized (failure)
                            failure)))
             (terminal  (provider--authentication-condition provider failure)))
        (test-assert (typep failure 'provider-unauthorized)
                     "host401 remains catchable by bounded credential refresh")
        (test-assert (and (= (provider-error-status failure) 401)
                          (equal (provider-error-request-id failure) "auth-hook-request")
                          (null (provider-error-response failure)))
                     "host401 preserves status and request-id without its body")
        (test-assert
         (and (equal (cl-llm-provider-api:provider-api-error-message failure)
                     (or (second case) fallback))
              (search (or (second case) fallback) (princ-to-string failure))
              (equal (provider-error-code failure) (third case)))
         "only valid JSON string fields replace the generic401 explanation")
        (test-assert
         (and (search "HTTP 401" (princ-to-string terminal))
              (search "auth-hook-request" (princ-to-string terminal))
              (if (second case)
                  (and (not (typep failure 'provider--unauthorized-without-message))
                       (search (second case) (princ-to-string terminal)))
                  (and (typep failure 'provider--unauthorized-without-message)
                       (not (search fallback (princ-to-string terminal)))))
              (or (null (third case)) (search (third case) (princ-to-string terminal))))
         "render body messages and codes, but omit synthesized fallback text")))
    (test-assert (not (open-stream-p stream)) "the error-body stream closes after one drain"))
  (let* ((*provider-active-credential-values* '("public-credential" "public-extra"))
         (body                                (copy-seq "{\"error\":{\"message\":\"public\\u002dcredential\\npublic-extra\\u001b\",\"code\":\"public\\u002dcredential\"}}"))
         (id                                  (copy-seq (format nil "public-credential~%public-extra")))
         (failure
           (handler-case
               (provider-signal-transport-failure
                provider (make-condition 'http-request-failed
                                         :status  401
                                         :body    body
                                         :headers (list (cons "x-request-id" id))
                                         :uri     nil
                                         :method  ':post))
             (provider-unauthorized (condition)
               condition)))
         (terminal                            (provider--authentication-condition provider failure))
         (report                              (princ-to-string terminal)))
    (test-assert (and (not (search "public-credential" report))
                      (not (search "public-extra" report))
                      (notany (lambda (character) (< (char-code character) 32)) report))
                 "decoded fields and request identity redact active secrets and controls")
    (fill body #\x)
    (fill id #\x)
    (test-assert (and (equal report (princ-to-string terminal))
                      (null (provider-error-response terminal)))
                 "the terminal report detaches from mutable wire strings"))
  (let* ((body (format nil "{\"error\":{\"message\":\"~A\",\"code\":\"~A\"}}"
                       (make-string 1600 :initial-element #\m)
                       (make-string 500 :initial-element #\c)))
         (failure
           (handler-case
               (provider-signal-transport-failure
                provider (make-condition 'http-request-failed
                                         :status 401 :body body :headers nil
                                         :uri nil :method ':post))
             (provider-unauthorized (condition)
               condition))))
    (test-assert
     (and (<= (length (cl-llm-provider-api:provider-api-error-message failure))
              *provider-error-detail-limit*)
          (<= (length (provider-error-code failure)) 80)
          (null (provider-error-request-id failure)))
     "message and code are bounded while absent request identity stays absent")
    (test-assert
     (not (search "request NIL" (princ-to-string
                                 (provider--authentication-condition provider failure))))
     "missing diagnostic fields add no fabricated value"))
  nil)

(-> test-provider-authentication-retries () null)
(defun test-provider-authentication-retries ()
  "Test bounded refresh and detached terminal authentication diagnostics."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation  (conversation-create configuration :identifier "provider-retry"))
         (result
           (make-instance 'provider-result
                          :response-id  "retry-success"
                          :output-items nil
                          :tool-calls   nil
                          :usage        nil
                          :turn-state   nil)))
    (unwind-protect
         (progn
           (provider-tests--authentication-hook
            (test-codex-provider-create configuration nil))
           (let ((provider
                   (test-codex-provider-create
                    configuration
                    (list :unauthorized result)))
                 (events nil))
             (test-assert
              (eq (provider-stream-turn provider conversation
                                        :tool-namespaces #()
                                        :event-callback  (lambda (event) (push event events)))
                  result)
              "one forced refresh may satisfy the first unauthorized response")
             (test-assert
              (equal (nreverse (test-codex-provider-refresh-flags provider))
                     '(nil t))
              "the first retry forces credential refresh")
             (test-assert
              (and (every (lambda (event) (typep event 'provider-connection-event)) events)
                   (equal (provider-tests--connection-phases events) '(:attempting :completed))
                   (every (lambda (event)
                            (= 1 (getf (provider-connection-event-details event) :attempt)))
                          events))
              "credential refresh adds only the one logical connection lifecycle"))
           (dolist (case '((nil nil) (t nil) (nil t) (t t)))
             (let* ((refreshable-p (first case))
                    (missing-p     (second case))
                    (message       (copy-seq "Upstream account unavailable."))
                    (code          (copy-seq "missing_upstream_credential"))
                    (request-id    (copy-seq "request-authentication"))
                    (original
                      (make-condition (if missing-p
                                          'provider--unauthorized-without-message
                                          'provider-unauthorized)
                                      :message    message
                                      :status     401
                                      :code       code
                                      :request-id request-id
                                      :response   (list :discarded-body)))
                    (provider
                      (if refreshable-p
                          (test-codex-provider-create
                           configuration (list :unauthorized original :unexpected))
                          (make-instance
                           'test-codex-provider
                           :configuration      configuration
                           :credential-manager (api-key-credential-manager-create
                                                :provider-name "retry-api-key"
                                                :pathname      (configuration-api-keys-path configuration))
                           :session-id         (make-identifier)
                           :outcomes           (list original :unexpected))))
                    (events        nil)
                    (terminal
                      (handler-case
                          (provider-stream-turn
                           provider conversation
                           :tool-namespaces #()
                           :event-callback  (lambda (event) (push event events)))
                        (authentication-error (condition)
                          condition))))
               (test-assert (typep terminal 'authentication-error)
                            "exhausted unauthorized responses become authentication failures")
               (test-assert
                (and (not (typep terminal 'provider-error))
                     (not (typep terminal 'provider-retryable-error)))
                "terminal authentication is outside provider retry classification")
               (test-assert
                (equal (nreverse (test-codex-provider-refresh-flags provider))
                       (if refreshable-p '(nil t) '(nil)))
                "only refreshable credentials receive one forced refresh")
               (test-assert
                (and (every (lambda (event) (typep event 'provider-connection-event)) events)
                     (equal (provider-tests--connection-phases events) '(:attempting :terminated))
                     (every (lambda (event)
                              (= 1 (getf (provider-connection-event-details event) :attempt)))
                            events))
                "authentication exhaustion adds no retry, failure or waiting events")
               (let* ((report      (princ-to-string terminal))
                      (preserved-p (if missing-p
                                       (not (search message report))
                                       (search message report))))
                 (test-assert preserved-p "terminal authentication renders only provider body messages")
                 (test-assert
                  (equal (cl-llm-provider-api:provider-api-error-message original) message)
                  "the source condition keeps its own message for other callers")
                 (test-assert
                  (and (search (if refreshable-p
                                   "rejected Autolith's credentials after a bounded refresh."
                                   "rejected Autolith's API key;") report)
                       (or refreshable-p (search (credential-manager-login-hint
                                                  (provider-credential-manager provider)) report))
                       (search "HTTP 401" report)
                       (search code report) (search request-id report))
                  "both auth hints survive alongside rendered provider diagnostics")
                 (test-assert
                  (and preserved-p (= (provider-error-status terminal) 401))
                  "terminal authentication preserves HTTP status")
                 (test-assert
                  (and preserved-p (equal (provider-error-code terminal) code))
                  "terminal authentication preserves provider code")
                 (test-assert
                  (and preserved-p (equal (provider-error-request-id terminal) request-id))
                  "terminal authentication preserves request identity")
                 (test-assert
                  (and preserved-p (null (provider-error-response terminal))
                       (null (provider-error-response-id terminal)))
                  "terminal authentication retains no provider response or response identity")
                 (test-assert
                  (and preserved-p
                       (equal (cl-llm-provider-api:provider-api-error-message terminal) report)
                       (equal (autolith-error-message terminal) report))
                  "diagnostic readers and rendering expose the same actionable report")
                 (fill message #\x)
                 (fill code #\x)
                 (fill request-id #\x)
                 (test-assert
                  (and preserved-p
                       (equal (princ-to-string terminal) report)
                       (equal (provider-error-code terminal) "missing_upstream_credential")
                       (equal (provider-error-request-id terminal) "request-authentication"))
                  "terminal diagnostic strings are detached from the original condition")))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> provider-tests--stream-failure () condition)
(defun provider-tests--stream-failure ()
  "Return a retryable stream failure carrying identifiers for the audit trail."
  (make-condition 'response-stream-error
                  :message     "Injected stream interruption."
                  :status      nil
                  :code        nil
                  :request-id  "req-stream"
                  :response-id "resp-stream"
                  :response    nil))

(-> test-agent-attempt-failure-metadata () null)
(defun test-agent-attempt-failure-metadata ()
  "Test failed provider attempts persist an audit record under their request."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation
                  (conversation-create configuration :identifier "attempt-audit"))
                (statuses nil)
                (observer (make-instance 'callback-agent-observer
                                         :status-callback
                                         (lambda (status details)
                                           (push (list status details) statuses)
                                           nil)))
                (callback (agent--provider-event-callback
                           observer
                           :conversation   conversation
                           :request-number 4)))
           (funcall callback
                    (make-instance 'provider-attempt-failed-event
                                   :attempt           2
                                   :elapsed-seconds   17
                                   :output-received-p t
                                   :retryable-p       t
                                   :condition         (provider-tests--stream-failure)))
           (let ((records nil))
             (conversation-map-records
              conversation
              (lambda (record)
                (when (eq (first record) ':provider)
                  (push (getf (rest record) :metadata) records))))
             (let ((metadata (first records)))
               (test-assert
                (and (= (length records) 1)
                     (= (getf metadata :request-number) 4)
                     (= (getf metadata :attempt) 2)
                     (= (getf metadata :elapsed-seconds) 17)
                     (eq (getf metadata :output-received-p) t)
                     (eq (getf metadata :retryable-p) t)
                     (string= (getf (getf metadata :failure) :request-id)
                              "req-stream")
                     (string= (getf (getf metadata :failure) :response-id)
                              "resp-stream"))
                "a failed attempt persists its audit record under the request number")))
           (test-assert
            (and (= (length statuses) 1)
                 (eq (first (first statuses)) ':provider-attempt-failed)
                 (= (getf (second (first statuses)) :attempt) 2))
            "a failed attempt is reported to the observer"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> provider-tests--connected-stream-pair () (values stream t t t))
(defun provider-tests--connected-stream-pair ()
  "Return a loopback character stream plus the sockets holding it open."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type     ':stream
                                 :protocol ':tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
    (sb-bsd-sockets:socket-bind listener
                                (sb-bsd-sockets:make-inet-address "127.0.0.1")
                                0)
    (sb-bsd-sockets:socket-listen listener 1)
    (multiple-value-bind (address port) (sb-bsd-sockets:socket-name listener)
      (let ((client (make-instance 'sb-bsd-sockets:inet-socket
                                   :type     ':stream
                                   :protocol ':tcp)))
        (sb-bsd-sockets:socket-connect client address port)
        (let ((accepted (sb-bsd-sockets:socket-accept listener)))
          (values (sb-bsd-sockets:socket-make-stream client
                                                     :input        t
                                                     :output       t
                                                     :element-type 'character)
                  client
                  accepted
                  listener))))))

(-> provider-tests--dexador-call-p (t) boolean)
(defun provider-tests--dexador-call-p (form)
  "Return true when FORM is a direct Dexador request call."
  (and (consp form)
       (symbolp (first form))
       (let ((package (symbol-package (first form))))
         (and package
              (string= (package-name package) "DEXADOR")
              (member (symbol-name (first form)) '("GET" "POST" "REQUEST")
                      :test #'string=)))
       t))

(-> provider-tests--response-deadline-form-p (t) boolean)
(defun provider-tests--response-deadline-form-p (form)
  "Return true when FORM establishes the hard provider response deadline."
  (and (consp form)
       (not (null (member (first form)
                          '(provider-call-with-response-deadline
                            provider--call-opening))))))

(-> provider-tests--audit-dexador-deadlines () (values integer list))
(defun provider-tests--audit-dexador-deadlines ()
  "Inspect every provider source form and report Dexador calls outside a deadline."
  (let ((count      0)
        (violations nil)
        (root       (merge-pathnames "src/provider/"
                                     (asdf:system-source-directory :autolith))))
    (labels ((walk (form deadline-p pathname)
               "Walk FORM under DEADLINE-P, recording violations from PATHNAME."
               (when (consp form)
                 (let ((nested-deadline-p
                         (or deadline-p
                             (provider-tests--response-deadline-form-p form))))
                   (when (provider-tests--dexador-call-p form)
                     (incf count)
                     (unless nested-deadline-p
                       (push (list (enough-namestring pathname root)
                                   (first form))
                             violations)))
                   (walk (first form) nested-deadline-p pathname)
                   (walk (rest form) nested-deadline-p pathname)))))
      (dolist (pathname (directory (merge-pathnames "**/*.lisp" root)))
        (with-open-file (stream pathname :direction ':input)
          (let ((*read-eval* nil)
                (*package*   (find-package '#:autolith)))
            (loop for form = (read stream nil stream)
                  until (eq form stream)
                  do (walk form nil pathname))))))
    (values count (nreverse violations))))

(-> test-provider-response-deadlines () null)
(defun test-provider-response-deadlines ()
  "Test response deadlines, diagnostic body draining, and transport routing."
  (multiple-value-bind (call-count violations)
      (provider-tests--audit-dexador-deadlines)
    (test-assert (plusp call-count)
                 "the provider transport audit finds Dexador call sites")
    (test-assert (null violations)
                 (format nil "every provider Dexador call has a hard response deadline: ~S"
                         violations)))
  (multiple-value-bind (stream client accepted listener)
      (provider-tests--connected-stream-pair)
    (unwind-protect
         (let ((*provider-error-body-deadline-seconds* 1)
               (started                                (get-universal-time)))
           (test-assert (null (provider--drain-error-body stream))
                        "a silent optional error body produces no diagnostic text")
           (test-assert (< (- (get-universal-time) started) 30)
                        "optional error-body draining stops at its short deadline")
           (test-assert (not (open-stream-p stream))
                        "optional error-body draining closes the response stream"))
      (ignore-errors (close stream))
      (ignore-errors (sb-bsd-sockets:socket-close accepted))
      (ignore-errors (sb-bsd-sockets:socket-close client))
      (ignore-errors (sb-bsd-sockets:socket-close listener))))
  (multiple-value-bind (stream client accepted listener)
      (provider-tests--connected-stream-pair)
    (let* ((configuration                          (test-configuration))
           (root                                   (test-configuration-root configuration))
           (provider                               (provider-create configuration))
           (*provider-error-body-deadline-seconds* 1)
           (condition
             (make-condition 'http-request-failed
                             :body    stream
                             :status  500
                             :headers nil
                             :uri     nil
                             :method  ':post)))
      (unwind-protect
           (test-assert
            (handler-case
                (progn
                  (provider-signal-http-failure provider condition)
                  nil)
              (provider-retryable-error (failure)
                (= (provider-error-status failure) 500)))
            "a body-drain deadline preserves the originating HTTP status failure")
        (ignore-errors (close stream))
        (ignore-errors (sb-bsd-sockets:socket-close accepted))
        (ignore-errors (sb-bsd-sockets:socket-close client))
        (ignore-errors (sb-bsd-sockets:socket-close listener))
        (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))))
  (let ((stream
          (make-instance 'test-failing-close-stream
                         :source "{\"error\":\"synthetic\"}")))
    (test-call-with-function-replacements
     (list
      (list
       'dexador:post
       (lambda (&rest arguments)
         (declare (ignore arguments))
         (error 'http-request-failed
                :body    stream
                :status  500
                :headers nil
                :uri     nil
                :method  ':post))))
     (lambda ()
       (multiple-value-bind (body status headers)
           (credential-manager-refresh-request
            (make-instance 'grok-credential-manager
                           :primary-source
                           (make-instance 'autolith-credential-source
                                          :pathname #P"/tmp/grok-auth.sexp"))
            :url     "https://provider.invalid/oauth"
            :headers nil
            :content "")
         (declare (ignore headers))
         (test-assert
          (and (= status 500)
               (search "synthetic" body)
               (test-failing-close-stream-close-abort-p stream))
          "OAuth refresh drains and abort-closes a streamed error body")))))
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (manager
           (api-key-credential-manager-create
            :provider-name "deadline-model-errors"
            :pathname      (configuration-api-keys-path configuration)))
         (stream
           (make-instance 'test-failing-close-stream
                          :source "{\"error\":\"synthetic\"}")))
    (unwind-protect
         (progn
           (api-key-credential-manager-save-key manager "deadline-key")
           (test-call-with-function-replacements
            (list
             (list
              'dexador:get
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (error 'http-request-failed
                       :body    stream
                       :status  500
                       :headers nil
                       :uri     nil
                       :method  ':get))))
            (lambda ()
              (test-assert
               (handler-case
                   (progn
                     (openai-compatible--fetch-models
                      configuration
                      :provider-name      "deadline-model-errors"
                      :endpoint           "https://provider.invalid/models"
                      :headers            nil
                      :credential-manager manager)
                     nil)
                 (configuration-error ()
                   t))
               "model discovery preserves typed HTTP failure mapping")))
           (test-assert (test-failing-close-stream-close-abort-p stream)
                        "model discovery drains and abort-closes a streamed error body"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (credentials
           (make-instance 'oauth-credentials
                          :access-token  "deadline-access"
                          :refresh-token "deadline-refresh"
                          :account-id    "deadline-account"
                          :expires-at    nil))
         (grok-manager
           (make-instance
            'grok-credential-manager
            :primary-source
            (make-instance 'autolith-credential-source
                           :pathname (configuration-grok-auth-path configuration)))))
    (unwind-protect
         (test-call-with-function-replacements
          (list
           (list
            'provider-call-with-response-deadline
            (lambda (seconds function)
              (declare (ignore seconds function))
              (error 'sb-sys:deadline-timeout))))
          (lambda ()
            (flet ((signals-type-p (thunk type)
                     (handler-case
                         (progn (funcall thunk) nil)
                       (condition (condition)
                         (typep condition type)))))
              (dolist (thunk
                       (list
                        (lambda () (anthropic-validate-api-key "key"))
                        (lambda () (fireworks-validate-api-key "key"))
                        (lambda () (openrouter-validate-api-key "key"))
                        (lambda () (mistral-validate-api-key "key"))
                        (lambda ()
                          (chatgpt-oauth--request
                           :url "https://provider.invalid/oauth" :content ""))
                        (lambda ()
                          (gemini-oauth--request
                           :url "https://provider.invalid/oauth" :content ""))))
                (test-assert (signals-type-p thunk 'authentication-error)
                             "authentication response deadlines map to typed failures"))
              (test-assert
               (signals-type-p
                (lambda ()
                  (credential-manager-refresh-exchange
                   grok-manager credentials "deadline-refresh"))
                'token-refresh-failed)
               "OAuth refresh deadlines map to a typed token failure"))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation  (conversation-create configuration :identifier "response-deadlines"))
         (credentials
           (make-instance 'oauth-credentials
                          :access-token  "deadline-access"
                          :refresh-token "deadline-refresh"
                          :account-id    "deadline-account"
                          :expires-at    (+ (get-universal-time) 3600)))
         (codex         (provider-create configuration))
         (openai
           (openai-compatible-provider-create
            configuration
            :name                "deadline-openai"
            :family              ':openai
            :headers             nil
            :reasoning-parameter nil
            :stream-usage-p      t))
         (anthropic     (anthropic-provider-create configuration))
         (gemini
           (gemini-code-assist-provider-create
            configuration
            :credential-manager (credential-manager-create configuration)))
         (model-manager
           (api-key-credential-manager-create
            :provider-name "deadline-models"
            :pathname      (configuration-api-keys-path configuration)))
         (deadlines     nil))
    (unwind-protect
         (progn
           (api-key-credential-manager-save-key model-manager "deadline-key")
           (setf (gemini-code-assist-provider-project gemini) "deadline-project"
                 (gemini-code-assist-provider-setup-complete-p gemini) t)
           (test-call-with-function-replacements
            (list
             (list
              'provider-call-with-response-deadline
              (lambda (seconds function)
                (push seconds deadlines)
                (funcall function)))
             (list
              'dexador:post
              (lambda (url &rest arguments)
                (declare (ignore url))
                (if (getf arguments :want-stream)
                    (values (make-string-input-stream "") 200 nil)
                    (values "{}" 200 nil))))
             (list
              'dexador:get
              (lambda (url &rest arguments)
                (declare (ignore url arguments))
                (values "{\"data\":[]}" 200 nil))))
            (lambda ()
              (provider-open-response-stream
               codex (json-object) :credentials credentials :conversation conversation)
              (provider-open-native-compaction
               codex (json-object) :credentials credentials :conversation conversation)
              (openai-compatible--fetch-models
               configuration
               :provider-name      "deadline-models"
               :endpoint           "https://provider.invalid/models"
               :headers            nil
               :credential-manager model-manager)
              (provider-open-response-stream
               openai (json-object) :credentials credentials :conversation conversation)
              (provider-open-response-stream
               anthropic (json-object) :credentials credentials :conversation conversation)
              (gemini-code-assist--post-once gemini credentials
                                             :method  "loadCodeAssist"
                                             :request (json-object))
              (gemini-code-assist--get-once gemini credentials "operations/deadline")
              (provider-open-response-stream
               gemini (json-object) :credentials credentials :conversation conversation)))
           (test-assert
            (equal (nreverse deadlines) '(300 300 30 300 300 300 300 300))
            "every provider response and stream-open path uses its response deadline"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-provider-stream-retries () null)
(defun test-provider-stream-retries ()
  "Test bounded stream reconnection, observer events, and final failure."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "provider-stream-retry"))
         (result
           (make-instance 'provider-result
                          :response-id  "stream-retry-success"
                          :output-items nil
                          :tool-calls   nil
                          :usage        nil
                          :turn-state   nil)))
    (unwind-protect
         (let ((*bounded-retry-sleep-function*
                 (lambda (seconds)
                   (declare (ignore seconds)))))
           (dolist (failure
                    '(:stream-error :server-error :overloaded :slow-down))
             (let ((events nil)
                   (provider
                     (test-codex-provider-create
                      configuration
                      (list failure result))))
               (test-assert
                (eq (provider-stream-turn
                     provider
                     conversation
                     :tool-namespaces #()
                     :event-callback  (lambda (event) (push event events)))
                    result)
                "transient stream and server failures retry the provider turn")
               (if (eq failure :stream-error)
                   (provider-tests--assert-persistent-reconnect events)
                   (let* ((retry-events
                            (reverse
                             (remove-if-not (lambda (event)
                                              (typep event 'provider-retry-event))
                                            events)))
                          (announcement (first retry-events))
                          (in-flight    (second retry-events)))
                     (test-assert (= (length retry-events) 2)
                                  "semantic failures emit exactly one bounded retry pair")
                     (test-assert
                      (and announcement
                           (= (provider-retry-event-attempt announcement) 1)
                           (= (provider-retry-event-maximum-attempts announcement) 6)
                           (<= 1 (provider-retry-event-delay announcement) 60))
                      "provider retries expose their attempt and delay to the observer")
                     (test-assert
                      (and in-flight
                           (= (provider-retry-event-attempt in-flight) 1)
                           (= (provider-retry-event-maximum-attempts in-flight) 6)
                           (zerop (provider-retry-event-delay in-flight)))
                      "the reconnect attempt itself reports no remaining countdown")
                     (test-assert
                      (equal (provider-tests--connection-phases events)
                             '(:attempting :attempting :completed))
                      "semantic failures never enter persistent waiting")))
               (test-assert
                (equal (nreverse
                        (test-codex-provider-refresh-flags provider))
                       '(nil nil))
                "provider retries do not force an authentication refresh")))
           ;; A later attempt without output must not erase earlier generation.
           (let* ((*provider-maximum-transient-retries* 1)
                  (events                               nil)
                  (provider
                    (provider-tests--transport-provider
                     configuration
                     (list (make-instance
                            'test-tls-eof-stream
                            :source (concatenate
                                     'string
                                     (test-sse-event-string
                                      (json-object "type" "response.created"
                                                   "response" (json-object "id" "partial")))
                                     (test-sse-event-string
                                      (json-object "type" "response.output_text.delta"
                                                   "delta" "partial"))))
                           (provider-tests--tls-eof-condition)
                           (make-instance 'test-character-input-stream
                                          :source (provider-tests--completed-sse-source "forbidden"))))))
             (credential-source-save
              (credential-manager-primary-source (provider-credential-manager provider))
              (provider-tests--credentials configuration))
             (test-assert
              (handler-case
                  (progn (provider-stream-turn
                          provider conversation
                          :tool-namespaces #()
                          :event-callback  (lambda (event) (push event events)))
                         nil)
                (provider-retryable-error () t))
              "generated transport retries abandon the request at the bounded limit")
             (test-assert (= 2 (test-transport-provider-attempt-count provider))
                          "cumulative generation prevents an unbounded third transport")
             (test-assert
              (equal (provider-tests--connection-phases events)
                     '(:attempting :attempting :terminated))
              "later output-free failure never reenters persistent waiting")
             (test-assert
              (and (= 2 (count-if (lambda (event) (typep event 'provider-retry-event)) events))
                   (= 1 (count-if (lambda (event)
                                    (typep event 'provider-retries-exhausted-event)) events)))
              "the bounded announcement, countdown and exhaustion are retained"))
           (let ((delays nil)
                 (provider
                   (test-codex-provider-create
                    configuration
                    (list :overloaded :overloaded result))))
             (let ((*bounded-retry-sleep-function*
                     (lambda (seconds)
                       (push seconds delays))))
               (test-assert
                (eq (provider-stream-turn
                     provider
                     conversation
                     :tool-namespaces #()
                     :event-callback  #'identity)
                    result)
                "provider overload retries may recover")
               (test-assert
                (and (= (length delays) 2)
                     (every (lambda (delay) (<= 1 delay 60)) delays))
                "provider overload retries use bounded jittered backoff")))
           (let ((provider
                   (test-codex-provider-create
                    configuration
                    (list :overloaded result))))
             (let ((*bounded-retry-sleep-function*
                     (lambda (seconds)
                       (declare (ignore seconds))
                       (error
                        (make-condition 'application-turn-cancelled)))))
               (test-assert
                (handler-case
                    (progn
                      (provider-stream-turn
                       provider
                       conversation
                       :tool-namespaces #()
                       :event-callback  #'identity)
                      nil)
                  (application-turn-cancelled ()
                    t))
                "turn cancellation interrupts provider overload backoff")
               (test-assert
                (= (length
                    (test-codex-provider-refresh-flags provider))
                   1)
                "turn cancellation prevents another provider attempt")))
           (let ((provider
                   (test-codex-provider-create
                    configuration
                    (append (make-list 7 :initial-element :server-error)
                            (list result)))))
             (test-assert
              (handler-case
                  (progn
                    (provider-stream-turn provider conversation
                                          :tool-namespaces #()
                                          :event-callback  #'identity)
                    nil)
                (provider-retryable-error ()
                  t))
              "transient provider recovery stops after six retries")
             (test-assert
              (= (length (test-codex-provider-refresh-flags provider)) 7)
              "retry exhaustion permits exactly seven total attempts")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-provider-codex-request-headers () null)
(defun test-provider-codex-request-headers ()
  "Test Codex requests carry conversation identity and no routing token."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "provider-headers"))
         (provider      (provider-tests--transport-provider configuration nil))
         (credentials   (provider-tests--credentials configuration)))
    (unwind-protect
         (flet ((header (headers name)
                  (rest (assoc name headers :test #'string=))))
           (let ((headers (provider--codex-request-headers
                           provider credentials conversation
                           :accept "text/event-stream")))
             (test-assert
              (every (lambda (name)
                       (string= (header headers name)
                                (conversation-identifier conversation)))
                     '("session-id" "thread-id" "x-client-request-id"))
              "cache affinity and request identity headers name the conversation")
             (test-assert
              (not (string= (header headers "session-id")
                            (provider-session-id provider)))
              "the per-process provider session identity stays out of the headers")
             (test-assert
              (null (assoc "x-codex-turn-state" headers :test #'string=))
              "the backend's routing token is never replayed")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-provider-usage-limit-terminal () null)
(defun test-provider-usage-limit-terminal ()
  "Test an exhausted ChatGPT allowance fails at once while rate limits still retry."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (provider      (provider-create configuration)))
    (unwind-protect
         (flet ((failure (body)
                  "Return the condition signaled for an HTTP 429 carrying BODY."
                  (handler-case
                      (progn
                        (provider-signal-transport-failure
                         provider
                         (make-condition 'http-request-failed
                                         :body    body
                                         :status  429
                                         :headers '(("x-request-id" . "request-quota")
                                                    ("x-codex-primary-used-percent" . "100"))
                                         :uri     nil
                                         :method  ':post))
                        nil)
                    (provider-error (condition)
                      condition))))
           (let ((cases
                   '(("{\"error\":{\"type\":\"usage_limit_reached\",\"plan_type\":\"plus\",\"resets_at\":1790000000}}"
                      "usage limit was reached" "plus plan" "resets at")
                     ("{\"error\":{\"type\":\"usage_not_included\"}}"
                      "does not include usage of this model" nil nil)
                     ("{\"error\":{\"code\":\"credit_balance_exhausted\",\"message\":\"no credits\"}}"
                      "usage limit was reached" nil nil))))
             (loop for (body . fragments) in cases
                   for condition = (failure body)
                   do (test-assert
                       (and condition
                            (not (typep condition 'provider-retryable-error))
                            (= (provider-error-status condition) 429)
                            (string= (provider-error-request-id condition)
                                     "request-quota")
                            (every (lambda (fragment)
                                     (or (null fragment)
                                         (search fragment
                                                 (autolith-error-message condition))))
                                   fragments))
                       "an exhausted allowance is a terminal, explained failure")))
           (test-assert
            (typep (failure "{\"error\":{\"type\":\"rate_limit_exceeded\",\"message\":\"slow down\"}}")
                   'provider-retryable-error)
            "an ordinary rate limit keeps its bounded retries")
           (test-assert
            (typep (failure "rate limited") 'provider-retryable-error)
            "an unreadable 429 body keeps its bounded retries")
           (test-assert
            (= (getf (getf (provider-rate-limits provider) :primary) :used-percent)
               100)
            "usage-limit responses still record the rate limit snapshot"))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-provider-truncation-recovery () null)
(defun test-provider-truncation-recovery ()
  "Exercise Chat truncation recovery and exhaustion through authenticated requests."
  (with-test-configuration (configuration)
    (let ((snapshot                             (provider--registry-snapshot))
          (*provider-maximum-transient-retries* 6)
          (*provider-maximum-streaming-retries* 2))
      (unwind-protect
           (progn
             (register-openai-compatible-provider
              :name     "truncation-test"
              :endpoint "https://provider.invalid/v1/chat/completions"
              :models   '("retry/chat-model"))
             (openai-compatible-provider-tests--save-key
              configuration "truncation-test" "synthetic-truncation-key")
             (let* ((selected     (configuration-copy configuration :model "retry/chat-model"))
                    (provider     (provider-create selected))
                    (conversation (conversation-create selected :identifier "truncation-recovery")))
               (labels ((source (truncated-p)
                          "Return a complete SSE response, optionally ending in truncation."
                          (format nil "data: ~A~%~%data: [DONE]~%~%"
                                  (json-encode
                                   (json-object
                                    "id" "truncation-response"
                                    "choices"
                                    (json-array
                                     (json-object
                                      "delta"
                                      (if truncated-p
                                          (json-object
                                           "content" "partial"
                                           "tool_calls"
                                           (json-array
                                            (json-object
                                             "index" 0 "id" "partial-call" "type" "function"
                                             "function"
                                             (json-object
                                              "name" (provider-wire-function-name--encode "test" "unwanted")
                                              "arguments" "{\"incomplete\":"))))
                                          (json-object "content" "finished"))
                                      "finish_reason" (if truncated-p "length" "stop"))))))))
                 (dolist (mode '(:recover :exhaust))
                   (let ((attempts 0)
                         (streams  nil)
                         (delays   nil)
                         (result   nil)
                         (failure  nil))
                     (let ((*bounded-retry-sleep-function* (lambda (delay) (push delay delays))))
                       (test-call-with-function-replacements
                        (list
                         (list 'dexador:post
                               (lambda (url &rest arguments)
                                 (declare (ignore url arguments))
                                 (incf attempts)
                                 (let ((stream (make-string-input-stream
                                                (source (or (eq mode ':exhaust) (= attempts 1))))))
                                   (push stream streams)
                                   (values stream 200 '(("x-request-id" . "truncation-request")))))))
                        (lambda ()
                          (handler-case
                              (setf result (provider-stream-turn provider conversation
                                                                 :tool-namespaces #()
                                                                 :event-callback  #'identity))
                            (provider-stream-abandoned (condition)
                              (setf failure condition))))))
                     (test-assert (= attempts (if (eq mode ':recover) 2 3))
                                  "recovery stops at success and exhaustion stops after three attempts")
                     (test-assert (and (= (length delays) (1- attempts))
                                       (every (lambda (delay) (<= 1 delay 60)) delays))
                                  "each permitted retry uses bounded backoff")
                     (test-assert (every (lambda (stream) (not (open-stream-p stream))) streams)
                                  "every attempted response stream is closed")
                     (if (eq mode ':recover)
                         (test-assert (and result (null failure)
                                           (string= (provider-result-assistant-text result) "finished")
                                           (null (provider-result-tool-calls result)))
                                      "only the completed retry exposes output or executable calls")
                         (test-assert (and failure (null result)
                                           (= (provider-stream-abandoned-attempts failure) 3)
                                           (string= (provider-error-request-id failure) "truncation-request"))
                                      "exhaustion returns the typed terminal failure and last request ID")))))))
        (provider--registry-restore snapshot))))
  nil)

(-> test-codex-context-request () null)
(defun test-codex-context-request ()
  "Anchor the Codex goal where it first appeared, moving it only when it changes."
  (with-test-configuration (configuration)
    (let* ((*context-contributors*        nil)
           (*context-resolver*            (cl-llm-provider-api:make-context-resolver))
           (*context-last-deliveries*     (make-hash-table :test #'equal))
           (*context-last-delivery-order* nil)
           (conversation                  (conversation-create configuration))
           (provider                      (provider-create configuration)))
      (conversation-append-user-message conversation "synthetic task")
      (flet ((input (goal)
               (json-get (provider-request-object provider conversation #() :goal-context goal)
                         "input"))
             (history-count ()
               (length (conversation-input-items-for-family conversation
                                                            (provider-family provider)))))
        (let* ((first       (provider-request-object provider conversation #()
                                                     :goal-context "current goal"))
               (first-input (json-get first "input"))
               (cache-key   (json-get first "prompt_cache_key")))
          (test-assert (equalp (aref first-input (1- (length first-input)))
                               (responses-developer-message "current goal"))
                       "a new goal follows the newest history")
          (conversation-append-provider-item
           conversation (json-object "type" "function_call" "name" "sample"
                                     "call_id" "a" "arguments" "{}"))
          (conversation-append-provider-item
           conversation (json-object "type" "function_call_output" "call_id" "a" "output" "one"))
          (let* ((history (json-encode (conversation-input-items-for-family
                                        conversation (provider-family provider))))
                 (count   (history-count))
                 (second  (input "current goal"))
                 (changed (input "updated goal")))
            (test-assert (equalp first-input (subseq second 0 (length first-input)))
                         "a tool round extends the prior input, goal included")
            (test-assert (equal cache-key (json-get (provider-request-object
                                                     provider conversation #()
                                                     :goal-context "current goal")
                                                    "prompt_cache_key"))
                         "context placement preserves root cache identity")
            (test-assert (and (= (length changed) (1+ count))
                              (not (search "current goal" (json-encode changed)))
                              (find (responses-developer-message "updated goal") changed
                                    :test #'equalp)
                              (equal (json-get (aref changed (1- (length changed))) "type")
                                     "function_call_output"))
                         "a changed goal replaces the old one before the complete tool exchange")
            (let ((request (provider-request-object
                            provider conversation #()
                            :goal-context "ignored during compaction" :compaction-p t)))
              (test-assert (not (search "ignored during compaction" (json-encode request)))
                           "ordinary goal context is omitted from compaction"))
            (test-assert (equal history (json-encode (conversation-input-items-for-family
                                                      conversation (provider-family provider))))
                         "request placement does not mutate durable history"))))))
  nil)

(-> test-codex-reasoning-context () null)
(defun test-codex-reasoning-context ()
  "Request reasoning from every turn only for the Codex models that keep it."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration))
          (provider     (provider-create configuration)))
      (conversation-append-user-message conversation "synthetic task")
      (flet ((reasoning-context ()
               (json-get (json-get (provider-request-object provider conversation #())
                                   "reasoning")
                         "context")))
        (test-assert (equal "all_turns" (reasoning-context))
                     "a Codex model marked for it keeps reasoning from every turn")
        (let ((*codex-all-turns-reasoning-models* nil))
          (test-assert (null (reasoning-context))
                       "other models leave reasoning retention to the service")))))
  nil)

(-> test-codex-rendered-context-request () null)
(defun test-codex-rendered-context-request ()
  "Exercise actual goal plus rendered-context ordering and partial omission."
  (with-test-configuration (configuration)
    (let* ((*context-contributors*        nil)
           (*context-resolver*            (cl-llm-provider-api:make-context-resolver))
           (*context-last-deliveries*     (make-hash-table :test #'equal))
           (*context-last-delivery-order* nil)
           (instruction                   "Current request data: λ.")
           (conversation                  (conversation-create configuration))
           (provider                      (provider-create configuration)))
      (register-context-contributor
       "codex-placement-test"
       (lambda (context)
         (declare (ignore context))
         (when instruction
           (make-context-contribution :identifier  "codex-placement-test"
                                      :instruction instruction))))
      (conversation-append-user-message conversation "synthetic task")
      (multiple-value-bind (first delivery)
          (provider-request-object provider conversation #() :goal-context "current goal")
        (let* ((first-input (json-get first "input"))
               (rendered    (context-delivery-rendered delivery)))
          (test-assert
           (string= (json-encode (subseq first-input (- (length first-input) 2)))
                    (json-encode (vector (responses-developer-message "current goal")
                                         (responses-developer-message rendered))))
           "goal precedes rendered context in the actual wire suffix")
          (conversation-append-provider-item
           conversation (json-object "type" "function_call" "name" "sample"
                                     "call_id" "a" "arguments" "{}"))
          (conversation-append-provider-item
           conversation (json-object "type" "function_call_output"
                                     "call_id" "a" "output" "one"))
          (let* ((history (json-encode (conversation-input-items-for-family
                                        conversation (provider-family provider))))
                 (second-input
                   (json-get (provider-request-object provider conversation #()
                                                      :goal-context "current goal")
                             "input")))
            (test-assert
             (string= (json-encode (subseq first-input 0 (- (length first-input) 2)))
                      (json-encode (subseq second-input 0 (- (length first-input) 2))))
             "a tool round keeps the preceding history as the shared prefix")
            (test-assert
             (and (string= (json-encode first-input)
                           (json-encode (subseq second-input 0 (length first-input))))
                  (equal (json-get (aref second-input (1- (length second-input))) "type")
                         "function_call_output"))
             "unchanged goal and rendered context keep their place before the tool round")
            (multiple-value-bind (partial partial-delivery)
                (provider-request-object provider conversation #())
              (let ((input (json-get partial "input")))
                (test-assert
                 (string= (json-encode (aref input 1))
                          (json-encode
                           (responses-developer-message
                            (context-delivery-rendered partial-delivery))))
                 "omitting the goal leaves the rendered context in its place")
                (test-assert (not (search "current goal" (json-encode input)))
                             "partial omission never replays the former goal")))
            (setf instruction nil)
            (let ((input (json-get (provider-request-object provider conversation #()) "input")))
              (test-assert (not (search "Current request data" (json-encode input)))
                           "omitting all context never replays its data"))
            (test-assert
             (string= history (json-encode (conversation-input-items-for-family
                                            conversation (provider-family provider))))
             "rendered context placement leaves durable history unchanged"))))))
  nil)

(-> test-openai-api-anchored-contributions-request () null)
(defun test-openai-api-anchored-contributions-request ()
  "Keep anchored context in its history place while the goal and ordinary context follow."
  (with-test-configuration (configuration)
    (let* ((*context-contributors*        nil)
           (*context-resolver*            (cl-llm-provider-api:make-context-resolver))
           (*context-last-deliveries*     (make-hash-table :test #'equal))
           (*context-last-delivery-order* nil)
           (anchored-p                    t)
           (tail-text                     "Trailing current block.")
           (conversation                  (conversation-create configuration))
           (provider                      (openai-api-provider-create configuration)))
      (register-context-contributor
       "openai-api-anchored-placement-test"
       (lambda (context)
         (declare (ignore context))
         (append
          (when anchored-p
            (list (change-class
                   (make-context-contribution :identifier  "anchored-block"
                                              :instruction "Anchored current block.")
                   'anchored-context-contribution)))
          (when tail-text
            (list (make-context-contribution :identifier  "trailing-block"
                                             :instruction tail-text))))))
      (conversation-append-user-message conversation "synthetic task")
      (labels ((history (request)
                 "Return REQUEST's input after its leading system instructions."
                 (subseq (json-get request "input") 1))

               (input (&optional (goal "current goal"))
                 "Return the input history of the next request with GOAL."
                 (history (provider-request-object provider conversation #()
                                                   :goal-context goal)))

               (tool-round (call-id)
                 "Append one durable tool exchange."
                 (conversation-append-provider-item
                  conversation (json-object "type" "function_call" "name" "sample"
                                            "call_id" call-id "arguments" "{}"))
                 (conversation-append-provider-item
                  conversation (json-object "type" "function_call_output"
                                            "call_id" call-id "output" "one"))))
        (multiple-value-bind (first delivery)
            (provider-request-object provider conversation #() :goal-context "current goal")
          (let ((previous (history first)))
            (test-assert
             (and (= 4 (length previous))
                  (search "Anchored current block" (json-encode (aref previous 1)))
                  (equalp (aref previous 2) (responses-developer-message "current goal"))
                  (search "Trailing current block" (json-encode (aref previous 3)))
                  (search "Anchored current block" (context-delivery-rendered delivery)))
             "the anchored block follows its boundary while the goal and tail end the input")
            (dolist (call-id '("anchored-0" "anchored-1"))
              (tool-round call-id)
              (let ((current (input)))
                (test-assert
                 (string= (json-encode (subseq previous 0 (- (length previous) 2)))
                          (json-encode (subseq current 0 (- (length previous) 2))))
                 "each tool round extends the history through the anchored block")
                (setf previous current)))
            (setf tail-text "Changed trailing block.")
            (let ((changed (input "updated goal")))
              (test-assert
               (and (string= (json-encode (subseq previous 0 (- (length previous) 2)))
                             (json-encode (subseq changed 0 (- (length changed) 2))))
                    (search "Changed trailing block" (json-encode (aref changed (1- (length changed))))))
               "changed ordinary context and goal leave every earlier item in place"))
            (conversation-append-user-message conversation "new instruction")
            (let ((current (input "updated goal")))
              (test-assert
               (and (search "Anchored current block" (json-encode (aref current 1)))
                    (search "new instruction"
                            (json-encode (aref current (- (length current) 3)))))
               "a new user instruction keeps the anchored block and precedes the context"))
            (let ((history (json-encode (conversation-input-items-for-family
                                         conversation (provider-family provider)))))
              (setf anchored-p nil)
              (test-assert (not (search "Anchored current block" (json-encode (input))))
                           "omitting anchored context never replays it")
              (test-assert
               (string= history (json-encode (conversation-input-items-for-family
                                              conversation (provider-family provider))))
               "projection and omission leave durable history untouched")))))))
  nil)

(-> test-codex-tool-round-context-prefix () null)
(defun test-codex-tool-round-context-prefix ()
  "End each Codex tool round with durable history so its whole input is the next prefix."
  (with-test-configuration (configuration)
    (let* ((*context-contributors*        nil)
           (*context-resolver*            (cl-llm-provider-api:make-context-resolver))
           (*context-last-deliveries*     (make-hash-table :test #'equal))
           (*context-last-delivery-order* nil)
           (stable-text                   "Stable current block.")
           (volatile-text                 nil)
           (conversation                  (conversation-create configuration))
           (provider                      (provider-create configuration)))
      (register-context-contributor
       "codex-tool-round-prefix-test"
       (lambda (context)
         (declare (ignore context))
         (append
          (list (change-class
                 (make-context-contribution :identifier  "anchored-block"
                                            :instruction "Anchored current block.")
                 'anchored-context-contribution)
                (make-context-contribution :identifier  "stable-block"
                                           :instruction stable-text
                                           :lifetime    ':turn))
          (when volatile-text
            (list (make-context-contribution :identifier  "volatile-block"
                                             :instruction volatile-text
                                             :lifetime    ':next-request))))))
      (conversation-append-user-message conversation "synthetic task")
      (labels ((input ()
                 "Return the projected Codex input of the next request."
                 (json-get (provider-request-object provider conversation #()
                                                    :goal-context "current goal")
                           "input"))

               (tool-round (call-id)
                 "Append one durable tool exchange."
                 (conversation-append-provider-item
                  conversation (json-object "type" "function_call" "name" "sample"
                                            "call_id" call-id "arguments" "{}"))
                 (conversation-append-provider-item
                  conversation (json-object "type" "function_call_output"
                                            "call_id" call-id "output" "one")))

               (prefix-p (previous current)
                 "Return true when every item of PREVIOUS starts CURRENT unchanged."
                 (and (<= (length previous) (length current))
                      (string= (json-encode previous)
                               (json-encode (subseq current 0 (length previous))))))

               (last-type (input)
                 "Return the wire type of INPUT's final item."
                 (json-get (aref input (1- (length input))) "type")))
        (let ((previous (input)))
          (test-assert
           (and (search "current goal" (json-encode previous))
                (search "Stable current block" (json-encode previous)))
           "the first request of a turn carries the goal and stable context")
          (dolist (call-id '("round-0" "round-1"))
            (tool-round call-id)
            (let ((current (input)))
              (test-assert (and (prefix-p previous current)
                                (equal (last-type current) "function_call_output"))
                           "a tool round sends the previous input unchanged and ends with its output")
              (setf previous current)))
          (setf volatile-text "One request notice.")
          (tool-round "round-2")
          (let ((current (input)))
            (test-assert
             (and (prefix-p previous current)
                  (search "One request notice"
                          (json-encode (aref current (1- (length current))))))
             "one-request context follows the newest tool output")
            (setf previous (subseq current 0 (1- (length current)))))
          (setf volatile-text nil)
          (tool-round "round-3")
          (let ((current (input)))
            (test-assert
             (and (prefix-p previous current)
                  (not (search "One request notice" (json-encode current))))
             "delivered one-request context is never replayed into the prefix")
            (setf previous current))
          (conversation-append-user-message conversation "new instruction")
          (let* ((current (input))
                 (start   (position "new instruction" current
                                    :key  #'json-encode
                                    :test (lambda (text encoded) (search text encoded)))))
            (test-assert
             (and start
                  (prefix-p (subseq previous 0 2) current)
                  (search "Anchored current block" (json-encode (aref current 1)))
                  (search "current goal" (json-encode (subseq current start)))
                  (search "Stable current block" (json-encode (subseq current start)))
                  (= start (- (length previous) 2)))
             "a new user message moves turn context behind it and keeps earlier history")
            (setf previous current))
          (tool-round "round-4")
          (let ((current (input)))
            (test-assert (and (prefix-p previous current)
                              (equal (last-type current) "function_call_output"))
                         "the new turn's tool rounds extend its first request")
            (setf previous current))
          (setf stable-text "Changed stable block.")
          (let* ((history (json-encode (conversation-input-items-for-family
                                        conversation (provider-family provider))))
                 (current (input))
                 (encoded (json-encode current)))
            (test-assert
             (and (not (search "Stable current block" encoded))
                  (search "Changed stable block" encoded)
                  (equal (last-type current) "function_call_output")
                  (search "Anchored current block" (json-encode (aref current 1)))
                  (search "current goal" encoded))
             "changed current state precedes the final tool exchange while anchored context stays")
            (test-assert
             (string= history (json-encode (conversation-input-items-for-family
                                            conversation (provider-family provider))))
             "context placement leaves durable history unchanged"))))))
  nil)

(-> test-codex-resumed-turn-context-prefix () null)
(defun test-codex-resumed-turn-context-prefix ()
  "Keep Codex turn context at its anchors after a mid-turn restart replays the conversation."
  (with-test-configuration (configuration)
    (let* ((*context-contributors*        nil)
           (*context-resolver*            (cl-llm-provider-api:make-context-resolver))
           (*context-last-deliveries*     (make-hash-table :test #'equal))
           (*context-last-delivery-order* nil)
           (conversation                  (conversation-create configuration))
           (provider                      (provider-create configuration)))
      (register-context-contributor
       "codex-resumed-turn-prefix-test"
       (lambda (context)
         (declare (ignore context))
         (list (change-class
                (make-context-contribution :identifier  "anchored-block"
                                           :instruction "Anchored current block.")
                'anchored-context-contribution)
               (make-context-contribution :identifier  "stable-block"
                                          :instruction "Stable current block."
                                          :lifetime    ':turn))))
      (conversation-append-user-message conversation "earlier task")
      (conversation-append-provider-item
       conversation (json-object "type" "message" "role" "assistant"
                                 "content" (json-array (json-object "type" "output_text"
                                                                    "text" "earlier answer"))))
      (conversation-append-user-message conversation "synthetic task")
      (labels ((input (target)
                 "Return the projected Codex input of TARGET's next request."
                 (json-get (provider-request-object provider target #()
                                                    :goal-context "current goal")
                           "input"))

               (tool-round (target call-id)
                 "Append one durable tool exchange to TARGET."
                 (conversation-append-provider-item
                  target (json-object "type" "function_call" "name" "sample"
                                      "call_id" call-id "arguments" "{}"))
                 (conversation-append-provider-item
                  target (json-object "type" "function_call_output"
                                      "call_id" call-id "output" "one")))

               (prefix-p (previous current)
                 "Return true when every item of PREVIOUS starts CURRENT unchanged."
                 (and (<= (length previous) (length current))
                      (string= (json-encode previous)
                               (json-encode (subseq current 0 (length previous))))))

               (occurrences (text input)
                 "Return how many items of INPUT carry TEXT."
                 (count-if (lambda (item) (search text (json-encode item))) input))

               (turn-context-p (input)
                 "Return true when each context block follows the turn's user message once."
                 (let ((start (position-if (lambda (item)
                                             (search "synthetic task" (json-encode item)))
                                           input)))
                   (and start
                        (< (+ start 3) (length input))
                        (search "Anchored current block" (json-encode (aref input (+ start 1))))
                        (search "current goal" (json-encode (aref input (+ start 2))))
                        (search "Stable current block" (json-encode (aref input (+ start 3))))
                        (every (lambda (text) (= 1 (occurrences text input)))
                               '("Anchored current block" "current goal" "Stable current block"))))))
        (input conversation)
        (tool-round conversation "round-0")
        (let* ((previous (input conversation))
               (resumed  (conversation-load (conversation-pathname conversation))))
          (test-assert (turn-context-p previous)
                       "the live turn places its context after its user message")
          (let ((*context-last-deliveries*     (make-hash-table :test #'equal))
                (*context-last-delivery-order* nil))
            (test-assert (string= (json-encode previous) (json-encode (input resumed)))
                         "a resumed conversation rebuilds the interrupted request byte for byte")
            (dolist (call-id '("round-1" "round-2"))
              (tool-round resumed call-id)
              (let ((current (input resumed)))
                (test-assert
                 (and (prefix-p previous current)
                      (turn-context-p current)
                      (equal (json-get (aref current (1- (length current))) "type")
                             "function_call_output"))
                 "each resumed tool round extends the previous input with context in place")
                (setf previous current))))))))
  nil)

(defclass test-error-body-character-stream (test-character-input-stream)
  ((reads :initform 0 :accessor test-error-body-reads
          :documentation "Number of character reads, including EOF probes.")
   (closes :initform 0 :accessor test-error-body-closes
           :documentation "Number of attempted closes.")
   (abort-p :initform nil :accessor test-error-body-abort-p
            :documentation "Whether cleanup requested an abortive close.")
   (failure :initarg :failure :initform nil :reader test-error-body-failure
            :documentation "Synthetic read failure or nonlocal exit.")
   (close-failure :initarg :close-failure :initform nil
                  :reader        test-error-body-close-failure
                  :documentation "Synthetic cleanup failure."))
  (:documentation "An optional diagnostic body with observable read and cleanup behavior."))

(defmethod trivial-gray-streams:stream-read-char
    ((stream test-error-body-character-stream))
  "Count a body read and optionally leave it by a synthetic failure or throw."
  (incf (test-error-body-reads stream))
  (unless (open-stream-p stream) (error "Synthetic body is already closed."))
  (case (test-error-body-failure stream)
    (:error
     (error "Synthetic diagnostic read failure."))
    (:throw
     (throw 'test-error-body-exit :cancelled))
    (:deadline
     (error 'sb-sys:deadline-timeout :seconds 2)))
  (call-next-method))

(defmethod close ((stream test-error-body-character-stream) &key abort)
  "Record abort-close independently of the optional cleanup failure."
  (incf (test-error-body-closes stream))
  (setf (test-error-body-abort-p stream) (not (null abort)))
  (call-next-method)
  (case (test-error-body-close-failure stream)
    (:error
     (error "Synthetic diagnostic close failure."))
    (:deadline
     (error 'sb-sys:deadline-timeout :seconds 2))))

(defclass test-error-body-binary-stream
    (trivial-gray-streams:fundamental-binary-input-stream)
  ((source :initarg :source :reader test-error-body-binary-source
           :documentation "Synthetic UTF-8 or invalid diagnostic octets.")
   (position :initform 0 :accessor test-error-body-binary-position
             :documentation "The next unread byte offset.")
   (closes :initform 0 :accessor test-error-body-binary-closes
           :documentation "Number of attempted closes.")
   (abort-p :initform nil :accessor test-error-body-binary-abort-p
            :documentation "Whether cleanup requested an abortive close."))
  (:documentation "An undecoded diagnostic body for the existing bounded UTF-8 drain."))

(defmethod stream-element-type ((stream test-error-body-binary-stream))
  "Declare the undecoded diagnostic body's byte element type."
  '(unsigned-byte 8))

(defmethod trivial-gray-streams:stream-read-byte
    ((stream test-error-body-binary-stream))
  "Return the next byte or the Gray EOF marker."
  (let ((position (test-error-body-binary-position stream))
        (source   (test-error-body-binary-source stream)))
    (if (< position (length source))
        (prog1 (aref source position)
          (incf (test-error-body-binary-position stream)))
        :eof)))

(defmethod close ((stream test-error-body-binary-stream) &key abort)
  "Record the binary diagnostic body's abort-close."
  (incf (test-error-body-binary-closes stream))
  (setf (test-error-body-binary-abort-p stream) (not (null abort)))
  (call-next-method))

(-> test-provider-usage-limit-streams () null)
(defun test-provider-usage-limit-streams ()
  "Classify HTTP conditions through Codex's retry boundary.

Transport is synthetic; retry delays are suppressed."
  (with-test-configuration (configuration)
    (let ((provider                       (provider-create configuration))
          (*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds))))
          (quota                          "{\"error\":{\"type\":\"usage_limit_reached\",\"code\":\"insufficient_quota\",\"message\":\"Synthetic local quota exhausted\",\"plan_type\":\"stub\"}}")
          (rate                           "{\"error\":{\"type\":\"rate_limit_exceeded\",\"message\":\"synthetic rate detail\"}}"))
      (labels ((body (text representation)
                 (ecase representation
                   (:string
                    text)
                   (:octets
                    (sb-ext:string-to-octets text :external-format :utf-8))
                   (:character
                    (make-instance 'test-error-body-character-stream :source text))
                   (:binary
                    (make-instance 'test-error-body-binary-stream
                                   :source (sb-ext:string-to-octets text :external-format :utf-8)))))

               (failure (content &optional (status 429))
                 (handler-case
                     (provider-signal-transport-failure
                      provider (make-condition 'http-request-failed
                                               :status status :body content :uri nil :method :post
                                               :headers '(("x-request-id" . "synthetic-body-id")
                                                          ("retry-after" . "1"))))
                   (provider-error (condition) condition)))

               (check (condition class)
                 (test-assert (typep condition class) "the original HTTP status selects its typed failure")
                 (test-assert (string= (provider-error-request-id condition) "synthetic-body-id")
                              "body normalization retains request attribution"))

               (closed (stream)
                 (when (streamp stream)
                   (test-assert (not (open-stream-p stream)) "diagnostic body is closed")
                   (etypecase stream
                     (test-error-body-character-stream
                      (test-assert (and (= (test-error-body-closes stream) 1)
                                        (test-error-body-abort-p stream))
                                   "the raw character body is abort-closed once"))
                     (test-error-body-binary-stream
                      (test-assert (and (= (test-error-body-binary-closes stream) 1)
                                        (test-error-body-binary-abort-p stream))
                                   "the raw binary body is abort-closed once"))))))
        (dolist (representation '(:string :octets :character :binary))
          (dolist (text (list quota rate))
            (let ((attempts 0) (streams nil)
                  (quota-p (eq text quota)))
              (let ((condition
                      (handler-case
                          (provider--call-with-bounded-retries
                           provider
                           (lambda (refresh callback)
                             (declare (ignore callback))
                             (test-assert (not refresh) "quota and rate failures do not refresh credentials")
                             (incf attempts)
                             (let ((content (body text representation)))
                               (when (streamp content) (push content streams))
                              (let ((condition (failure content)))
                                ;; Keep this body/cleanup fixture's generated-output safety bound.
                                ;; Pre-generation throttling has its own caller-lifetime witness.
                                (unless quota-p
                                  (setf (slot-value condition 'cl-llm-provider-api::generation-started-p) t))
                                (error condition))))
                           (lambda (event) (declare (ignore event))))
                        (provider-error (condition) condition))))
                (check condition 'provider-error)
                (test-assert (= (provider-error-status condition) 429) "the status survives the body drain")
                (test-assert (eq (not (null (typep condition 'provider-stream-abandoned))) (not quota-p))
                             "only generated throttling reaches the streaming safety limit")
                (test-assert (= attempts (if quota-p 1 (1+ *provider-maximum-streaming-retries*)))
                             "quota is terminal and generated throttling has a finite safety bound")
                (unless quota-p
                  (test-assert (search "synthetic rate detail" (autolith-error-message condition))
                               "fallback retains the normalized body after quota inspection")))
              (dolist (stream streams)
                (closed stream)
                (when (typep stream 'test-error-body-character-stream)
                  (test-assert (<= (test-error-body-reads stream) (+ 2 (length text)))
                               "quota miss never drains the raw stream a second time"))))))
        (dolist (representation '(:octets :character :binary))
          (let* ((text      "{\"error\":{\"type\":\"rate_limit_exceeded\",\"message\":\"synthetic café\"}}")
                 (content   (body text representation))
                 (condition (failure content)))
            (check condition 'provider-retryable-error)
            (test-assert (search "café" (autolith-error-message condition)) "UTF-8 diagnostic detail is retained")
            (closed content)))
        (dolist (content (list nil "" "{" "[]" "null" "{\"error\":42}"
                               "{\"error\":{\"type\":42}}" "usage_limit_reached" 42
                               (make-array 1 :element-type '(unsigned-byte 8) :initial-element 255)))
          (check (failure content) 'provider-retryable-error))
        (dolist (mode '(:error :deadline))
          (let* ((stream    (make-instance 'test-error-body-character-stream :source quota :failure mode))
                 (condition (failure stream)))
            (check condition 'provider-retryable-error)
            (closed stream)))
        (dolist (mode '(:error))
          (let* ((stream    (make-instance 'test-error-body-character-stream :source quota :close-failure mode))
                 (condition (failure stream)))
            (check condition 'provider-error)
            (test-assert (not (typep condition 'provider-retryable-error))
                         "cleanup failure does not mask the terminal allowance classification")
            (closed stream)))
        (let ((stream    (make-instance 'test-error-body-character-stream :source quota :close-failure :deadline))
              (attempts  0)
              (escaped-p nil))
          (handler-case
              (provider--call-with-bounded-retries
               provider
               (lambda (refresh callback)
                 (declare (ignore refresh callback))
                 (incf attempts)
                 (failure stream))
               (lambda (event) (declare (ignore event))))
            (sb-sys:deadline-timeout () (setf escaped-p t)))
          (test-assert escaped-p "a native CLOSE deadline escapes before provider classification")
          (test-assert (= attempts 1) "a native CLOSE deadline is not retried")
          (closed stream))
        (let ((stream (make-instance 'test-error-body-character-stream :source quota :failure :throw)))
          (test-assert (eq (catch 'test-error-body-exit (failure stream)) :cancelled)
                       "a nonlocal body exit is preserved")
          (closed stream))
        (let ((stream (make-instance 'test-error-body-character-stream
                                     :source (concatenate 'string (make-string 4500 :initial-element #\Space) quota))))
          (check (failure stream) 'provider-retryable-error)
          (test-assert (= (test-character-input-stream-position stream) 4000)
                       "optional body drain is bounded")
          (closed stream))
        (let ((stream (make-instance 'test-error-body-binary-stream
                                     :source (make-array 1 :element-type '(unsigned-byte 8) :initial-element 255))))
          (check (failure stream) 'provider-retryable-error)
          (closed stream))
        (let ((stream (make-instance 'test-error-body-character-stream :source quota)))
          (close stream :abort t)
          (check (failure stream) 'provider-retryable-error)
          (test-assert (= (test-error-body-closes stream) 1) "an already closed body is not closed again"))
        (dolist (status '(500 401))
          (let* ((stream (body rate :character)) (condition (failure stream status)))
            (check condition (if (= status 401) 'provider-unauthorized 'provider-retryable-error))
            (test-assert (= (provider-error-status condition) status) "normalization retains nonquota status")
            (closed stream))))))
  nil)

(-> test-provider-transient-event-retries () null)
(defun test-provider-transient-event-retries ()
  "Exercise real SSE classification through the host retry ladder without waiting."
  (with-test-configuration (configuration)
    (let ((provider (test-codex-provider-create configuration nil)))
      (dolist (code '("service_unavailable_error" "overloaded_error" "rate_limit_error"
                      "invalid_request_error" "authentication_error" "invalid_prompt"))
        (dolist (recover-p '(t nil))
          (let* ((attempts                             0)
                 (delays                               nil)
                 (statuses                             nil)
                 (failure                              nil)
                 (result                               nil)
                 (*provider-maximum-transient-retries* 2)
                 (*bounded-retry-sleep-function*
                   (lambda (delay) (push delay delays))))
            (let* ((observer
                     (make-instance 'callback-agent-observer
                                    :status-callback
                                    (lambda (kind details)
                                      (push (cons kind details) statuses))))
                   (callback (agent--provider-event-callback observer)))
              (setf result
                    (handler-case
                        (provider--call-with-bounded-retries
                         provider
                         (lambda (refresh event-callback)
                           (declare (ignore refresh))
                           (incf attempts)
                           (unless recover-p
                             (funcall event-callback (make-instance 'assistant-delta-event :text "owned partial")))
                           (if (and recover-p (> attempts 1))
                               :success
                               (provider-consume-stream
                                provider
                                (make-string-input-stream
                                 (test-sse-event-string
                                  (json-object
                                   "type" "error" "sequence_number" 3
                                   "error" (json-object "type" code "code" :null
                                                        "message" "Temporary provider failure"))))
                                event-callback)))
                         callback)
                      (provider-error (condition)
                        (setf failure condition)
                        nil))))
            (let* ((transient-p       (member code '("service_unavailable_error" "overloaded_error"
                                                     "rate_limit_error") :test #'string=))
                   (expected-attempts (if transient-p (if recover-p 2 3) 1))
                   (exhausted         (assoc :provider-retries-exhausted statuses)))
              (test-assert (= attempts expected-attempts)
                           "only transient SSE errors retry; generation keeps its finite safety limit")
              (test-assert (and (= (length delays) (1- expected-attempts))
                                (every (lambda (delay) (<= 1 delay 60)) delays))
                           "each retry sleeps once with bounded backoff")
              (test-assert (if (and transient-p recover-p)
                               (and (eq result :success) (null failure))
                               (and (typep failure 'provider-error)
                                    (string= (provider-error-code failure) code)
                                    (search "Temporary provider failure" (princ-to-string failure))))
                           "success ends recovery and terminal failures retain the provider reason")
              (test-assert (and (null exhausted)
                                (or recover-p (not transient-p)
                                    (typep failure 'provider-stream-abandoned)))
                           "streaming safety stops are distinct from finite pre-generation exhaustion")))))
      (dolist (status '(503 429))
        (test-assert
         (handler-case (provider--signal-http-status-failure provider status)
           (provider-retryable-error () t))
         "HTTP service and rate-limit failures already permit retry"))))
  nil)

(-> test-provider-exhaustion-boundaries () null)
(defun test-provider-exhaustion-boundaries ()
  "Distinguish real exhaustion from streaming safety stops and interrupted backoff."
  (with-test-configuration (configuration)
    (let ((provider (test-codex-provider-create configuration nil)))
      (dolist (mode '(:partial :abort :resample :fresh))
        (let* ((attempts                             0)
               (sleeps                               0)
               (notices                              nil)
               (last-condition                       nil)
               (abort-condition                      (make-condition 'provider-retryable-error :message "Interrupted backoff"))
               (*provider-maximum-transient-retries* 2)
               (*provider-maximum-streaming-retries* 0)
               (*bounded-retry-sleep-function*
                 (lambda (delay)
                   (declare (ignore delay))
                   (incf sleeps)
                   (when (eq mode ':abort) (error abort-condition))))
               (observer
                 (make-instance 'callback-agent-observer
                                :status-callback
                                (lambda (kind details)
                                  (when (eq kind ':provider-retries-exhausted)
                                    (push details notices)))))
               (failure
                 (handler-case
                     (provider--call-with-bounded-retries
                      provider
                      (lambda (refresh callback)
                        (declare (ignore refresh))
                        (incf attempts)
                        (when (and (eq mode ':resample) (= attempts 1))
                          (error 'provider-resample-requested :message "Resample"
                                 :attempt 1 :maximum-attempts 2))
                        (unless (eq mode ':fresh)
                          (when (eq mode ':partial)
                            (funcall callback (make-instance 'assistant-delta-event :text "partial")))
                          (setf last-condition
                                (make-condition 'provider-retryable-error
                                                :message "Owned finite transient"
                                                :code    "owned_finite_transient"))
                          (error last-condition)))
                      (agent--provider-event-callback observer))
                   (provider-error (condition) condition))))
          (ecase mode
            (:partial
             (test-assert (and (= attempts 1) (zerop sleeps)
                               (typep failure 'provider-stream-abandoned) (null notices))
                          "partial output obeys the safety cap without a false exhaustion notice"))
            (:abort
             (test-assert (and (= attempts 1) (= sleeps 1)
                               (eq failure abort-condition) (null notices))
                          "interrupted backoff preserves its condition without claiming exhaustion"))
            (:resample
             (test-assert (and (= attempts 4) (= sleeps 2)
                               (eq failure last-condition) (= (length notices) 1)
                               (= (getf (first notices) :attempts) 4))
                          "independent resampling does not corrupt exhausted attempt counts"))
            (:fresh
             (test-assert (and (= attempts 1) (zerop sleeps) (null failure) (null notices))
                          "a later request starts with no retry or notice state")))))
      (let ((anthropic (make-instance 'anthropic-messages-provider)))
        (dolist (code '("overloaded_error" "rate_limit_error"))
          (test-assert
           (handler-case
               (provider-consume-stream
                anthropic
                (make-string-input-stream
                 (test-sse-event-string
                  (json-object "type" "error" "error"
                               (json-object "type" code "message" "Temporary failure"))))
                #'identity)
             (provider-retryable-error (condition)
               (string= (provider-error-code condition) code)))
           "Anthropic error events use the same exact transient classification")))))
  nil)


;;;; -- Product Connection Recovery --

(-> provider-recovery-tests--call-with-provider (integer function) t)
(defun provider-recovery-tests--call-with-provider (port function)
  "Call FUNCTION with a factory-selected local Chat Completions provider."
  (with-test-configuration (configuration)
    (let ((name "owned-connection-recovery-fixture"))
      (unwind-protect
           (progn
             (register-provider
              name :models '("owned-connection-model") :family ':openai
              :protocol ':chat-completions
              :endpoint (format nil "http://127.0.0.1:~D/v1/chat/completions" port)
              :factory  (lambda (configuration &key reasoning-summaries-p)
                         (declare (ignore reasoning-summaries-p))
                         (openai-compatible-provider-create
                          configuration :name name :family ':openai)))
             (setf (configuration-validation-deferred-p configuration) t
                   (config :model configuration) "owned-connection-model"
                   (config :model-provider configuration) name)
             (let ((provider (provider-create configuration)))
               (test-assert (typep provider 'openai-compatible-provider)
                            "the registered factory selects the production HTTP provider")
               (funcall function provider
                        (conversation-create configuration :identifier "owned-recovery"))))
        (unregister-provider name)))))

(-> test-provider-recovery-native-opening () null)
(defun test-provider-recovery-native-opening ()
  "Exercise a real healthy HTTP opening and an isolated non-listening refusal."
  (dolist (healthy-p '(nil t))
    (let ((socket       (make-instance 'sb-bsd-sockets:inet-socket
                                       :type ':stream :protocol ':tcp))
          (server       nil)
          (accepted     nil)
          (stream       nil)
          (headers-read 0))
      (unwind-protect
           (progn
             (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
             (when healthy-p
               (sb-bsd-sockets:socket-listen socket 1)
               (setf server
                     (sb-thread:make-thread
                      (lambda ()
                        (setf accepted (sb-bsd-sockets:socket-accept socket)
                              stream (sb-bsd-sockets:socket-make-stream
                                      accepted :input t :output t :element-type 'character
                                      :external-format :utf-8 :buffering ':none))
                        (let ((body-length 0))
                          (loop for line = (read-line stream nil nil)
                                while (and line (plusp (length (string-trim '(#\Return) line))))
                                do (incf headers-read)
                                when (search "content-length:" (string-downcase line))
                                  do (setf body-length (parse-integer line :start 15 :junk-allowed t)))
                          (loop repeat body-length do (read-char stream)))
                        (format stream "HTTP/1.1 200 OK~C~CContent-Type: text/event-stream~C~CContent-Length: 0~C~CConnection: close~C~C~C~C"
                                #\Return #\Newline #\Return #\Newline #\Return #\Newline
                                #\Return #\Newline #\Return #\Newline)
                        (finish-output stream)
                        (close stream))
                      :name "owned recovery healthy control")))
             (multiple-value-bind (address port) (sb-bsd-sockets:socket-name socket)
               (declare (ignore address))
               (provider-recovery-tests--call-with-provider
                port
                (lambda (provider conversation)
                  (let ((started (get-internal-real-time))
                        (failure nil)
                        (status  nil))
                    (handler-case
                        (multiple-value-bind (response response-status headers)
                            (provider-call-with-response-deadline
                             (if healthy-p 2 0.2)
                             (lambda ()
                               (provider--open-response-stream
                                provider (json-object "model" "owned-connection-model" "messages" #())
                                :credentials  (make-instance 'oauth-credentials :access-token "inert")
                                :conversation conversation)))
                          (declare (ignore headers))
                          (setf status response-status)
                          (when (streamp response) (close response)))
                      (error (condition) (setf failure condition)))
                    (format t "~&NATIVE healthy=~S start=~D end=~D class=~S status=~S accepted=~S headers=~D~%"
                            healthy-p started (get-internal-real-time) (type-of failure)
                            status (not (null accepted)) headers-read)
                    (test-assert (if healthy-p (and (null failure) (eql status 200)) failure)
                                 "the selected production route distinguishes healthy and refusing sockets")
                    (test-assert (< (/ (- (get-internal-real-time) started)
                                      internal-time-units-per-second) 3)
                                 "the supported deadline bounds actual opening detection"))))))
        (when stream (close stream :abort t))
        (when accepted (sb-bsd-sockets:socket-close accepted))
        (sb-bsd-sockets:socket-close socket)
        (when server
          (sb-thread:join-thread server :timeout 3 :default nil)
          (when (sb-thread:thread-alive-p server)
            (sb-thread:terminate-thread server)
            (sb-thread:join-thread server :default nil))))))
  nil)

(-> test-provider-recovery-long-ladder () null)
(defun test-provider-recovery-long-ladder ()
  "Exercise extended retry counts with modeled waits inside a live monotonic window."
  (provider-recovery-tests--call-with-provider
   1
   (lambda (provider conversation)
     (declare (ignore conversation))
     (let ((attempts 0)
           (waits    nil)
           (failure  nil)
           (answer   nil))
       (let ((*bounded-retry-sleep-function* (lambda (seconds) (push seconds waits))))
         (handler-case
             (setf answer
                   (multiple-value-list
                    (provider--call-with-bounded-retries
                     provider
                     (lambda (refresh callback)
                       (declare (ignore refresh callback))
                       (incf attempts)
                       (if (< attempts 10)
                           (provider--call-with-transport-normalization
                            (lambda ()
                              (error 'usocket:connection-refused-error))
                            :terminal-errors-p t)
                           (values :healthy :second-value)))
                     #'identity)))
           (provider-error (condition) (setf failure condition))))
       (test-assert (null failure) "eligible instability does not exhaust a finite retry ladder")
       (test-assert (and (equal answer '(:healthy :second-value)) (= attempts 10))
                    "the tenth actual attempt preserves all returned values")
       (test-assert (and (= (length waits) 9) (> (reduce #'+ waits :initial-value 0) 300))
                    "modeled delays exceed five minutes without advancing monotonic elapsed time")
       (test-assert (every (lambda (seconds) (<= seconds 300)) waits)
                    "every jittered wait respects the per-wait cap"))))
  nil)

(-> test-provider-recovery-native-cancellation () null)
(defun test-provider-recovery-native-cancellation ()
  "Interrupt an accepted silent opening through the existing controller owner."
  (let ((listener  (make-instance 'sb-bsd-sockets:inet-socket
                                  :type ':stream :protocol ':tcp))
        (accepted  nil)
        (peer      nil)
        (worker    nil)
        (outcome   nil)
        (unwound-p nil))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
           (sb-bsd-sockets:socket-listen listener 1)
           (multiple-value-bind (address port) (sb-bsd-sockets:socket-name listener)
             (declare (ignore address))
             (provider-recovery-tests--call-with-provider
              port
              (lambda (provider conversation)
                (setf worker
                      (sb-thread:make-thread
                       (lambda ()
                         (unwind-protect
                              (handler-case
                                  (provider-call-with-response-deadline
                                   5
                                   (lambda ()
                                     (provider--open-response-stream
                                      provider (json-object "model" "owned-connection-model" "messages" #())
                                      :credentials  (make-instance 'oauth-credentials :access-token "inert")
                                      :conversation conversation)))
                                (application-turn-cancelled () (setf outcome :cancelled))
                                (error (condition) (setf outcome (type-of condition))))
                           (setf unwound-p t)))
                       :name "owned cancellable provider opening"))
                (setf accepted
                      (provider-call-with-response-deadline
                       2 (lambda () (sb-bsd-sockets:socket-accept listener)))
                      peer (sb-bsd-sockets:socket-make-stream
                            accepted :input t :element-type '(unsigned-byte 8) :buffering ':none))
                (provider-recovery-tests--read-native-request peer)
                (let ((controller (make-instance 'application-input-controller :main-thread worker))
                      (cancel-at  (get-internal-real-time)))
                  (setf (application-input-controller-turn-cancellation-delivery-pending-p controller) t)
                  (application-input-controller--interrupt-main-for-turn-cancellation controller)
                  (sb-thread:join-thread worker :timeout 2 :default :timed-out)
                  (format t "~&NATIVE cancellation start=~D end=~D outcome=~S unwind=~S~%"
                          cancel-at (get-internal-real-time) outcome unwound-p)
                  (test-assert (and (eq outcome :cancelled) unwound-p
                                    (not (sb-thread:thread-alive-p worker)))
                               "controller cancellation escapes normalization and finishes the owned opening")
                  (test-assert (< (/ (- (get-internal-real-time) cancel-at)
                                    internal-time-units-per-second) 1)
                               "native cancellation does not wait for the five-second opening timeout")
                  (let ((closed-p
                          (handler-case
                              (provider-call-with-response-deadline
                               0.5
                               (lambda ()
                                 (loop while (read-byte peer nil nil))
                                 t))
                            (sb-sys:deadline-timeout () nil))))
                    (test-assert closed-p "the cancelled native client closes its accepted transport")))))))
      (when worker
        (when (sb-thread:thread-alive-p worker)
          (sb-thread:terminate-thread worker)
          (sb-thread:join-thread worker :default nil)))
      (when peer (close peer :abort t))
      (when accepted (sb-bsd-sockets:socket-close accepted))
      (sb-bsd-sockets:socket-close listener)))
  nil)

(-> test-provider-recovery-service-and-semantic-controls () null)
(defun test-provider-recovery-service-and-semantic-controls ()
  "Extend only service instability, leaving ordinary semantic rejection bounded."
  (provider-recovery-tests--call-with-provider
   1
   (lambda (provider conversation)
     (declare (ignore conversation))
     (dolist (status '(500 502 503 504 400 429))
       (let ((attempts 0) (failure nil) (result nil))
         (let ((*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds)))))
           (handler-case
               (setf result
                     (provider--call-with-bounded-retries
                      provider
                      (lambda (refresh callback)
                        (declare (ignore refresh callback))
                        (incf attempts)
                        (if (< attempts 10)
                            (error 'provider-retryable-error :message "owned control" :status status)
                            :healthy))
                      #'identity))
             (provider-error (condition) (setf failure condition))))
         (test-assert (if (or (= status 429) (<= 500 status 599))
                          (and (null failure) (eq result :healthy) (= attempts 10))
                          (and failure (= attempts (1+ *provider-maximum-transient-retries*))))
                      "service and throttling instability receive the caller-owned ladder")))))
  nil)

(-> test-provider-recovery-cumulative-generation () null)
(defun test-provider-recovery-cumulative-generation ()
  "Keep the imported cumulative generated-failure budget across mixed attempts."
  (provider-recovery-tests--call-with-provider
   1
   (lambda (provider conversation)
     (declare (ignore conversation))
     (let ((attempts 0) (failure nil))
       (let ((*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds)))))
         (handler-case
             (provider--call-with-bounded-retries
              provider
              (lambda (refresh callback)
                (declare (ignore refresh))
                (incf attempts)
                (when (oddp attempts)
                  (funcall callback (make-instance 'assistant-delta-event :text "partial")))
                (error 'provider-transport-error :message "owned read reset"))
              #'identity)
           (provider-error (condition) (setf failure condition))))
       (test-assert (and (= attempts 5) (typep failure 'provider-stream-abandoned)
                         (= (provider-stream-abandoned-attempts failure) 3))
                    "three generated failures stop the same wrapper despite intervening openings"))))
  nil)

(-> test-provider-recovery-native-timeout-to-healthy () null)
(defun test-provider-recovery-native-timeout-to-healthy ()
  "Recover native opening timeouts through the finite ladder, then open healthy HTTP."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type ':stream :protocol ':tcp))
        (server nil) (accepted nil) (stream nil) (attempts 0) (waits nil) (failures nil))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
           (multiple-value-bind (address port) (sb-bsd-sockets:socket-name socket)
             (declare (ignore address))
             (provider-recovery-tests--call-with-provider
              port
              (lambda (provider conversation)
                (let ((*provider-maximum-transient-retries* 2)
                      (*bounded-retry-sleep-function*
                        (lambda (seconds)
                          (push seconds waits)
                          (when (= (length waits) 2)
                            (sb-bsd-sockets:socket-listen socket 1)
                            (setf server
                                  (sb-thread:make-thread
                                   (lambda ()
                                     (setf accepted (sb-bsd-sockets:socket-accept socket)
                                           stream (sb-bsd-sockets:socket-make-stream
                                                   accepted :input t :output t :element-type 'character
                                                   :external-format :utf-8 :buffering ':none))
                                     (let ((body-length 0))
                                       (loop for line = (read-line stream nil nil)
                                             while (and line (plusp (length (string-trim '(#\Return) line))))
                                             when (search "content-length:" (string-downcase line))
                                               do (setf body-length (parse-integer line :start 15 :junk-allowed t)))
                                       (loop repeat body-length do (read-char stream)))
                                     (format stream "HTTP/1.1 200 OK~C~CContent-Type: text/event-stream~C~CContent-Length: 0~C~CConnection: close~C~C~C~C"
                                             #\Return #\Newline #\Return #\Newline #\Return #\Newline
                                             #\Return #\Newline #\Return #\Newline)
                                     (finish-output stream)
                                     (close stream))
                                   :name "owned delayed healthy provider")))))
                      (failure nil) (status nil))
                  (handler-case
                      (setf status
                            (provider--call-with-bounded-retries
                             provider
                             (lambda (refresh callback)
                               (declare (ignore refresh callback))
                               (incf attempts)
                               (multiple-value-bind (response status headers)
                                   (provider-call-with-response-deadline
                                    (if (< attempts 3) 0.1 2)
                                    (lambda ()
                                      (provider--open-response-stream
                                       provider (json-object "model" "owned-connection-model" "messages" #())
                                       :credentials  (make-instance 'oauth-credentials :access-token "inert")
                                       :conversation conversation)))
                                 (declare (ignore headers))
                                 (when (streamp response) (close response))
                                 status))
                             (lambda (event)
                               (when (typep event 'provider-attempt-failed-event)
                                 (push (provider-attempt-failed-event-condition event) failures)))))
                    (provider-error (condition) (setf failure condition)))
                  (format t "~&NATIVE recovery attempts=~D waits=~D accepted=~S status=~S failure=~S~%"
                          attempts (length waits) (not (null accepted)) status (type-of failure))
                  (test-assert (and (null failure) (= attempts 3) (= (length waits) 2)
                                    accepted (eql status 200))
                               "the third native opening succeeds within the finite transient count")
                  (test-assert (and (= (length failures) 2)
                                    (every (lambda (condition)
                                             (and (typep condition 'provider-opening-instability)
                                                  (eq (provider-opening-instability-category condition) ':opening-timeout)))
                                           failures))
                               "the native non-listening fixture reports local opening timeouts"))))))
      (when stream (close stream :abort t))
      (when accepted (sb-bsd-sockets:socket-close accepted))
      (sb-bsd-sockets:socket-close socket)
      (when server
        (sb-thread:join-thread server :timeout 3 :default nil)
        (when (sb-thread:thread-alive-p server)
          (sb-thread:terminate-thread server)
          (sb-thread:join-thread server :default nil)))))
  nil)

(-> test-provider-recovery-owner-deadline () null)
(defun test-provider-recovery-owner-deadline ()
  "An earlier enclosing deadline terminates ownership instead of reopening."
  (provider-recovery-tests--call-with-provider
   1
   (lambda (provider conversation)
     (declare (ignore conversation))
     (provider-recovery-tests--deadline-quantum-controls)
     (let ((attempts 0) (expired-p nil)
           (*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds)))))
       (handler-case
           (sb-sys:with-deadline (:seconds 0.03)
             (provider--call-with-bounded-retries
              provider
              (lambda (refresh callback)
                (declare (ignore refresh callback))
                (incf attempts)
                (when (> attempts 2)
                  (error 'provider-error :message "An expired owner dispatched again."))
                (provider--call-with-transport-normalization
                 (lambda () (provider--call-opening (lambda () (sleep 0.1))))
                 :terminal-errors-p t))
              #'identity))
         (sb-sys:deadline-timeout () (setf expired-p t))
         (provider-error () (setf expired-p (= attempts 1))))
       (test-assert expired-p "owner expiry escapes normalization without instability retry")
       (test-assert (= attempts 1) "no dispatch follows expiry of the earlier owner"))))
  (let ((state (make-provider-connection-recovery)))
    (provider--connection-wait state 0.01)
    (test-assert (not (provider-connection-recovery-ended-p state))
                 "natural native wait expiry does not end a live owner"))
  (let ((expired-p nil) (state nil))
    (handler-case
        (sb-sys:with-deadline (:seconds 0.02)
          (setf state (make-provider-connection-recovery))
          (provider--call-with-connection-episode
           state (lambda () (provider--connection-wait state 1))))
      (sb-sys:deadline-timeout () (setf expired-p t))
      (provider-owner-ended () (setf expired-p t)))
    (test-assert expired-p "caller expiry terminates a native wait")
    (test-assert (provider-connection-recovery-ended-p state)
                 "deadline unwind removes ownership")
    (test-assert (not (member state *provider-connection-episodes*))
                 "deadline unwind unregisters the exact waiter"))
  nil)

(-> test-provider-recovery-dns-codes () null)
(defun test-provider-recovery-dns-codes ()
  "Classify concrete SBCL DNS semantics without printed error matching."
  (dolist (entry (list (cons sb-bsd-sockets-internal::eai-again t)
                      (cons sb-bsd-sockets-internal::eai-noname nil)
                      (cons sb-bsd-sockets-internal::eai-badflags nil)))
    (let ((retried-p nil))
      (handler-case
          (provider--call-opening
           (lambda ()
             (error 'sb-bsd-sockets:name-service-error
                    :errno (first entry) :syscall "getaddrinfo")))
        (provider-retryable-error () (setf retried-p t))
        (provider-error () nil))
      (test-assert (eq retried-p (rest entry)) "only temporary resolver codes enter recovery")))
  (test-assert
   (eq :dns-temporary
       (provider--raw-opening-category (make-condition 'sb-bsd-sockets:try-again-error)))
   "a typed temporary resolver failure needs no synthetic errno slot")
  nil)

(-> test-provider-recovery-owned-waiter () null)
(defun test-provider-recovery-owned-waiter ()
  "Wake the exact owned waiter through cancellation and actual public goal pause."
  (provider-recovery-tests--registration-races)
  (provider-recovery-tests--ordinary-goal-ownership)
  (provider-recovery-tests--concurrent-waiters)
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (application  (make-instance
                          'application
                          :configuration configuration
                          :conversation  conversation
                          :ui            (terminal-ui-create
                                          :terminal (make-instance 'recording-terminal :columns 60)))))
      (dolist (mode '(:before-registration :at-dispatch :wait :controller :pause))
        (let* ((goal    (list :objective "Owned fixture" :status ':active))
               (ready   (sb-thread:make-semaphore :count 0))
               (release (sb-thread:make-semaphore :count 0))
               (worker nil) (state nil) (attempts 0) (cleanup 0) (outcome nil))
          (setf (application-goal application) goal)
          (let ((original-wait (symbol-function 'provider--wait-on-connection-condition)))
            (test-call-with-function-replacements
             (list (list 'provider--wait-on-connection-condition
                         (lambda (episode timeout)
                           (when (and (eq state episode)
                                      (member mode '(:wait :controller :pause)))
                             ;; Cancellation must acquire this mutex after the native
                             ;; wait atomically registers and releases it.
                             (sb-thread:signal-semaphore ready))
                           (funcall original-wait episode timeout))))
             (lambda ()
              (unwind-protect
                   (progn
                     (setf worker
                           (sb-thread:make-thread
                            (lambda ()
                              (let* ((*provider-connection-owner* goal)
                                     (*provider-connection-owner-check*
                                       (lambda ()
                                         (unless (eq (getf goal :status) ':active)
                                           (error 'provider-owner-ended :message "Fixture goal stopped."))
                                         nil))
                                     (episode                     (make-provider-connection-recovery)))
                                (setf state episode)
                                (unwind-protect
                                     (handler-case
                                         (progn
                                           (when (eq mode ':before-registration)
                                             (sb-thread:signal-semaphore ready)
                                             (sb-thread:wait-on-semaphore release))
                                           (provider--call-with-connection-episode
                                            episode
                                            (lambda ()
                                              (provider--call-with-connection-recovery
                                               episode
                                               (lambda ()
                                                 (incf attempts)
                                                 (when (eq mode ':at-dispatch)
                                                   (sb-thread:signal-semaphore ready)
                                                   (sb-thread:wait-on-semaphore release))
                                                 (provider--call-opening
                                                  (lambda ()
                                                    (when (eq mode ':at-dispatch)
                                                      (error "Dispatch after owner stop."))
                                                    (error 'usocket:connection-refused-error))))))))
                                       (provider-owner-ended () (setf outcome ':ended))
                                       (application-turn-cancelled () (setf outcome ':controller))
                                       (error (condition) (setf outcome (type-of condition))))
                                  (incf cleanup))))
                            :name "owned connection waiter"))
                     (test-assert (sb-thread:wait-on-semaphore ready :timeout 2)
                                  "the owned request reaches its synchronized boundary")
                     (let ((unrelated (make-provider-connection-recovery))
                           (cancel-at (get-internal-real-time)))
                       (case mode
                         ((:before-registration :pause)
                          (application-goal-request application ':pause nil))
                         (:at-dispatch
                          (setf (getf goal :status) ':paused)
                          (provider-cancel-connection-owner goal))
                         (:wait
                          (provider-cancel-connection-owner goal))
                         (:controller
                          (let ((controller (make-instance 'application-input-controller :main-thread worker)))
                            (setf (application-input-controller-turn-cancellation-delivery-pending-p controller) t)
                            (application-input-controller--interrupt-main-for-turn-cancellation controller))))
                       (sb-thread:signal-semaphore release)
                       (sb-thread:join-thread worker :timeout 2 :default ':timeout)
                       (test-assert (not (sb-thread:thread-alive-p worker))
                                    "cancellation wakes a long owned wait promptly")
                       (test-assert (< (/ (- (get-internal-real-time) cancel-at)
                                         internal-time-units-per-second) 1)
                                    "no backoff duration delays the owner stop")
                       (test-assert (and (= cleanup 1)
                                         (eq outcome (if (eq mode ':controller) ':controller ':ended))
                                         (= attempts (if (eq mode ':before-registration) 0 1)))
                                    "one cleanup and no later attempt after stop")
                       (test-assert (not (provider-connection-recovery-ended-p unrelated))
                                    "unrelated request ownership is unaffected")
                       (test-assert (not (member state *provider-connection-episodes*))
                                    "every terminal path removes its registration")))
                (when (and worker (sb-thread:thread-alive-p worker))
                  (sb-thread:terminate-thread worker)
                  (sb-thread:join-thread worker :default nil))))))))))
  nil)

(-> test-provider-recovery-repeated-opening-timeout () null)
(defun test-provider-recovery-repeated-opening-timeout ()
  "A stalled request exhausts finite opening retries while its same-route peer succeeds."
  (provider-recovery-tests--call-with-provider
   1
   (lambda (provider conversation)
     (let ((attempts 0) (healthy 0) (events nil) (outcome nil)
           (request-a                            (json-object "request" "A"))
           (request-b                            (json-object "request" "B"))
           (*provider-opening-timeout-seconds*   0.005)
           (*provider-maximum-transient-retries* 2))
       (test-call-with-function-replacements
        (list
         (list 'provider-open-response-stream
               (lambda (selected request &key credentials conversation)
                 (declare (ignore credentials conversation))
                 (test-assert (eq selected provider) "both requests use the identical route")
                 (if (eq request request-a)
                     (progn
                       (incf attempts)
                       (when (> attempts 5)
                         (error 'provider-owner-ended :message "Local regression guard."))
                       (sleep 0.02))
                     (progn
                       (incf healthy)
                       (make-string-input-stream ""))))))
        (lambda ()
          (let ((*bounded-retry-sleep-function*
                  (lambda (seconds)
                    (declare (ignore seconds))
                    (provider--call-with-bounded-retries
                     provider
                     (lambda (refresh callback)
                       (declare (ignore refresh callback))
                       (with-open-stream (stream (provider--open-response-stream
                                                  provider request-b :conversation conversation))
                         (test-assert (null (read-char stream nil nil))
                                      "the independent peer opens successfully")))
                     #'identity))))
            (setf outcome
                  (handler-case
                      (provider--call-with-bounded-retries
                       provider
                       (lambda (refresh callback)
                         (declare (ignore refresh callback))
                         (provider--open-response-stream provider request-a
                                                         :conversation conversation))
                       (lambda (event) (push event events)))
                    (provider-error (condition) condition))))))
       (test-assert (and (typep outcome 'provider-opening-instability)
                         (eq (provider-opening-instability-category outcome) ':opening-timeout))
                    "the real opening deadline survives both normalization boundaries")
       (test-assert (= attempts 3) "opening failures consume the finite retry count")
       (test-assert (notany (lambda (event)
                              (and (typep event 'provider-attempt-failed-event)
                                   (provider-attempt-failed-event-output-received-p event)))
                            events)
                    "all stalled opening attempts finish without generation output")
       (test-assert (= healthy 2) "a healthy same-route request succeeds between each retry")
       (test-assert (= 1 (count-if (lambda (event)
                                    (typep event 'provider-retries-exhausted-event)) events))
                    "finite exhaustion is reported rather than the test guard"))))
  nil)

(-> provider-recovery-tests--deadline-quantum-controls () null)
(defun provider-recovery-tests--deadline-quantum-controls ()
  "Control ownership rounding only after SBCL actually signals its deadline."
  (dolist (units '(0 1 2))
    (let* ((state                         (make-provider-connection-recovery))
           (observed                      nil)
           (outcome                       nil)
           (*provider-connection-episode* state))
      (setf (provider-connection-recovery-deadline state) nil
            (provider-connection-recovery-owner-check state)
            (lambda () (when observed (/ units internal-time-units-per-second))))
      (handler-case
          (provider--call-opening
           (lambda ()
             (handler-bind ((sb-sys:deadline-timeout
                              (lambda (condition)
                                (declare (ignore condition))
                                (setf observed t))))
               (sb-sys:with-deadline (:seconds 0 :override t)
                 (sleep 0.001)))))
        (provider-owner-ended () (setf outcome ':owner))
        (provider-opening-instability () (setf outcome ':local)))
      (test-assert observed "the runtime signals an actual deadline at the controlled ownership boundary")
      (test-assert (eq outcome (if (<= units 1) ':owner ':local))
                   "zero and one quantum end ownership, outside tolerance remains a local timeout")))
  (dolist (units '(0 1 2))
    (let ((state (make-provider-connection-recovery)) (dispatched nil) (ended nil))
      (setf (provider-connection-recovery-deadline state) nil
            (provider-connection-recovery-owner-check state)
            (lambda () (/ units internal-time-units-per-second)))
      (let ((*provider-connection-episode* state))
        (handler-case (provider--call-opening (lambda () (setf dispatched t)))
          (provider-owner-ended () (setf ended t))))
      (test-assert (and (eq ended (zerop units)) (eq dispatched (plusp units)))
                   "ordinary dispatch does not borrow the post-condition quantum tolerance")))
  nil)

(-> provider-recovery-tests--ordinary-goal-ownership () null)
(defun provider-recovery-tests--ordinary-goal-ownership ()
  "Exercise ordinary application dispatch without a mission, deadline or opt-in."
  (dolist (mode '(:before-registration :at-dispatch :wait :resume :clear))
    (with-test-configuration (configuration)
      (let* ((application (mission-test--application configuration))
             (goal        (list :objective "Ordinary fixture" :status ':active))
             (attempts 0) (ended nil) (captured nil))
        (setf (application-goal application) goal)
        (test-call-with-function-replacements
         (list
          (list 'agent-run-user-turn
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (setf captured *provider-connection-owner*)
                  (when (member mode '(:before-registration :resume))
                    (application-goal-request application ':pause nil)
                    (when (eq mode ':resume)
                      ;; Restore the same goal identity to expose revival of a
                      ;; cancelled turn, without starting new fixture work.
                      (setf (getf goal :status) ':active)))
                  (let ((state (make-provider-connection-recovery)))
                    (provider--call-with-connection-episode
                     state
                     (lambda ()
                       (when (eq mode ':at-dispatch)
                         (application-goal-request application ':pause nil))
                       (provider--call-with-connection-recovery
                        state
                        (lambda ()
                          (incf attempts)
                          (when (> attempts 2)
                            (error 'provider-error :message "Fixture observed post-pause replay."))
                          (if (eq mode ':wait)
                              (error 'provider-opening-instability :category ':connection
                                     :message                                "Fixture refusal.")
                              (progn
                                (when (eq mode ':clear)
                                  (application-goal-request application ':clear nil))
                                (provider--call-opening (lambda () (values 17 23))))))))))))
          (list 'application-render-records (lambda (&rest arguments) (declare (ignore arguments))))
          (list 'application--note-goal-turn (lambda (&rest arguments) (declare (ignore arguments))))
          (list 'application--maybe-refresh-conversation-title
                (lambda (&rest arguments) (declare (ignore arguments)))))
         (lambda ()
           (let ((*bounded-retry-sleep-function*
                   (lambda (seconds)
                     (declare (ignore seconds))
                     (application-goal-request application ':pause nil))))
             (handler-case (application--run-turn application "Fixture dispatch" :tools-p nil)
               (provider-owner-ended () (setf ended t))
               (error () nil)))))
        (test-assert (eq captured goal) "ordinary application dispatch attaches the exact caller goal")
        (test-assert ended "pause, clear and pause-resume terminate the existing ordinary turn")
        (test-assert (= attempts (if (member mode '(:before-registration :at-dispatch :resume)) 0 1))
                     "a terminal ordinary turn never dispatches another provider attempt")
        (test-assert (null (mission-context-find (application-conversation application)))
                     "ordinary goal ownership does not create a mission"))))
  nil)

(-> provider-recovery-tests--concurrent-waiters () null)
(defun provider-recovery-tests--concurrent-waiters ()
  "Pause an ordinary application waiter while an unrelated native waiter stays live."
  (with-test-configuration (configuration)
    (let* ((application (mission-test--application configuration))
           (goal        (list :objective "Native ordinary owner" :status ':active))
           (owners      (list goal (list :second)))
           (states      (mapcar (lambda (owner)
                                  (let ((*provider-connection-owner* owner))
                                    (make-provider-connection-recovery))) owners))
           (ready       (sb-thread:make-semaphore :count 0))
           (threads     nil)
           (outcomes    (make-array 2 :initial-element nil))
           (original    (symbol-function 'provider--wait-on-connection-condition)))
      (setf (application-goal application) goal)
      (test-call-with-function-replacements
       (list
        (list 'provider--wait-on-connection-condition
              (lambda (state seconds)
                (sb-thread:signal-semaphore ready)
                (funcall original state seconds)))
        (list 'agent-run-user-turn
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (let ((state (make-provider-connection-recovery)))
                  (setf (first states) state)
                  (provider--call-with-connection-episode
                   state (lambda () (provider--connection-wait state 300)))))))
       (lambda ()
         (unwind-protect
              (progn
                (loop for state in states for index from 0
                      do (let ((owned state) (slot index))
                           (push (sb-thread:make-thread
                                  (lambda ()
                                    (handler-case
                                        (if (zerop slot)
                                            (application--run-turn application "Native fixture" :tools-p nil)
                                            (provider--call-with-connection-episode
                                             owned (lambda () (provider--connection-wait owned 300))))
                                      (provider-owner-ended () (setf (aref outcomes slot) ':ended))
                                      (error (condition) (setf (aref outcomes slot) (type-of condition))))))
                                 threads)))
                (test-assert (sb-thread:wait-on-semaphore ready :timeout 2) "first simultaneous waiter registers")
                (test-assert (sb-thread:wait-on-semaphore ready :timeout 2) "second simultaneous waiter registers")
                (application-goal-request application ':pause nil)
                (sb-thread:join-thread (second threads) :timeout 2 :default ':timeout)
                (test-assert (eq (aref outcomes 0) ':ended) "public ordinary goal pause wakes its native waiter")
                (test-assert (and (sb-thread:thread-alive-p (first threads))
                                  (null (aref outcomes 1)))
                             "unrelated concurrent waiter is still alive after peer pause")
                (provider-cancel-connection-owner (second owners))
                (sb-thread:join-thread (first threads) :timeout 2 :default ':timeout)
                (test-assert (eq (aref outcomes 1) ':ended) "unrelated waiter requires its own cancellation")
                (dolist (state states)
                  (handler-case
                      (provider--call-with-connection-episode state (lambda () (error "Revived a terminal episode.")))
                    (provider-owner-ended () nil))
                  (test-assert (not (member state *provider-connection-episodes*))
                               "terminal waiter cannot register or revive")))
           (dolist (thread threads)
             (when (sb-thread:thread-alive-p thread)
               (sb-thread:terminate-thread thread))
             (sb-thread:join-thread thread :default nil)))))))
  nil)

(-> provider-recovery-tests--read-native-request (stream) null)
(defun provider-recovery-tests--read-native-request (peer)
  "Wait for the native request, not merely TCP accept during client initialization."
  (provider-call-with-response-deadline
   2
   (lambda ()
     (let* ((headers
              (with-output-to-string (output)
                (loop with matched = 0
                      with delimiter = '(13 10 13 10)
                      for byte = (read-byte peer)
                      do (write-char (code-char byte) output)
                         (setf matched (if (= byte (nth matched delimiter)) (1+ matched) 0))
                      until (= matched 4))))
            (marker (search "content-length:" (string-downcase headers))))
       (unless marker (error "The owned native fixture requires an explicit body length."))
       (let ((length (parse-integer headers :start (+ marker 15) :junk-allowed t)))
         (dotimes (index length)
            (declare (ignorable index))
           (read-byte peer))))))
  nil)

(-> provider-recovery-tests--registration-races () null)
(defun provider-recovery-tests--registration-races ()
  "Cancel before registration, between checks, and at dispatch without state revival."
  (dolist (mode '(:before-registration :between-checks :dispatch))
    (let* ((owner                       (list :race mode))
           (*provider-connection-owner* owner)
           (state                       (make-provider-connection-recovery))
           (original                    (symbol-function 'provider--check-connection-owner))
           (checks 0) (attempts 0) (ended nil))
      (when (eq mode ':before-registration) (provider-cancel-connection-owner owner))
      (test-call-with-function-replacements
       (list (list 'provider--check-connection-owner
                   (lambda (episode)
                     (multiple-value-prog1 (funcall original episode)
                       (when (and (eq state episode) (= (incf checks) 1) (eq mode ':between-checks))
                         (provider-cancel-connection-owner owner))))))
       (lambda ()
         (handler-case
             (provider--call-with-connection-episode
              state
              (lambda ()
                (when (eq mode ':dispatch) (provider-cancel-connection-owner owner))
                (provider--call-with-connection-recovery state (lambda () (incf attempts)))))
           (provider-owner-ended () (setf ended t)))))
      (test-assert (and ended (zerop attempts)) "cancellation at each registration boundary prevents dispatch")
      (test-assert (not (member state *provider-connection-episodes*)) "cancelled registration is removed")))
  (let* ((state (make-provider-connection-recovery))
         (lock  (provider-connection-recovery-lock state)))
    (with-lock-held (lock)
      (provider--wait-on-connection-condition state 0)
      (test-assert (eq (sb-thread:mutex-owner lock) sb-thread:*current-thread*)
                   "native condition timeout returns to the caller with the mutex reacquired")))
  nil)


;;;; -- Detached Native Retry Accounting --

(-> test-provider-native-detached-retry-accounting () null)
(defun test-provider-native-detached-retry-accounting ()
  "Retain retry classification only while ordinary mission policy admits further work."
  (dolist (policy '(:block :record))
    (dolist (terminal-p '(nil t))
      (expanded-native-test--fixture
       (lambda (application provider)
         (let* ((source  (application-conversation application))
                (context (mission-request-view-context source))
                (view    (conversation-compaction-view (conversation-compaction-capture source)))
                (goal    (application-goal application))
                (calls   0)
                (waits   0)
                (caught  nil)
                (result  nil))
           (setf (getf goal :unknown-usage-policy) policy)
           (test-call-with-function-replacements
            (list (list 'credential-manager-credentials
                        (lambda (&rest arguments)
                          (declare (ignore arguments))
                          (provider-tests--credentials (application-configuration application))))
                  (list 'dexador:post
                        (lambda (&rest arguments)
                          (declare (ignore arguments))
                          (incf calls)
                          (cond
                            ((= calls 1)
                             (error 'http-request-failed :status 429 :body "Retry later."
                                                        :headers nil :uri "fixture" :method ':post))
                            (terminal-p
                             (error 'http-request-failed :status 400 :body "Invalid request."
                                                        :headers nil :uri "fixture" :method ':post))
                            (t
                             (values (json-encode
                                      (json-object "output" (vector (json-object "type" "compaction"
                                                                                "encrypted_content" "opaque"))
                                                   "usage" (json-object "input_tokens" 1 "output_tokens" 2)))
                                     200 nil)))))
                  (list 'provider--connection-wait
                        (lambda (state seconds)
                          (declare (ignore state seconds))
                          (incf waits))))
            (lambda ()
              (handler-case
                  (setf result
                        (mission-call-with-native-compaction-view
                         context view
                         (lambda () (provider-native-compact-conversation
                                     provider view :tool-namespaces #() :event-callback #'identity))))
                (serious-condition (condition) (setf caught condition)))))
           (test-assert (= calls (if (eq policy ':block) 1 2))
                        "Only a viable ordinary mission retries the transient native attempt.")
           (test-assert (= waits (if (eq policy ':block) 0 1))
                        "Quarantined requests do not enter backoff; record-only uncertainty may recover.")
           (test-assert (eq (getf goal :status) (if (eq policy ':block) ':blocked ':active))
                        "The selected unknown-usage policy retains terminal authority.")
           (test-assert (= (getf goal :turns-used) calls) "Each real native attempt consumes one ordinary turn.")
           (test-assert (= (getf goal :unknown-usage) (if (and terminal-p (eq policy ':record)) 2 1))
                        "Failed dispatched attempts retain unknown usage without a fabricated zero.")
           (test-assert (= (getf goal :tokens-used) (if (and (not terminal-p) (eq policy ':record)) 3 0))
                        "Only the successful response contributes measured tokens.")
           (test-assert (and (zerop (getf goal :requests-outstanding)) (zerop (getf goal :tokens-reserved)))
                        "Every per-attempt reservation settles once.")
           (if (and (eq policy ':record) (not terminal-p))
               (test-assert (and result (null caught)) "A viable detached native request recovers to success.")
               (test-assert (and (typep caught 'mission-error)
                                 (typep (mission-error-cause caught) 'provider-error)
                                 (= (provider-error-status (mission-error-cause caught))
                                    (if (eq policy ':block) 429 400)))
                            "The exact terminal provider cause remains supervised."))))
       :owned-p t :large-p nil)))
  nil)

(-> test-provider-native-detached-mission-lifetime () null)
(defun test-provider-native-detached-mission-lifetime ()
  "End real detached backoff through actual mission expiry or exact cancellation."
  (dolist (mode '(:expiry :caller-expiry :cancel :owner-cancel))
    (expanded-native-test--fixture
     (lambda (application provider)
       (let* ((source   (application-conversation application))
              (context  (mission-request-view-context source))
              (view     (conversation-compaction-view (conversation-compaction-capture source)))
              (goal     (application-goal application))
              (ready    (sb-thread:make-semaphore :count 0))
              (original (symbol-function 'provider--wait-on-connection-condition))
              (posts 0) (waits 0) (episode nil) (failure nil) (worker nil))
         (setf (getf goal :unknown-usage-policy) ':record)
         (when (eq mode ':expiry)
           (setf (getf goal :deadline) (+ (get-universal-time) 2)))
         (test-call-with-function-replacements
          (list (list 'credential-manager-credentials
                      (lambda (&rest arguments)
                        (declare (ignore arguments))
                        (provider-tests--credentials (application-configuration application))))
                (list 'provider--connection-retry-delay
                      (lambda (index &optional random-state)
                        (declare (ignore index random-state)) 1))
                (list 'provider--wait-on-connection-condition
                      (lambda (state seconds)
                        (setf episode state)
                        (incf waits)
                        (when (= posts 3) (sb-thread:signal-semaphore ready))
                        (funcall original state seconds)))
                (list 'dexador:post
                      (lambda (&rest arguments)
                        (declare (ignore arguments))
                        (incf posts)
                        (values "Transient pressure." 429 nil))))
          (lambda ()
            (unwind-protect
                 (progn
                   (setf worker
                         (sb-thread:make-thread
                          (lambda ()
                            (let* ((expires                              (+ (/ (get-internal-real-time) internal-time-units-per-second) 1/4))
                                   (*provider-connection-owner-check*
                                     (when (eq mode ':caller-expiry)
                                       (lambda () (- expires (/ (get-internal-real-time) internal-time-units-per-second)))))
                                   (*provider-maximum-transient-retries* 0)
                                   (*bounded-retry-sleep-function*       #'sleep))
                              (handler-case
                                  (mission-call-with-native-compaction-view
                                   context view
                                   (lambda () (provider-native-compact-conversation
                                               provider view :tool-namespaces #() :event-callback #'identity)))
                                (serious-condition (condition) (setf failure condition)))))
                          :name "detached native mission lifetime"))
                   (unless (member mode '(:expiry :caller-expiry))
                     (test-assert (sb-thread:wait-on-semaphore ready :timeout 5)
                                  "A viable detached mission outlives the bounded retry maximum.")
                     (if (eq mode ':cancel)
                         (application-mission-stop application ':cancelled "Fixture cancellation.")
                         (provider-cancel-connection-owner goal)))
                   (sb-thread:join-thread worker :timeout 4 :default ':timeout)
                   (test-assert (not (sb-thread:thread-alive-p worker)) "Actual mission lifetime promptly ends backoff.")
                   (test-assert (typep failure 'provider-owner-ended) "Exact caller ownership terminates between attempts.")
                   (test-assert (and episode (eq goal (provider-connection-recovery-owner episode)))
                                "The entire detached episode retains its actual mission owner.")
                   (test-assert (if (member mode '(:expiry :caller-expiry)) (plusp posts) (= posts 3))
                                "No HTTP request follows the observed caller lifetime end.")
                   (test-assert (>= waits posts) "Transient attempts reach real condition waits.")
                   (test-assert (and (zerop (getf goal :requests-outstanding))
                                     (zerop (getf goal :tokens-reserved))
                                     (= posts (getf goal :turns-used))
                                     (= posts (getf goal :unknown-usage)))
                                "Each dispatched attempt settles its ordinary unknown usage exactly once.")
                   (test-assert (null *provider-connection-episodes*) "The terminal episode is removed.")
                   (test-assert (null (expanded-native-input-snapshots provider)) "Cancellation cannot select portable fallback.")
                   (format t "~&DETACHED-LIFETIME mode=~S posts=~D waits=~D cause=~S joined=~S~%"
                           mode posts waits (type-of failure) (not (sb-thread:thread-alive-p worker))))
              (when (and worker (sb-thread:thread-alive-p worker))
                (application-mission-stop application ':cancelled "Fixture safety cleanup.")
                (when episode (provider--end-connection-episode episode))
                (sb-thread:join-thread worker :timeout 3 :default ':timeout))
              (test-assert (not (sb-thread:thread-alive-p worker)) "The fixture joins its sole worker."))))))
     :owned-p t :large-p nil))
  nil)

(-> test-provider-native-detached-budget-and-zero-usage () null)
(defun test-provider-native-detached-budget-and-zero-usage ()
  "Preserve ordinary exhausted-turn authority and definitive unavailable zero usage."
  (dolist (status '(429 404))
    (expanded-native-test--fixture
     (lambda (application provider)
       (let* ((source  (application-conversation application))
              (context (mission-request-view-context source))
              (view    (conversation-compaction-view (conversation-compaction-capture source)))
              (goal    (application-goal application))
              (waits   0)
              (failure nil))
         (setf (getf goal :unknown-usage-policy) ':record
               (getf goal :turn-limit) 1)
         (test-call-with-function-replacements
          (list (list 'provider--connection-wait
                      (lambda (state seconds)
                        (declare (ignore state seconds)) (incf waits))))
          (lambda ()
            (expanded-native-test--transport
             application :status status :signaled-p t :function
             (lambda ()
               (handler-case
                   (mission-call-with-native-compaction-view
                    context view
                    (lambda () (provider-native-compact-conversation
                                provider view :tool-namespaces #() :event-callback #'identity)))
                 (serious-condition (condition) (setf failure condition)))))))
         (test-assert (zerop waits) "An exhausted turn budget cannot authorize another recovery wait.")
         (test-assert (= 1 (getf goal :turns-used)) "The sole real attempt consumes the sole normal turn.")
         (test-assert (= (getf goal :unknown-usage) (if (= status 429) 1 0))
                      "Only unavailable native capability has definitive zero usage.")
         (test-assert (and (zerop (getf goal :tokens-used))
                           (zerop (getf goal :tokens-reserved))
                           (zerop (getf goal :requests-outstanding)))
                      "Ordinary settlement releases all reservations without fabricated measured usage.")
         (if (= status 429)
             (test-assert (and (typep failure 'mission-error) (eq (mission-error-reason failure) ':exhausted)
                               (eq (getf goal :status) ':exhausted))
                          "Ordinary exhaustion, not the retryable condition, owns the terminal decision.")
             (test-assert (null failure) "Unavailable capability returns normally with known zero usage."))))
     :owned-p t :large-p nil))
  nil)


;;;; -- Inherited Caller Lifetime --

(-> provider-tests--caller-entry (keyword application model-provider &key (:function function)) *)
(defun provider-tests--caller-entry (entry application provider &key function)
  "Enter the ordinary owning boundaries, including the agent's explicit operation."
  (let ((conversation (application-conversation application)))
    (flet ((stream () (provider-stream-turn provider conversation :tool-namespaces #() :event-callback #'identity)))
      (ecase entry
        (:goal
         (application--call-with-goal-connection-owner application #'stream))
        (:stream
         (stream))
        (:legacy
         (mission-call-with-provider-operation conversation #'stream))
        (:explicit
         (let ((owner (make-provider-operation-owner)))
           (provider-call-with-operation-owner
            owner (lambda () (mission-call-with-provider-operation
                              conversation #'stream :owner owner :operation (make-provider-operation))))))
        (:agent
         (funcall function))))))

(-> test-provider-ordinary-caller-composition () null)
(defun test-provider-ordinary-caller-composition ()
  "Check inherited numeric and signaling authority through actual ordinary requests."
  (dolist (entry '(:goal :stream :legacy :explicit :agent))
    (dolist (remaining '(1/100 0 -1 120 nil :signal))
      (route-tests--call-with-runtime
       (lambda (provider unused configuration)
         (declare (ignore unused))
         (let* ((application (mission-inference-test--application configuration provider))
                (agent       (application-agent application))
                (goal nil) (calls 0) (observed nil) (failure nil) (signal-seen nil)
                (original    (make-condition 'simple-error :format-control "Inherited caller cancellation."))
                (*provider-connection-owner-check*
                  (lambda () (if (eq remaining :signal) (error original) remaining))))
           (unwind-protect
                (progn
                  (setf (agent-provider agent) provider
                        (config :session-title-generation-p configuration) nil
                        (config :provider-fallback-mappings configuration) "[]")
                  (if (eq entry :goal)
                      (setf (application-goal application) (list :status :active))
                      (application-mission-start application (mission-test--specification :turns 8 :tokens 10000)))
                  (setf goal (application-goal application))
                  (test-call-with-function-replacements
                   (list (list 'redact (lambda (text &key language model-directory runtime-library)
                                        (declare (ignore language model-directory runtime-library)) (values text :ok)))
                         (list 'cl-llm-provider-api::provider-execute-request
                               (lambda (&rest arguments)
                                 (declare (ignore arguments))
                                 (incf calls)
                                 (setf observed (funcall *provider-connection-owner-check*))
                                 (mission-test--result "Caller-owned result."))))
                   (lambda ()
                     (handler-bind ((serious-condition (lambda (condition) (when (eq condition original) (setf signal-seen t)))))
                       (handler-case
                           (provider-tests--caller-entry entry application provider
                                                         :function (lambda () (agent-run-user-turn agent "Caller lifetime." :tools-p nil)))
                         (serious-condition (condition) (setf failure condition))))))
                  (setf signal-seen (eq original (if (typep failure 'mission-error) (mission-error-cause failure) failure)))
                  (cond ((eq remaining :signal)
                         (test-assert (and signal-seen failure (zerop calls)) "Inherited cancellation identity survives every owning entry."))
                        ((and (numberp remaining) (<= remaining 0))
                         (test-assert (and failure (zerop calls)) "Elapsed inherited caller prevents provider dispatch."))
                        (t
                         (test-assert (and (null failure) (= calls 1)) "A viable ordinary caller dispatches once.")
                         (test-assert (if (eq entry :goal)
                                          (eql observed remaining)
                                          (and (numberp observed) (plusp observed)
                                               (or (null remaining) (<= observed remaining))))
                                      "Owning boundaries preserve the caller minimum, including NIL and longer callers.")))
                  (unless (eq entry :goal)
                    (test-assert (and (zerop (getf goal :requests-outstanding)) (zerop (getf goal :tokens-reserved)))
                                 "Ordinary caller unwind settles reservations."))
                  (format t "~&CALLER-ENTRY ~S remaining=~S requests=~D observed=~S cause=~S identity=~S~%"
                          entry remaining calls observed (type-of failure) signal-seen))
             (unless (eq entry :goal) (mission-detach application))
             (remhash application *mission-schedule-services*)
             (terminal-ui-stop (application-ui application))))))))
  nil)

(-> test-provider-ordinary-caller-real-wait () null)
(defun test-provider-ordinary-caller-real-wait ()
  "Expire inherited callers in genuine opening backoff through ordinary entry paths."
  (dolist (entry '(:goal :stream :legacy :explicit :agent))
    (dolist (mode '(:expiry :cancel))
      (route-tests--call-with-runtime
       (lambda (provider unused configuration)
         (declare (ignore unused))
         (let* ((application                          (mission-inference-test--application configuration provider))
                (agent                                (application-agent application))
                (original-wait                        (symbol-function 'provider--wait-on-connection-condition))
                (cancel                               (make-condition 'simple-error :format-control "Caller cancelled in real backoff."))
                (expires                              (+ (/ (get-internal-real-time) internal-time-units-per-second) 1/4))
                (posts 0) (waits 0) (failure nil) (signal-seen nil)
                (*bounded-retry-sleep-function*       #'sleep)
                (*provider-maximum-transient-retries* 0)
                (*provider-connection-owner-check*
                  (lambda ()
                    (when (and (eq mode :cancel) (plusp waits)) (error cancel))
                    (- expires (/ (get-internal-real-time) internal-time-units-per-second)))))
           (unwind-protect
                (progn
                  (setf (agent-provider agent) provider
                        (config :session-title-generation-p configuration) nil
                        (config :provider-fallback-mappings configuration) "[]")
                  (if (eq entry :goal)
                      (setf (application-goal application) (list :status :active))
                      (progn
                        (application-mission-start application (mission-test--specification :turns 16 :tokens 10000))
                        (setf (getf (application-goal application) :unknown-usage-policy) :record)))
                  (test-call-with-function-replacements
                   (list (list 'redact (lambda (text &key language model-directory runtime-library)
                                        (declare (ignore language model-directory runtime-library)) (values text :ok)))
                         (list 'provider--connection-retry-delay
                               (lambda (index &optional random-state) (declare (ignore index random-state)) 1))
                         (list 'provider--wait-on-connection-condition
                               (lambda (state seconds) (incf waits) (funcall original-wait state seconds)))
                         (list 'dexador:post
                               (lambda (&rest arguments)
                                 (declare (ignore arguments))
                                 (incf posts)
                                 (if (> posts 2) (error "Diagnostic safety dispatch limit.")
                                     (values "Transient pressure." 429 nil)))))
                   (lambda ()
                     (handler-bind ((serious-condition (lambda (condition) (when (eq condition cancel) (setf signal-seen t)))))
                       (handler-case
                           (provider-tests--caller-entry entry application provider
                                                         :function (lambda () (agent-run-user-turn agent "Wait for caller." :tools-p nil)))
                         (serious-condition (condition) (setf failure condition))))))
                  (test-assert (and (= posts 1) (plusp waits) failure) "The shorter caller ends genuine wait before a second POST.")
                  (setf signal-seen (eq cancel (if (typep failure 'mission-error) (mission-error-cause failure) failure)))
                  (test-assert (if (eq mode :cancel) signal-seen
                                   (typep (if (typep failure 'mission-error) (mission-error-cause failure) failure) 'provider-owner-ended))
                               "Real backoff retains elapsed caller and exact signaling cancellation authority.")
                  (test-assert (null *provider-connection-episodes*) "Caller termination removes the exact episode.")
                  (unless (eq entry :goal)
                    (let ((goal (application-goal application)))
                      (test-assert (and (= 1 (getf goal :turns-used)) (= 1 (getf goal :unknown-usage))
                                        (zerop (getf goal :requests-outstanding)) (zerop (getf goal :tokens-reserved)))
                                   "Real dispatched failure settles ordinary accounting once.")))
                  (format t "~&CALLER-WAIT ~S ~S POST=~D waits=~D cause=~S identity=~S~%"
                          entry mode posts waits (type-of failure) signal-seen))
             (unless (eq entry :goal) (mission-detach application))
             (remhash application *mission-schedule-services*)
             (terminal-ui-stop (application-ui application))))))))
  nil)


;;;; -- Pre-generation Request Window --

(-> test-provider-recovery-window-waits () null)
(defun test-provider-recovery-window-waits ()
  "Both retry ladders stop within a shared window under a one-hour caller."
  (provider-recovery-tests--call-with-provider
   1
   (lambda (provider conversation)
     (declare (ignore conversation))
     (dolist (mode '(:connection :finite :injected))
       (let* ((state                              (make-provider-connection-recovery))
              (start                              (get-internal-real-time))
              (attempts 0) (events nil)
              (*provider-recovery-window-seconds* 3/100)
              (*bounded-retry-sleep-function*
                (if (eq mode ':injected)
                    (lambda (seconds) (sleep (+ seconds 0.01)))
                    #'sleep)))
         (setf (provider-connection-recovery-deadline state)
               (+ 3600 (/ start internal-time-units-per-second)))
         (let ((failure
                 (handler-case
                     (provider--call-with-bounded-retries
                      provider
                      (lambda (refresh callback)
                        (declare (ignore refresh callback))
                        (incf attempts)
                        (if (eq mode ':finite)
                            (error 'provider-retryable-error :message "Semantic transient.")
                            (error 'provider-opening-instability :category ':connection
                                   :message                                "Transport failure.")))
                      (lambda (event) (push event events)) :recovery state)
                   (provider-error (condition) condition))))
           (test-assert (typep failure 'provider-recovery-expired)
                        "a long caller does not authorize unbounded recovery")
           (test-assert (not (typep failure 'provider-retryable-error))
                        "window exhaustion cannot reenter an automatic ladder")
           (test-assert (= attempts 1) "the clipped wait expires before any second dispatch")
           (test-assert (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 1)
                        "real waits and injected sleeps terminate promptly")
           (test-assert (not (member state *provider-connection-episodes*))
                        "expiry releases the exact episode")
           (test-assert (notany (lambda (event) (typep event 'provider-retries-exhausted-event)) events)
                        "time exhaustion is distinct from count exhaustion")
           (let* ((wait  (find-if (lambda (event)
                                   (if (eq mode ':finite)
                                       (typep event 'provider-retry-event)
                                       (and (typep event 'provider-connection-event)
                                            (eq (getf (provider-connection-event-details event) :phase) ':waiting))))
                                events))
                  (delay (typecase wait
                           (provider-retry-event
                            (provider-retry-event-delay wait))
                           (provider-connection-event
                            (getf (provider-connection-event-details wait) :delay)))))
             (test-assert (and (realp delay) (< 0 delay) (<= delay 3/100))
                          "both retry observers report the clipped actual wait")))))
     (test-call-with-function-replacements
      (list (list 'credential-manager-refreshable-p
                  (lambda (manager) (declare (ignore manager)) t)))
      (lambda ()
        (let* ((state                          (make-provider-connection-recovery))
               (alternate                      (openai-compatible-provider-create
                                                (provider-configuration provider)
                                                :name "owned-connection-recovery-fixture" :family ':openai))
               (attempts 0) (deadlines nil) (routes nil) (refreshes nil)
               (*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds)))))
          (setf (provider-connection-recovery-route-count state) 2
                (provider-connection-recovery-route-threshold state) 1)
          (test-assert
           (equal '(recovered values)
                  (multiple-value-list
                   (provider--call-with-bounded-retries
                    provider
                    (lambda (refresh callback)
                      (declare (ignore callback))
                      (push refresh refreshes)
                      (push (provider-connection-recovery-route-index state) routes)
                      (push (provider-connection-recovery-recovery-deadline state) deadlines)
                      (sleep 1/100)
                      (ecase (incf attempts)
                        (1
                         (error 'provider-opening-instability :category ':connection :message "Connect."))
                        (2
                         (error 'provider-unauthorized :status 401 :message "Refresh."))
                        (3
                         (error 'provider-retryable-error :message "Semantic transient."))
                        (4
                         (values 'recovered 'values))))
                    #'identity :recovery state
                    :select-provider     (lambda ()
                                          (if (zerop (provider-connection-recovery-route-index state))
                                              provider alternate)))))
           "route change, authentication refresh and finite reentry preserve return values")
          (test-assert (and (= attempts 4) (equal (reverse routes) '(0 1 1 1))
                            (equal (reverse refreshes) '(nil nil t nil))
                            (numberp (first deadlines)) (apply #'= deadlines))
                       "every retry scope retains the exact original monotonic window"))
        (let ((state (make-provider-connection-recovery)) (attempts 0))
          (test-assert
           (handler-case
               (provider--call-with-bounded-retries
                provider
                (lambda (refresh callback)
                  (declare (ignore refresh callback))
                  (incf attempts)
                  (setf (provider-connection-recovery-recovery-deadline state) 0)
                  (error 'provider-unauthorized :status 401 :message "Refresh after expiry."))
                #'identity :recovery state)
             (provider-recovery-expired () (= attempts 1)))
           "authentication refresh cannot dispatch after recovery expiry"))))
     (test-assert
      (equal '(fresh values)
             (multiple-value-list
              (provider--call-with-bounded-retries
               provider (lambda (refresh callback) (declare (ignore refresh callback))
                          (values 'fresh 'values)) #'identity)))
      "a genuinely new logical request receives its own window")))
  nil)

(-> test-provider-recovery-window-opening () null)
(defun test-provider-recovery-window-opening ()
  "An opening consumes remaining recovery time, with caller cancellation taking precedence."
  (provider-recovery-tests--call-with-provider
   1
   (lambda (provider conversation)
     (dolist (mode '(:window :owner :cancel))
       (let* ((state                              (make-provider-connection-recovery))
              (now                                (/ (get-internal-real-time) internal-time-units-per-second))
              (attempts                           0)
              (*provider-opening-timeout-seconds* 1)
              (*provider-recovery-window-seconds* 1/25))
         (setf (provider-connection-recovery-deadline state) (+ now (if (eq mode ':owner) 1/100 3600)))
         (test-call-with-function-replacements
          (list (list 'provider-open-response-stream
                      (lambda (selected request &key credentials conversation)
                        (declare (ignore selected request credentials conversation))
                        (incf attempts)
                        (when (eq mode ':cancel) (provider--end-connection-episode state))
                        (sleep 0.1))))
          (lambda ()
            (let ((failure
                    (handler-case
                        (provider--call-with-bounded-retries
                         provider
                         (lambda (refresh callback)
                           (declare (ignore refresh callback))
                           (provider--open-response-stream provider (json-object) :conversation conversation))
                         #'identity :recovery state)
                      (provider-error (condition) condition))))
              (test-assert (typep failure (if (eq mode ':window) 'provider-recovery-expired 'provider-owner-ended))
                           "opening expiry preserves window, shorter caller and cancellation distinctions")
              (test-assert (= attempts 1) "no opening follows terminal expiry"))))))
     (let ((state (make-provider-connection-recovery)) (attempts 0))
       (setf (provider-connection-recovery-recovery-deadline state) -1)
       (test-assert
        (handler-case
            (provider--call-with-bounded-retries
             provider (lambda (refresh callback) (declare (ignore refresh callback)) (incf attempts))
             #'identity :recovery state)
          (provider-recovery-expired () t))
        "an already elapsed window is checked before dispatch")
       (test-assert (zerop attempts) "expiry never grants one last attempt"))))
  nil)

(-> test-provider-recovery-window-resume () null)
(defun test-provider-recovery-window-resume ()
  "Suspension preserves the window and terminal exhaustion cannot create resumable work."
  (route-tests--call-with-runtime
   (lambda (provider conversation configuration)
     (declare (ignore provider))
     (let* ((operation                        (make-provider-operation :conversation conversation :goal conversation :epoch 0))
            (owner                            (make-provider-operation-owner :operation operation :active-p t))
            (state                            (make-provider-connection-recovery))
            (restored                         (make-provider-connection-recovery))
            (deadline                         (+ 60 (/ (get-internal-real-time) internal-time-units-per-second)))
            (*provider-suspended-connections* (make-hash-table :test 'eq))
            (*provider-connection-operation*  operation))
       (setf (provider-operation-owner-scope operation) owner
             (provider-connection-recovery-owner state) conversation
             (provider-connection-recovery-owner restored) conversation
             (provider-connection-recovery-attempt state) 3
             (provider-connection-recovery-route-index state) 1
             (provider-connection-recovery-recovery-deadline state) deadline)
       (unwind-protect
            (progn
              (provider--remember-suspension conversation state configuration)
              (test-assert (provider-operation-resumable-p operation conversation configuration)
                           "live original window permits explicit resume")
              (provider--restore-suspension restored conversation configuration)
              (test-assert (= deadline (provider-connection-recovery-recovery-deadline restored))
                           "resume restores rather than renews the original monotonic window")
              (test-assert (and (= 3 (provider-connection-recovery-attempt restored))
                                (= 1 (provider-connection-recovery-route-index restored)))
                           "route and attempt history accompany the same window")
              (setf (provider-connection-recovery-recovery-deadline restored) -1)
              (provider--remember-suspension conversation restored configuration)
              (test-assert (not (provider-operation-resumable-p operation conversation configuration))
                           "an expired window is never eligible for resume")
              (test-assert
               (handler-case (provider--restore-suspension state conversation configuration)
                 (provider-recovery-expired () t))
               "even a direct restoration rejects expiry")
              (remhash operation *provider-suspended-connections*)
              (let ((*provider-connection-capture* nil))
                (test-assert
                 (handler-case
                     (provider-call-with-suspension
                      conversation
                      (lambda ()
                        (setf (second *provider-connection-capture*) state
                              (third *provider-connection-capture*) configuration)
                        (error 'provider-recovery-expired :attempt 3 :message "Window elapsed."))
                      :suspended-p (constantly t))
                   (provider-recovery-expired () t))
                 "a broad slice predicate cannot convert terminal exhaustion to a suspension"))
              (test-assert (zerop (hash-table-count *provider-suspended-connections*))
                           "terminal window exhaustion retains no resumable metadata"))
         (provider-operation-dispose operation)))))
  nil)

(-> test-provider-recovery-condition-generation () null)
(defun test-provider-recovery-condition-generation ()
  "Condition-only generation ends the recovery window, not caller or streaming safety."
  (provider-recovery-tests--call-with-provider
   1
   (lambda (provider conversation)
     (declare (ignore conversation))
     (dolist (mode '(:elapsed :sleep :success :no-generation :owner))
       (let* ((state (make-provider-connection-recovery))
              (attempts 0) (waits 0) (failure nil) (answer nil)
              (*bounded-retry-sleep-function*
                (lambda (seconds)
                  (declare (ignore seconds))
                  (incf waits)
                  (setf (provider-connection-recovery-recovery-deadline state) 0))))
         (handler-case
             (setf answer
                   (multiple-value-list
                    (provider--call-with-bounded-retries
                     provider
                     (lambda (refresh callback)
                       (declare (ignore refresh callback))
                       (incf attempts)
                       (when (member mode '(:elapsed :no-generation :owner))
                         (setf (provider-connection-recovery-recovery-deadline state) 0))
                       (when (eq mode ':owner)
                         (setf (provider-connection-recovery-deadline state) 0))
                       (if (and (eq mode ':success) (= attempts 2))
                           (values :healthy :second-value)
                           (error 'cl-llm-provider-api::provider-truncated-response
                                  :message              "Silent generated failure."
                                  :reason               "max_output_tokens"
                                  :generation-started-p (not (eq mode ':no-generation)))))
                     #'identity :recovery state)))
           (provider-error (condition) (setf failure condition)))
         (case mode
           ((:elapsed :sleep)
            (test-assert (and (typep failure 'provider-stream-abandoned)
                              (= attempts 3)
                              (= (provider-stream-abandoned-attempts failure) 3))
                         "silent generated failures retain the finite streaming budget after window expiry"))
           (:success
            (test-assert (and (null failure) (= attempts 2) (= waits 1)
                              (equal answer '(:healthy :second-value)))
                         "a generated retry can succeed with multiple values after its finite sleep"))
           (:no-generation
            (test-assert (and (typep failure 'provider-recovery-expired) (= attempts 1))
                         "a condition without generation evidence still expires the recovery window"))
           (:owner
            (test-assert (and (typep failure 'provider-owner-ended) (= attempts 1))
                         "condition-only generation never overrides caller expiry")))
         (test-assert (eq (provider-connection-recovery-generation-started-p state)
                          (not (eq mode ':no-generation)))
                      "the failed-attempt observer latches only genuine generation evidence")))))
  nil)

(-> test-provider-recovery-native-compaction-window () null)
(defun test-provider-recovery-native-compaction-window ()
  "Clip the actual native POST operation and settle its supervised attempt."
  (dolist (mode '(:window :owner :cancel :late-window :success :long-success))
    (expanded-native-test--fixture
     (lambda (application provider)
       (let* ((source                             (application-conversation application))
              (context                            (mission-request-view-context source))
              (view                               (conversation-compaction-view (conversation-compaction-capture source)))
              (goal                               (application-goal application))
              (start                              (/ (get-internal-real-time) internal-time-units-per-second))
              (expires                            (+ start (if (eq mode ':owner) 1/100 3600)))
              (original                           (symbol-function 'provider-call-with-response-deadline))
              (posts 0) (limits nil) (episode nil) (failure nil) (answer nil)
              (*provider-opening-timeout-seconds* 1/100)
              (*provider-recovery-window-seconds* (if (eq mode ':long-success) 120 1/25))
              (*provider-connection-owner-check*
                (lambda () (- expires (/ (get-internal-real-time) internal-time-units-per-second)))))
         (setf (getf goal :unknown-usage-policy) ':record)
         (test-call-with-function-replacements
          (list (list 'credential-manager-credentials
                      (lambda (&rest arguments)
                        (declare (ignore arguments))
                        (provider-tests--credentials (application-configuration application))))
                (list 'provider-call-with-response-deadline
                      (lambda (seconds function)
                        (push seconds limits)
                        (if (eq mode ':late-window)
                            (funcall function)
                            (funcall original seconds function))))
                (list 'dexador:post
                      (lambda (&rest arguments)
                        (declare (ignore arguments))
                        (incf posts)
                        (setf episode *provider-connection-episode*)
                        (when (eq mode ':cancel)
                          (provider--end-connection-episode episode))
                        (unless (member mode '(:success :long-success)) (sleep 1/5))
                        (values (json-encode
                                 (json-object "output" (vector (json-object "type" "compaction"
                                                                           "encrypted_content" "opaque"))
                                              "usage" (json-object "input_tokens" 1 "output_tokens" 2)))
                                200 nil))))
          (lambda ()
            (handler-case
                (setf answer
                      (multiple-value-list
                       (mission-call-with-native-compaction-view
                        context view
                        (lambda ()
                          (provider-native-compact-conversation
                           provider view :tool-namespaces #() :event-callback #'identity)))))
              (serious-condition (condition)
                (setf failure (if (typep condition 'mission-error)
                                 (mission-error-cause condition) condition))))))
         (test-assert (= posts 1) "native expiry or completion dispatches exactly once")
         (test-assert (and episode (not (member episode *provider-connection-episodes*)))
                      "native unwind removes its exact recovery episode")
         (test-assert (and (zerop (getf goal :requests-outstanding))
                           (zerop (getf goal :tokens-reserved)))
                      "native timeout and success settle all attempt reservations")
         (if (member mode '(:success :long-success))
             (progn
               (test-assert (and (null failure) (first answer) (second answer))
                            "native completion preserves its output and usage values")
               (test-assert (= (getf goal :tokens-used) 3)
                            "native success contributes measured usage"))
             (progn
               (test-assert (typep failure (if (member mode '(:window :late-window))
                                              'provider-recovery-expired 'provider-owner-ended))
                            "native deadlines classify recovery expiry after caller and cancellation checks")
               (test-assert (not (typep failure 'provider-retryable-error))
                            "native lifetime expiry cannot enter another retry")
               (unless (eq mode ':late-window)
                 (test-assert (< (- (/ (get-internal-real-time) internal-time-units-per-second) start) 3/20)
                              "the actual native POST is interrupted before its fake response"))
               (test-assert (= (getf goal :unknown-usage) 1)
                            "a dispatched native timeout retains unknown usage")))
         (test-assert (some (lambda (limit)
                             (and (realp limit)
                                  (if (eq mode ':long-success)
                                      (< 30 limit 121)
                                      (< 0 limit 1/20))))
                           limits)
                      "native operation uses the request/caller minimum, not a fresh 30-second limit")))
     :owned-p t :large-p nil))
  nil)
