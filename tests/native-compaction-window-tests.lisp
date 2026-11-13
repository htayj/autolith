(in-package #:autolith)

;;;; -- Native Compaction Window Contracts --

(-> native-compaction-tests--opaque (string) json-object)
(defun native-compaction-tests--opaque (content)
  "Return a native item containing CONTENT as opaque provider state."
  (json-object "type" "compaction" "encrypted_content" content))

(-> test-native-compaction-window-validation () null)
(defun test-native-compaction-window-validation ()
  "Require an item-done checkpoint and completed boundary, accounting paid invalid results."
  (with-test-configuration (configuration)
    (let* ((provider (provider-create configuration))
           (opaque   (native-compaction-tests--opaque "opaque"))
           (alias    (json-object "type" "compaction_summary" "encrypted_content" "alias"))
           (refused  (json-object "type" "message" "role" "assistant" "content"
                                  (vector (json-object "type" "refusal" "refusal" "Synthetic refusal."))))
           (ignored  (json-object "type" "function_call_output" "call_id" "returned-tail" "output" "discard")))
      (dolist (case (list (list (list opaque ignored) nil)
                         (list (list alias) nil)
                         (list (list opaque opaque) 'provider-native-unusable)
                         (list nil 'provider-native-unusable)
                         (list (list (native-compaction-tests--opaque "")) 'provider-native-unusable)
                         (list (list (json-object "type" "compaction" "encrypted_content" 17)) 'provider-native-unusable)
                         (list (list 17 opaque) 'provider-native-unusable)
                         (list (list opaque refused) 'provider-native-refusal)))
        (destructuring-bind (items expected) case
          (let ((events nil) (failure nil) (artifact nil) (usage nil))
            (handler-case
                (multiple-value-setq (artifact usage)
                  (provider--decode-native-compaction-response
                   provider (make-instance 'test-character-input-stream :source (native-compaction-tests--source items))
                   :status 200 :headers nil :event-callback (lambda (event) (push event events))))
              (provider-error (condition) (setf failure condition)))
            (test-assert (if expected (typep failure expected) (null failure)) (format nil "completed outcome expected ~S actual ~S (~A)" expected (type-of failure) failure))
            (let ((completed (remove-if-not (lambda (event) (typep event 'native-compaction-completed-event)) events)))
              (test-assert (and (= 1 (length completed))
                                (equal "paid-native" (native-compaction-completed-event-response-id (first completed)))
                                (= 100 (json-get (native-compaction-completed-event-usage (first completed)) "input_tokens")))
                           "one native accounting notification including completed invalid artifacts"))
            (test-assert (every (lambda (event) (typep event '(or generation-progress-event native-compaction-completed-event))) events)
                         "no ordinary completion or opaque item event")
            (unless expected
              (test-assert (and (equal "compaction" (json-get artifact "type")) (= 100 (json-get usage "input_tokens")))
                           "only one canonical artifact and actual usage returned")))))
      (dolist (source (list "" (format nil "data: [DONE]~%~%") (format nil "data: {bad~%~%")
                            (test-sse-event-string (json-object "type" "response.completed" "response"
                                                               (json-object "status" "incomplete" "output" (vector opaque))))
                            (native-compaction-tests--source (list opaque) :terminal "incomplete")
                            (native-compaction-tests--source (list refused) :terminal "interrupted")
                            (test-sse-event-string (json-object "type" "response.output_item.done" "item" refused))))
        (let ((events nil))
          (test-assert
           (handler-case
               (progn (provider--decode-native-compaction-response provider (make-string-input-stream source)
                                                                   :status 200 :event-callback (lambda (event) (push event events))) nil)
             (provider-native-unusable () nil)
             (provider-error () t))
           "uncertain or malformed streams never authorize portable recovery")
          (test-assert (notany (lambda (event) (typep event 'native-compaction-completed-event)) events)
                       "no paid completed claim for uncertain boundary")))
      (let ((events nil))
        (test-assert
         (handler-case
             (progn (provider--decode-native-compaction-response
                     provider (make-string-input-stream (native-compaction-tests--source nil :usage nil))
                     :status 200 :event-callback (lambda (event) (push event events))) nil)
           (provider-native-unusable () t)) "completed missing artifact is unusable")
        (test-assert (and (= 1 (length events)) (null (native-compaction-completed-event-usage (first events))))
                     "unknown usage accounted exactly once"))
      (dolist (limit '(line event))
        (let ((*sse-maximum-line-characters*  (if (eq limit 'line) 32 4096))
              (*sse-maximum-event-characters* (if (eq limit 'event) 32 4096)))
          (test-assert
           (handler-case
               (progn (provider--decode-native-compaction-response
                       provider (make-string-input-stream (native-compaction-tests--source (list opaque))) :status 200) nil)
             (provider-error () t)) "native reads enforce retained line/event bounds")))))
  nil)

(-> test-native-compaction-window-replay () null)
(defun test-native-compaction-window-replay ()
  "Replay full native windows and project portable handoffs across provider families."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (window       (list (native-compaction-tests--opaque "first")
                               (json-object "type" "reasoning" "summary" (json-array)
                                            "encrypted_content" "retained-reasoning")
                               (native-compaction-tests--opaque "last"))))
      (conversation-append-user-message conversation "original")
      (conversation-append-native-compaction conversation window
                                             :family ':responses :summary "portable handoff")
      (let ((loaded (conversation-load-by-id configuration (conversation-identifier conversation))))
        (dolist (family '(:responses :messages))
          (let ((before (conversation-input-items-for-family conversation family))
                (after  (conversation-input-items-for-family loaded family)))
            (test-assert (equal (mapcar #'json-encode before) (mapcar #'json-encode after))
                         "reload reproduces the exact family projection")
            (test-assert
             (if (eq family ':responses)
                 (equal (mapcar #'json-encode window) (mapcar #'json-encode after))
                 (and (= 1 (length after))
                      (test-object-contains-string-p after "portable handoff")))
             "the source family receives the full ordered window and other families receive only the portable handoff")
            (test-assert (= (if (eq family ':responses) 2 0)
                            (count-if #'native-compaction-item-p after))
                         "all opaque items belong exclusively to their originating family"))))
      (let ((legacy (conversation-create configuration)))
        (conversation-append-record
         legacy (list :native-compaction :family ':responses
                      :through-seq       (1- (conversation-next-sequence legacy))
                      :wire-json         (json-encode (native-compaction-tests--opaque "legacy"))
                      :summary           "legacy handoff"))
        (let ((loaded (conversation-load-by-id configuration (conversation-identifier legacy))))
          (test-assert (= 1 (count-if #'native-compaction-item-p
                                      (conversation-input-items-for-family loaded ':responses)))
                       "legacy one-object archives are readable")))))
  nil)

(-> test-native-compaction-v2-retention () null)
(defun test-native-compaction-v2-retention ()
  "Exercise the prepared 64K whole-message suffix, image allowance and newest user."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (provider     (provider-create configuration))
           (artifact     (native-compaction-tests--opaque "checkpoint"))
           (older        (json-object "type" "message" "role" "user" "content"
                                      (vector (json-object "type" "input_text" "text" (make-string 256000 :initial-element #\x)))))
           (developer    (json-object "type" "message" "role" "developer" "content"
                                      (vector (json-object "type" "input_text" "text" "durable developer"))))
           (newest       (json-object "type" "message" "role" "user" "content"
                                      (vector (json-object "type" "input_text" "text" "newest user")
                                              (json-object "type" "input_image" "image_url" "data:image/png;base64,c3ludGhldGlj")))))
      (conversation-append-user-message conversation "must stop before this older row")
      (dolist (item (list older developer
                         (json-object "type" "message" "role" "assistant" "content" #())
                         (json-object "type" "reasoning" "summary" #() "encrypted_content" "old reasoning")
                         artifact newest))
        (conversation-append-provider-item conversation item))
      (conversation-append-provider-item conversation
                                         (json-object "type" "message" "role" "developer" "content"
                                                      (vector (json-object "type" "input_text" "text" "ephemeral developer")))
                                         :persistence ':next-response)
      (let ((window (provider--native-retained-window provider conversation artifact)))
        (test-assert (equal (mapcar #'json-encode window) (mapcar #'json-encode (list developer newest artifact)))
                     "chronological whole durable messages only, stopping at first nonfit")
        (test-assert (not (test-object-contains-string-p window "must stop before")) "no jumping over oversized eligible row")
        (let* ((wire      (provider-wire-input-item provider newest))
               (cost      (ceiling (provider-context--request-characters (vector wire)) 4))
               (text-only (json-object "type" "message" "role" "user" "content"
                                       (vector (json-object "type" "input_text" "text" "newest user")))))
          (test-assert (> cost (ceiling (provider-context--request-characters (vector text-only)) 4))
                       "prepared image estimate contributes to retention cost")))
      (let* ((boundary (conversation-create configuration))
             (item     (json-object "type" "message" "role" "user" "content"
                                   (vector (json-object "type" "input_text" "text" ""))))
             (part     (aref (json-get item "content") 0))
             (overhead (provider-context--request-characters (vector (provider-wire-input-item provider item)))))
        (setf (gethash "text" part) (make-string (- 256000 overhead) :initial-element #\a))
        (conversation-append-provider-item boundary item)
        (test-assert (= 64000 (ceiling (provider-context--request-characters
                                        (vector (provider-wire-input-item provider item))) 4)) "exact prepared budget boundary")
        (test-assert (= 2 (length (provider--native-retained-window provider boundary artifact))) "equality fits whole newest user")
        (setf (gethash "text" part) (concatenate 'string (json-get part "text") "1234"))
        (test-assert
         (handler-case (progn (provider--native-retained-window provider boundary artifact) nil)
           (provider-native-unusable () t)) "newest user overflow authorizes native-unusable, never silent loss"))))
  nil)

(defclass native-v2-test-provider (codex-subscription-provider)
  ((source :initarg :source :accessor native-v2-test-source :documentation "Synthetic native SSE text.")
   (status :initarg :status :initform 200 :reader native-v2-test-status :documentation "Synthetic HTTP status.")
   (on-open :initarg :on-open :initform nil :reader native-v2-test-on-open :documentation "Arrival or stop at actual native dispatch.")
   (opens :initform 0 :accessor native-v2-test-opens :documentation "Actual native dispatch count.")
   (stream :initform nil :accessor native-v2-test-stream :documentation "Owned stream for close assertions.")
   (summaries :initform nil :accessor native-v2-test-summaries :documentation "Portable request projections.")
   (summary-error-p :initarg :summary-error-p :initform nil :reader native-v2-test-summary-error-p
                    :documentation "Whether the portable handoff fails."))
  (:documentation "Real native lifecycle with deterministic native and summary transports."))

(defmethod provider-open-native-compaction
    ((provider native-v2-test-provider) (request hash-table) &key credentials conversation)
  "Open and retain one synthetic owned SSE stream."
  (declare (ignore request credentials conversation))
  (incf (native-v2-test-opens provider))
  (when (native-v2-test-on-open provider) (funcall (native-v2-test-on-open provider)))
  (values (setf (native-v2-test-stream provider)
                (make-instance 'test-character-input-stream :source (native-v2-test-source provider)))
          (native-v2-test-status provider) nil))

(defmethod provider-stream-turn
    ((provider native-v2-test-provider) (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Return a portable summary while recording its actual encoded request."
  (declare (ignore event-callback goal-context))
  (test-assert compaction-p "native success and recovery both obtain portable handoffs")
  (push (provider-request-object provider conversation tool-namespaces :compaction-p t)
        (native-v2-test-summaries provider))
  (when (native-v2-test-summary-error-p provider)
    (error 'provider-protocol-error :message "Synthetic handoff failure."))
  (agent-test-result "portable-paid" (list (agent-test-compaction-message)) :turn-completion ':end))

(defclass native-v2-test-observer (agent-observer)
  ((causes :initform nil :accessor native-v2-test-causes :documentation "Actual recovery conditions.")
   (fail-p :initarg :fail-p :initform nil :reader native-v2-test-fail-p :documentation "Inject an observer failure."))
  (:documentation "Observe exact native recoverable causes without payload events."))

(defmethod agent-observer-status ((observer native-v2-test-observer) status details)
  "Record actual recovery conditions and optionally fail progress observation."
  (when (eq status ':native-compaction-recovery) (push details (native-v2-test-causes observer)))
  (when (and (native-v2-test-fail-p observer) (eq status ':provider-progress))
    (error 'provider-native-unusable :message "Synthetic observer failure, not a native result.")))

(-> test-native-compaction-v2-fallback () null)
(defun test-native-compaction-v2-fallback ()
  "Exercise agent accounting, one portable fallback and distinct denial/uncertainty stops."
  (let* ((opaque  (native-compaction-tests--opaque "native-success"))
         (refusal (json-object "type" "message" "role" "assistant" "content"
                               (vector (json-object "type" "refusal" "refusal" "Synthetic content refusal.")))))
    (dolist (row (list (list 200 (native-compaction-tests--source (list opaque)) t nil)
                       (list 404 "" t nil) (list 405 "" t nil) (list 501 "" t nil)
                       (list 200 (native-compaction-tests--source nil) t 'provider-native-unusable)
                       (list 200 (native-compaction-tests--source nil :usage nil) t 'provider-native-unusable)
                       (list 200 (native-compaction-tests--source (list opaque refusal)) t 'provider-native-refusal)
                       (list 200 (concatenate 'string
                                              (test-sse-event-string (json-object "type" "response.refusal.done" "refusal" "Explicit bounded refusal."))
                                              (native-compaction-tests--source (list opaque))) t 'provider-native-refusal)
                       (list 200 (test-sse-event-string (json-object "type" "response.refusal.done" "refusal" "Fragment only.")) nil nil)
                       (list 200 (test-sse-event-string
                                  (json-object "type" "response.failed" "response"
                                               (json-object "status" "failed" "id" "paid-refusal"
                                                            "usage" (json-object "input_tokens" 100 "output_tokens" 10)
                                                            "error" (json-object "code" "content_policy_violation" "message" "Definitive refusal."))))
                             t 'provider-native-refusal)
                       (list 403 "permission denied" nil nil) (list 429 (json-encode (json-object "error" (json-object "type" "usage_limit_reached"))) nil nil)
                       (list 200 (format nil "data: {bad~%~%") nil nil)
                       (list 200 (test-sse-event-string (json-object "type" "response.output_item.done" "item" refusal)) nil nil)
                       (list 200 (native-compaction-tests--source (list refusal) :terminal "incomplete") nil nil)))
      (destructuring-bind (status source succeeds-p cause-type) row
        (with-test-configuration (configuration)
          (let* ((conversation (conversation-create configuration))
                 (provider     (make-instance 'native-v2-test-provider :configuration configuration :source source :status status
                                              :credential-manager (credential-manager-create configuration) :session-id (make-identifier)))
                 (observer     (make-instance 'native-v2-test-observer))
                 (agent        (agent-create :configuration configuration :conversation conversation :provider provider
                                             :tool-registry (agent-test-registry) :worker nil))
                 (failure nil) (issued nil))
            (credential-source-save (credential-manager-primary-source (provider-credential-manager provider))
                                    (provider-tests--credentials configuration))
            (conversation-append-user-message conversation "original portable source")
            (conversation-append-provider-item conversation (agent-test-message "completed earlier turn"))
            (conversation-append-user-message conversation "penultimate tail user")
            (conversation-append-provider-item conversation (agent-test-message "completed middle turn"))
            (conversation-append-user-message conversation "active tail user")
            (let ((before (mapcar #'json-encode (conversation-input-items conversation))))
              (handler-case (setf issued (agent-compact-conversation agent observer))
                (error (condition) (setf failure condition)))
              (test-assert (eq succeeds-p (null failure)) (format nil "status ~D exact agent outcome (~A)" status failure))
              (test-assert (= 1 (native-v2-test-opens provider)) "native content is never rephrased or dispatched twice")
              (test-assert (not (open-stream-p (native-v2-test-stream provider))) "owned native stream closed on every outcome")
              (test-assert (= (if succeeds-p 1 0) (length (native-v2-test-summaries provider))) "one authorized portable path, no denial reroute")
              (when cause-type
                (test-assert (and (= 1 (length (native-v2-test-causes observer)))
                                  (typep (first (native-v2-test-causes observer)) cause-type)) "actual typed cause reaches observer")
                (test-assert (test-object-contains-string-p (first (native-v2-test-summaries provider)) "original portable source")
                             "fallback summarizes original capture, not unusable native output"))
              (if succeeds-p
                  (progn
                    (test-assert (= 2 issued) "native request and portable summary counted exactly once")
                    (test-assert (= 1 (count-if (lambda (record) (member (first record) '(:summary :native-compaction)))
                                               (let ((records nil))
                                                 (conversation-map-records conversation (lambda (record) (push record records)))
                                                 records))) "one checkpoint published")
                    (let ((loaded (conversation-load-by-id configuration (conversation-identifier conversation))))
                      (test-assert (equal (mapcar #'json-encode (conversation-input-items conversation))
                                          (mapcar #'json-encode (conversation-input-items loaded))) "replay exact published projection")
                      (test-assert (not (test-object-contains-string-p (conversation-input-items-for-family loaded ':messages)
                                                                      "native-success")) "other family receives portable handoff")))
                  (test-assert (equal before (mapcar #'json-encode (conversation-input-items conversation)))
                               "uncertain or denied native request publishes nothing"))))))))
  nil)

(-> test-native-compaction-v2-deadlines () null)
(defun test-native-compaction-v2-deadlines ()
  "Bound native opens/retry delays/reads together and never reopen after generation."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (provider     (provider-create configuration))
           (opaque       (native-compaction-tests--opaque "clock-checkpoint")))
      (credential-source-save (credential-manager-primary-source (provider-credential-manager provider))
                              (provider-tests--credentials configuration))
      (conversation-append-user-message conversation "clock source")
      (dolist (mode '(:safe-retry :no-callback :expired-retry :expired-lines :generated-eof
                      :partial-first-line :partial-first-frame :observer :cancel :caller-deadline
                       :late-open :generated-late :total-expired))
        (let* ((clock 0) (opens 0) (streams nil) (events nil) (failure nil) (window nil)
               (*provider-native-clock*        (lambda () clock))
               (*bounded-retry-sleep-function* (lambda (delay) (declare (ignore delay))
                                                 (incf clock (if (eq mode ':expired-retry) 121 10))))
               (reader                         *sse-read-line-function*)
               (*sse-read-line-function*
                 (lambda (stream)
                   (when (eq mode ':expired-lines) (incf clock 45))
                   (when (eq mode ':caller-deadline) (sleep 0.05))
                   (funcall reader stream))))
          (test-call-with-function-replacements
           (list
            (list 'provider-post-event-stream
                  (lambda (&rest arguments)
                    (declare (ignore arguments))
                    (incf opens)
                    (when (and (= opens 1) (member mode '(:safe-retry :expired-retry)))
                      (error 'provider-transport-error :message "Synthetic safe opening failure."))
                    (let* ((partial-p (and (= opens 1)
                                           (member mode '(:partial-first-line :partial-first-frame))))
                           (stream
                             (make-instance
                              (if (or partial-p (eq mode ':generated-eof))
                                  'test-tls-eof-stream
                                  'test-character-input-stream)
                              :source
                              (cond
                                (partial-p
                                 (if (eq mode ':partial-first-line)
                                     "data: {\"type\":\"response.output_item.done\",\"item\":{\"type\":\"compaction\",\"encrypted_content\":\"partial"
                                     (format nil "data: ~A~%"
                                             (json-encode (json-object "type" "response.output_item.done" "item" opaque)))))
                                ((eq mode ':generated-eof)
                                 (test-sse-event-string (json-object "type" "response.in_progress")))
                                (t
                                 (concatenate 'string
                                              (if (eq mode ':expired-lines) (format nil ": keepalive~%: keepalive~%: keepalive~%") "")
                                              (native-compaction-tests--source (list opaque))))))))
                      (when (eq mode ':late-open) (setf clock 121))
                      (push stream streams) (values stream 200 nil)))))
           (lambda ()
             (handler-case
                 (flet ((dispatch ()
                          (provider-native-compact-conversation
                           provider conversation :tool-namespaces #()
                           :event-callback
                           (unless (eq mode ':no-callback)
                             (lambda (event)
                              (push event events)
                              (when (typep event 'generation-progress-event)
                                (case mode (:generated-late
                                            (setf clock 160))
                                           (:total-expired
                                            (setf clock 301))))
                              (when (and (eq mode ':cancel) (typep event 'generation-progress-event))
                                (error 'application-turn-cancelled))
                              (when (and (eq mode ':observer) (typep event 'generation-progress-event))
                                (error 'provider-native-unusable :message "Observer stop.")))))))
                   (if (eq mode ':caller-deadline)
                       (provider-call-with-response-deadline 0.01 #'dispatch)
                       (setf window (dispatch))))
               (application-turn-cancelled (condition) (setf failure condition))
               (error (condition) (setf failure condition)))))
          (test-assert (if (member mode '(:safe-retry :no-callback :generated-late)) (null failure) failure) (format nil "native clock mode ~S outcome" mode))
          (test-assert (= opens (if (eq mode ':safe-retry) 2 1)) (format nil "shared expiry and generation prevent another open: ~S opens ~D failure ~A" mode opens failure))
          (test-assert (every (lambda (stream) (not (open-stream-p stream))) streams) "close on success, timeout, callback and generation failure")
          (when (eq mode ':generated-eof)
            (test-assert (some (lambda (event) (typep event 'generation-progress-event)) events) "payload-free generation evidence forwarded"))
          (when (member mode '(:partial-first-line :partial-first-frame))
            (test-assert (typep failure 'provider-protocol-error) "ambiguous native body failure is terminal uncertainty")
            (test-assert (null window) "partial first event cannot accept a later generation")
            (test-assert (notany (lambda (event)
                                   (typep event '(or generation-progress-event
                                                    native-compaction-completed-event
                                                    native-compaction-recovery-event)))
                                 events)
                         "uncertainty emits neither fabricated progress, accounting nor portable recovery"))
          (when (eq mode ':observer)
            (test-assert (and (typep failure 'provider-native-unusable)
                              (notany (lambda (event) (typep event 'native-compaction-recovery-event)) events))
                         "observer error is not a recoverable native result"))))))
  nil)

(-> test-native-compaction-v2-publication () null)
(defun test-native-compaction-v2-publication ()
  "Recover from final encoded overflow, and preserve arrivals or failed publication."
  (dolist (mode '(:arrivals :overflow :budget-overflow :summary-failure :request-limit :mission-stop :cancel-stop :stale))
    (with-test-configuration (configuration)
      (let* ((*context-contributors*                      nil)
             (*context-resolver*                          (cl-llm-provider-api:make-context-resolver))
             (*context-last-deliveries*                   (make-hash-table :test #'equal))
             (*context-last-delivery-order*               nil)
             (current-text                                "Captured current block.")
             (conversation                                (conversation-create configuration))
             (artifact                                    (native-compaction-tests--opaque "publication-opaque"))
             (concurrent-projection                       nil)
             (provider                                    (make-instance 'native-v2-test-provider :configuration configuration
                                                                         :credential-manager                     (credential-manager-create configuration)
                                                                         :session-id                             (make-identifier)
                                                                         :source                                 (native-compaction-tests--source (list artifact))
                                                                         :summary-error-p                        (eq mode ':summary-failure)
                                                                         :on-open
                                                                         (lambda ()
                                                                           (setf current-text "Fresh after capture block.")
                                                                           (case mode
                                                                             (:mission-stop
                                                                              (error 'mission-error :reason ':inference-budget :message "Synthetic mission boundary."))
                                                                             (:cancel-stop
                                                                              (error 'application-turn-cancelled))
                                                                             (:stale
                                                                              (conversation-append-summary conversation "Concurrent legitimate checkpoint.")
                                                                              (setf concurrent-projection (mapcar #'json-encode (conversation-input-items conversation))))
                                                                             (:arrivals
                                                                              (conversation-append-provider-item
                                                                               conversation (json-object "type" "function_call_output" "call_id" "arrival-call" "output" "arrived tool result"))
                                                                              (conversation-append-user-message conversation "arrived during native"))))))
             (agent                                       (agent-create :configuration configuration :conversation conversation :provider provider
                                                                        :tool-registry (agent-test-registry) :worker nil))
             (observer                                    (make-instance 'native-v2-test-observer))
             (original-admit                              #'provider-check-request-context)
             (*agent-maximum-provider-requests-per-turn*  (if (eq mode ':request-limit) 1 512))
             (*agent-compaction-maximum-summary-requests* (if (eq mode ':budget-overflow) 1 4))
             (overflow-count 0) (failure nil) (issued nil))
        (register-context-contributor
         "native-publication-current"
         (lambda (context)
           (declare (ignore context))
           (list (make-context-contribution :identifier "native-current" :instruction current-text))))
        (credential-source-save (credential-manager-primary-source (provider-credential-manager provider))
                                (provider-tests--credentials configuration))
        (conversation-append-user-message conversation "original fallback input")
        (conversation-append-provider-item conversation (agent-test-message "old completed answer"))
        (conversation-append-user-message conversation "middle user")
        (conversation-append-provider-item conversation (agent-test-message "middle answer"))
        (conversation-append-user-message conversation "latest durable user")
        (when (eq mode ':arrivals)
          (conversation-append-provider-item
           conversation (json-object "type" "function_call" "call_id" "arrival-call" "name" "complete" "arguments" "{}")))
        (let ((before (mapcar #'json-encode (conversation-input-items conversation))))
          (test-call-with-function-replacements
           (list (list 'provider-check-request-context
                       (lambda (actual-provider request)
                         (when (and (member mode '(:overflow :budget-overflow)) (native-v2-test-summaries provider)
                                    (test-object-contains-string-p request "publication-opaque"))
                           (incf overflow-count)
                           (error 'provider-context-budget-error :message "Synthetic final encoded overflow."
                                  :estimated-input-tokens 100 :output-reserve 1 :context-window 50 :non-text-estimate-p t))
                         (funcall original-admit actual-provider request))))
           (lambda ()
             (handler-case
                 (setf issued (agent-compact-conversation agent observer))
               (application-turn-cancelled (condition) (setf failure condition))
               (error (condition) (setf failure condition)))))
          (test-assert (= 1 (native-v2-test-opens provider)) "one native dispatch for publication")
          (test-assert (or (null (native-v2-test-stream provider))
                           (not (open-stream-p (native-v2-test-stream provider)))) "publication owns no open native stream")
          (case mode
            ((:mission-stop :cancel-stop :stale)
             (test-assert failure "mission, cancellation and stale capture are terminal")
             (test-assert (typep failure (case mode (:mission-stop
                                                     'mission-error)
                                             (:cancel-stop
                                              'application-turn-cancelled)
                                             (otherwise
                                              'conversation-error))) (format nil "~A terminal condition: ~A" mode failure))
             (test-assert (= (if (eq mode ':stale) 1 0) (length (native-v2-test-summaries provider)))
                          (format nil "~A portable count ~D" mode (length (native-v2-test-summaries provider))))
             (test-assert (equal (or concurrent-projection before) (mapcar #'json-encode (conversation-input-items conversation)))
                          "terminal stop preserves exact current durable projection"))
            (:budget-overflow
             (test-assert (typep failure 'provider-context-budget-error)
                          "Final native no-fit cannot renew the exhausted summary budget.")
             (test-assert (= 1 (length (native-v2-test-summaries provider)))
                          "Native and portable handoffs share one summary request bound.")
             (test-assert (= 1 overflow-count) "Only the native publisher rejects its candidate.")
             (test-assert (equal before (mapcar #'json-encode (conversation-input-items conversation)))
                          "Exhausted handoff publishes no native or portable checkpoint."))
            ((:summary-failure :request-limit)
             (test-assert failure "handoff failure or exhausted request allowance stops publication")
             (test-assert (= (if (eq mode ':request-limit) 0 1) (length (native-v2-test-summaries provider)))
                          "remaining allowance gates the portable dispatch")
             (test-assert (equal before (mapcar #'json-encode (conversation-input-items conversation)))
                          "failed handoff publishes no checkpoint"))
            (otherwise
             (test-assert (null failure) (format nil "publication succeeds: ~A" failure))
             (test-assert (= (if (eq mode ':overflow) 3 2) issued) "all paid attempts counted once")
             (let* ((request (provider-request-object provider conversation #()))
                    (loaded  (conversation-load-by-id configuration (conversation-identifier conversation))))
               (provider-check-request-context provider request)
               (test-assert (and (test-object-contains-string-p request "Fresh after capture block.")
                                 (not (test-object-contains-string-p request "Captured current block.")))
                            "ordinary request renders fresh current context after native capture")
               (test-assert (not (test-object-contains-string-p (conversation-input-items loaded) "Fresh after capture block."))
                            "rendered context is not retained as durable native history")
               (test-assert (test-object-contains-string-p request "latest durable user") "actual next ordinary request keeps latest user")
               (test-assert (equal (mapcar #'json-encode (conversation-input-items conversation))
                                   (mapcar #'json-encode (conversation-input-items loaded))) "reopen exactly matches publication")
               (test-assert (not (test-object-contains-string-p (conversation-input-items-for-family loaded ':messages)
                                                               "publication-opaque")) "portable family excludes opaque artifact")
               (if (eq mode ':overflow)
                   (progn
                     (test-assert (= 1 overflow-count) "only native reconstructed request fails admission")
                     (test-assert (= 2 (length (native-v2-test-summaries provider))) "native handoff then one portable fallback")
                     (test-assert (test-object-contains-string-p (first (native-v2-test-summaries provider))
                                                               "original fallback input") "late overflow uses original captured source")
                     (test-assert (and (= 1 (length (native-v2-test-causes observer)))
                                       (provider-native-unusable-usage (first (native-v2-test-causes observer))))
                                  "late recovery reports accounted native usage"))
                   (progn
                     (test-assert (test-object-contains-string-p request "arrived during native")
                                  "arrival survives in the next actual request")
                     (test-assert (and (= 1 (count-if (lambda (item) (equal (json-get item "type") "function_call"))
                                                      (json-get request "input")))
                                       (= 1 (count-if (lambda (item) (equal (json-get item "type") "function_call_output"))
                                                      (json-get request "input"))))
                                  "active call and arriving result survive as one complete pair"))))))))))
  nil)
