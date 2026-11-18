(in-package #:autolith)

;;;; -- Owned recovery presentation --

(-> provider-status-test-details (t) list)
(defun provider-status-test-details (event)
  "Recognize the event by protocol name so the baseline can causally lack notifications."
  (when (eq (class-name (class-of event)) 'provider-connection-event)
    (slot-value event 'details)))

(-> provider-status-test-call (configuration function function &key (:state t)) t)
(defun provider-status-test-call (configuration attempt callback &key state)
  "Run the real retry wrapper without opening a provider or accessing credentials."
  (let ((provider (provider-create configuration)))
    (if state
        (provider--call-with-bounded-retries provider attempt callback :recovery state)
        (provider--call-with-bounded-retries provider attempt callback))))

(-> provider-status-test-with-endpoints (list function) *)
(defun provider-status-test-with-endpoints (endpoints function)
  "Supply tainted derived endpoint values through a fixture-local read boundary."
  (let ((original (symbol-function 'config)))
    (test-call-with-function-replacements
     (list (list 'config
                 (lambda (&rest arguments)
                   (let ((entry (and (eq (first arguments) :provider-endpoint)
                                     (assoc (second arguments) endpoints))))
                     (if entry (rest entry) (apply original arguments))))))
     function)))

(-> test-provider-status-lifecycle () null)
(defun test-provider-status-lifecycle ()
  "Observe actual retry dispatch, route changes, terminal unwind and tainted input containment."
  (with-test-configuration (configuration)
    (let* ((events nil) (attempts 0)
           (state                          (make-provider-connection-recovery))
           (provider                       (provider-create configuration))
           (alternate-configuration        (configuration-copy configuration))
           (alternate                      nil)
           (*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds)))))
      (setf alternate (provider-create alternate-configuration)
            (provider-connection-recovery-route-count state) 2
            (provider-connection-recovery-route-threshold state) 1)
      (test-assert
       (equal '(done second)
              (multiple-value-list
               (provider-status-test-with-endpoints
                (list (cons configuration "https://user:credential@safe.example:8443/private/path?token=secret#fragment")
                      (cons alternate-configuration "https://other:password@alternate.example/private?secret=yes"))
                (lambda ()
                 (provider--call-with-bounded-retries
                  provider
                  (lambda (refresh callback)
                    (declare (ignore refresh callback))
                    (if (= (incf attempts) 1)
                        (error 'provider-opening-instability :category :connection
                               :message                                "exception-secret body header authorization")
                        (values 'done 'second)))
                  (lambda (event)
                    (when (provider-status-test-details event)
                      (push (provider-status-test-details event) events)))
                  :recovery        state
                  :select-provider (lambda ()
                                     (if (zerop (provider-connection-recovery-route-index state))
                                         provider alternate)))))))
       "recovery status preserves all success values")
      (setf events (nreverse events))
      (test-assert (equal '(:attempting :waiting :route-changed :attempting :completed)
                          (mapcar (lambda (details) (getf details :phase)) events))
                   "real retry dispatch publishes its ordered lifecycle")
      (test-assert (and (equal "safe.example:8443" (getf (first events) :endpoint))
                        (equal "alternate.example:443" (getf (third events) :endpoint))
                        (= 1 (getf (second events) :attempt))
                        (integerp (getf (second events) :next-retry-at))
                        (null (getf (first (last events)) :next-retry-at)))
                   "endpoint, attempt and next retry describe the selected route")
      (test-assert (notany (lambda (secret) (search secret (prin1-to-string events)))
                           '("credential" "password" "private" "secret" "fragment" "authorization"))
                   "no tainted endpoint components or raw failure payload reaches status")))
  nil)

(-> test-provider-status-headless-and-observer () null)
(defun test-provider-status-headless-and-observer ()
  "Dispatch through the actual callback observer and preserve exact observer errors."
  (with-test-configuration (configuration)
    (let* ((statuses nil) (attempts 0)
           (observer                       (make-instance 'callback-agent-observer
                                                         :status-callback (lambda (status details)
                                                                            (push (list status details) statuses))))
           (*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds)))))
      (provider-status-test-call
       configuration
       (lambda (refresh callback)
         (declare (ignore refresh callback))
         (if (= 1 (incf attempts))
             (error 'provider-opening-instability :category :connection :message "tainted")
             :ok))
       (agent--provider-event-callback observer))
      (test-assert (= 4 (count :provider-connection statuses :key #'first))
                   "headless observers receive actual owned recovery events")
      (let ((failure (make-condition 'provider-opening-instability
                                     :category :connection :message "observer-secret"))
            (calls 0) (dispatches 0))
        (test-assert
         (eq failure
             (handler-case
                 (provider-status-test-call
                  configuration
                  (lambda (refresh callback)
                    (declare (ignore refresh callback))
                    (incf dispatches))
                  (lambda (event)
                    (when (provider-status-test-details event)
                      (incf calls)
                      (error failure))))
               (error (condition) condition)))
         "the exact observer condition escapes all retry scopes")
        (test-assert (and (= 1 calls) (zerop dispatches))
                     "observer failure neither dispatches nor calls a broken observer again"))))
  nil)

(-> test-provider-status-terminal () null)
(defun test-provider-status-terminal ()
  "Terminate owned status on terminal failure, programmatic cancellation and deadline."
  (with-test-configuration (configuration)
    (dolist (mode '(:failure :cancel :deadline))
      (let* ((events nil) (owner (list mode))
             (*provider-connection-owner* owner)
             (state                       (make-provider-connection-recovery))
             (failure                     (make-condition 'provider-error :message "raw-secret"))
             (*bounded-retry-sleep-function*
               (lambda (seconds)
                 (declare (ignore seconds))
                 (case mode
                   (:cancel
                    (provider-cancel-connection-owner owner))
                   (:deadline
                    (setf (provider-connection-recovery-deadline state) 0))))))
        (handler-case
            (provider-status-test-call
             configuration
             (lambda (refresh callback)
               (declare (ignore refresh callback))
               (if (eq mode :failure) (error failure)
                   (error 'provider-opening-instability :category :connection :message "raw-secret")))
             (lambda (event)
               (when (provider-status-test-details event)
                 (push (provider-status-test-details event) events)))
             :state state)
          (provider-error () nil))
        (test-assert (eq (getf (first events) :phase)
                         (if (eq mode :failure) :terminated :cancelled))
                     "each ownership boundary publishes an owned terminal event")
        (test-assert (and (null (getf (first events) :next-retry-at))
                          (provider-connection-recovery-ended-p state))
                     "terminal status has no countdown and its episode is ended"))))
  nil)

(-> test-provider-status-consumers () null)
(defun test-provider-status-consumers ()
  "Exercise terminal rendering, ACP notification formatting and child progress dispatch."
  (with-test-configuration (configuration)
    (let* ((terminal                       (make-instance 'recording-terminal :columns 120))
           (conversation                   (conversation-create configuration :identifier "status-fixture"))
           (application                    (make-instance 'application :configuration configuration
                                                          :conversation               conversation
                                                          :tool-registry              (make-default-tool-registry)
                                                          :ui                         (terminal-ui-create :terminal terminal)))
           (terminal-observer              (application-agent-observer application))
           (session                        (make-instance 'acp-session :service (acp-observer-test-service configuration)
                                                          :identifier "status-fixture" :application application))
           (acp-observer                   (make-instance 'acp-observer :session session))
           (orchestrator                   (task-orchestrator-create))
           (job                            (task-tests--make-job orchestrator :identifier "status-child"))
           (notifications nil) (child-events nil) (attempts 0)
           (child-observer
             (make-instance 'callback-agent-observer
                            :status-callback (lambda (status details)
                                               (task-progress-note-status job status details))))
           (callbacks                      (mapcar #'agent--provider-event-callback
                                                   (list terminal-observer acp-observer child-observer)))
           (*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds)))))
      (terminal-ui-start (application-ui application))
      (unwind-protect
           (test-call-with-function-replacements
            (list
             (list 'agentcomms:agent-client-notify
                   (lambda (service method params)
                     (declare (ignore service))
                     (push (list method params) notifications)))
             (list 'task-orchestrator-emit
                   (lambda (owner kind event)
                     (declare (ignore owner kind))
                     (push event child-events))))
            (lambda ()
              (provider-status-test-with-endpoints
               (list (cons configuration "https://name:password@visible.example:9443/private?secret=yes#fragment"))
               (lambda ()
                (provider-status-test-call
                 configuration
                 (lambda (refresh callback)
                   (declare (ignore refresh callback))
                   (if (= (incf attempts) 1)
                       (error 'provider-opening-instability :category :connection :message "exception-secret")
                       :ok))
                 (lambda (event) (dolist (callback callbacks) (funcall callback event))))))
              (let ((output (clinedi:ansi-strip (recording-terminal-output terminal))))
                (test-assert (and (search "visible.example:9443" output)
                                  (search "WAITING" output) (search "COMPLETED" output))
                             "real terminal observer renders owned waiting and terminal notices")
                (test-assert (notany (lambda (secret) (search secret output))
                                     '("password" "private" "secret" "fragment"))
                             "terminal output contains only sanitized endpoint identity"))
              (test-assert (= 4 (length notifications))
                           "actual ACP observer dispatches four extension notifications")
              (let* ((envelope (second (first notifications)))
                     (data     (agentcomms:json-get envelope "data")))
                (test-assert (and (equal "_autolith/event" (first (first notifications)))
                                  (equal "provider-connection" (agentcomms:json-get envelope "kind"))
                                  (equal "completed" (agentcomms:json-get data "phase"))
                                  (equal "visible.example:9443" (agentcomms:json-get data "endpoint")))
                             "ACP recovery status uses its real session-owned event envelope"))
              (test-assert (= 4 (count-if (lambda (event) (getf event :provider-connection)) child-events))
                           "actual child observer emits structured recovery progress")
              (test-assert (null (slot-value (task-job-progress job) 'provider-connections))
                           "completion clears the exact child recovery status")))
        (terminal-ui-stop (application-ui application))
        (task-orchestrator-close orchestrator))))
  nil)

(-> test-provider-status-concurrent-owners () null)
(defun test-provider-status-concurrent-owners ()
  "Run two real owned waiters and cancel only one while the unrelated request remains active."
  (with-test-configuration (configuration)
    (let* ((owners    (vector (list :first) (list :second)))
           (states    (make-array 2 :initial-element nil))
           (events    (make-array 2 :initial-element nil))
           (observers (make-array 2 :initial-element nil))
           (ready     (sb-thread:make-semaphore :count 0))
           (threads   nil))
      (unwind-protect
           (progn
             (dotimes (index 2)
               (let ((index index))
                (setf (aref observers index)
                      (make-instance
                       'serialized-agent-observer
                       :delegate (make-instance
                                  'callback-agent-observer
                                  :status-callback
                                  (lambda (status details)
                                    (when (eq :provider-connection status)
                                      (push details (aref events index))
                                      (when (eq :waiting (getf details :phase))
                                        (sb-thread:signal-semaphore ready)))))))
                (push
                 (sb-thread:make-thread
                  (lambda ()
                    (let* ((*provider-connection-owner* (aref owners index))
                           (state                       (make-provider-connection-recovery)))
                      (setf (aref states index) state)
                      (handler-case
                          (provider-status-test-call
                           (configuration-copy configuration)
                           (lambda (refresh callback)
                             (declare (ignore refresh callback))
                             (error 'provider-opening-instability :category :connection :message "tainted"))
                           (agent--provider-event-callback (aref observers index))
                           :state state)
                        (provider-owner-ended () nil))))
                  :name "Owned recovery status fixture")
                 threads)))
             (test-assert (and (sb-thread:wait-on-semaphore ready :timeout 2)
                               (sb-thread:wait-on-semaphore ready :timeout 2))
                          "both unrelated requests reach their actual native wait")
             (dotimes (index 2)
               (test-assert
                (sb-sys:with-deadline (:seconds 1)
                  (with-recursive-lock-held ((serialized-agent-observer-lock (aref observers index))) t))
                "observer serialization locks are released during actual retry waits"))
             (provider-cancel-connection-owner (aref owners 0))
             (sb-thread:join-thread (second threads) :timeout 2 :default :timeout)
             (test-assert (and (eq :cancelled (getf (first (aref events 0)) :phase))
                               (not (provider-connection-recovery-ended-p (aref states 1)))
                               (eq :waiting (getf (first (aref events 1)) :phase)))
                          "exact-owner stop terminates only its own status")
             (test-assert (not (equal (getf (first (aref events 0)) :episode)
                                      (getf (first (aref events 1)) :episode)))
                          "concurrent status identities cannot overwrite each other"))
        (dotimes (index 2) (provider-cancel-connection-owner (aref owners index)))
        (dolist (thread threads) (sb-thread:join-thread thread :timeout 2 :default :timeout)))))
  nil)

(defclass provider-status-telemetry-provider (codex-subscription-provider)
  ((attempts :initform 0 :accessor provider-status-telemetry-attempts
             :documentation "Number of owned local telemetry fixture dispatches."))
  (:documentation "An ordinary provider method that fails once with tainted transport data."))

(defmethod provider-attempt-turn
    ((provider provider-status-telemetry-provider) (conversation conversation)
     &key tool-namespaces event-callback force-refresh goal-context compaction-p)
  "Exercise the real telemetry around method without opening a socket or credentials."
  (declare (ignore conversation tool-namespaces event-callback force-refresh goal-context compaction-p))
  (when (= 1 (incf (provider-status-telemetry-attempts provider)))
    (error 'provider-opening-instability :category :connection
           :message "exception-private-secret" :request-id "account-secret"
           :response                               "body-secret"))
  (agent-test-result "owned-telemetry-result" nil))

(-> test-provider-status-telemetry () null)
(defun test-provider-status-telemetry ()
  "Export existing bounded model categories and counters, never recovery authorities or payloads."
  (test-telemetry--call
   (lambda (configuration)
     (let* ((provider                       (make-instance 'provider-status-telemetry-provider
                                                           :configuration      configuration
                                                           :credential-manager (credential-manager-create configuration)
                                                           :session-id         (make-identifier)))
            (conversation                   (conversation-create configuration :identifier "telemetry-status"))
            (*bounded-retry-sleep-function* (lambda (seconds) (declare (ignore seconds)))))
       (multiple-value-bind (bodies results)
           (test-telemetry-hooks--capture
            configuration
            (lambda ()
              (provider-status-test-with-endpoints
               (list (cons configuration "https://name:password@telemetry-private.example/private?secret=yes"))
               (lambda ()
                 (provider-stream-turn provider conversation :tool-namespaces #()
                                       :event-callback                        #'identity)))))
         (test-assert (typep (first results) 'provider-result) "telemetry preserves ordinary provider completion")
         (test-telemetry-hooks--assert-attempts (first bodies) 2)
         (test-assert (notany (lambda (secret) (search secret (prin1-to-string bodies)))
                              '("password" "telemetry-private" "private-secret" "account-secret" "body-secret"))
                      "actual exported attempt envelopes contain no endpoint or raw condition payload")
         (test-assert (eq :cancelled (telemetry--condition-error-type
                                      (make-condition 'provider-owner-ended :message "secret")))
                      "owner termination uses the existing cancelled category")))))
  nil)
