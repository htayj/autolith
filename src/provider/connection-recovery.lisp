(in-package #:autolith)

;;;; -- Request-local connection recovery --

(defparameter *provider-opening-timeout-seconds* 30
  "Independent bound for opening one provider response, in seconds.")

(defvar *provider-connection-episode* nil
  "The current dynamically owned provider episode, or NIL outside dispatch.")

(defvar *provider-connection-owner* nil
  "Exact caller identity for cancellation, never a durable route preference.")

(defvar *provider-connection-owner-check* nil
  "Optional caller check returning remaining seconds, or signalling ownership loss.")

(defvar *provider-connection-episodes* nil
  "Live episodes registered for exact-owner cancellation.")

(defvar *provider-connection-episodes-lock* (make-lock "Provider episodes")
  "Protects registration only, never dispatch or waits.")

(defvar *provider-connection-owner-generations* (make-hash-table :test 'eq :weakness ':key)
  "Ephemeral cancellation epochs for exact owners, including unregistered episodes.")

(defvar *provider-suspended-connections* (make-hash-table :test 'eq :weakness ':key)
  "Numeric retry metadata retired by a finite slice, keyed by exact operation identity.")

(defvar *provider-connection-operation* nil
  "Lexical ordinary request identity retained only while its actual caller can resume.")

(defvar *provider-connection-capture* nil
  "Current invocation's conversation, state and configuration cell, never retained after unwind.")

(defstruct provider-operation
  "Ephemeral ordinary dispatch identity and scalar caller allowances. Never a saved request."
  conversation goal epoch deadline owner-scope
  (state :new) busy-p
  (request-number 0) (tool-rounds 0) (tool-calls 0) (soft-attempts 0)
  (tools-p t) tool-restriction-p tool-allowlist)

(defstruct provider-operation-owner
  "Outer work scope responsible for its exact operation through publication and disposal."
  operation published-p active-p)

(define-condition provider-operation-refused (provider-error)
  ()
  (:documentation "An explicit resume is stale, unowned or not authorized; never dispatch fresh."))

(define-condition provider-operation-transfer (provider-error)
  ((owner :initarg :owner :reader provider-operation-transfer-owner
          :documentation "Already-established outer scope which owns this transfer.")
   (operation :initarg :operation :reader provider-operation-transfer-operation
              :documentation "Exact ephemeral identity, with no dispatch resources."))
  (:documentation "Nonterminal handoff after a settled slice and explicit review barrier."))

(-> provider-operation-dispose (provider-operation) null)
(defun provider-operation-dispose (operation)
  "Dispose only OPERATION's metadata; stale queued consumers must fail closed."
  (with-lock-held (*provider-connection-episodes-lock*)
    (remhash operation *provider-suspended-connections*)
    (setf (provider-operation-state operation) :disposed
          (provider-operation-busy-p operation) nil))
  nil)

(-> provider-operation-owner-dispose (provider-operation-owner) null)
(defun provider-operation-owner-dispose (owner)
  "Release OWNER's exact operation at every terminal work boundary."
  (let ((operation (provider-operation-owner-operation owner)))
    (when (and operation (eq owner (provider-operation-owner-scope operation)))
      (provider-operation-dispose operation)))
  nil)

(-> provider-call-with-operation-owner (provider-operation-owner function) *)
(defun provider-call-with-operation-owner (owner function)
  "Cover dispatch, transfer acceptance and publication with terminal cleanup.
FUNCTION handles its own typed transfer inside this scope. Only successful explicit
publication transfers disposal responsibility to the outer queue owner."
  (with-lock-held (*provider-connection-episodes-lock*)
    (when (provider-operation-owner-active-p owner)
      (error 'provider-operation-refused :message "This provider work scope is already running."))
    (setf (provider-operation-owner-active-p owner) t
          (provider-operation-owner-published-p owner) nil))
  (unwind-protect (funcall function)
    (unless (provider-operation-owner-published-p owner)
      (provider-operation-owner-dispose owner))
    (with-lock-held (*provider-connection-episodes-lock*)
      (setf (provider-operation-owner-active-p owner) nil))))

(-> provider-operation-owner-publish (provider-operation-owner provider-operation-transfer function) *)
(defun provider-operation-owner-publish (owner transfer publish)
  "Publish an already-owned TRANSFER, then transfer terminal disposal responsibility.
PUBLISH is invoked now, never stored. A failed publication leaves a disposed token,
including when PUBLISH enqueued it before signalling or being interrupted."
  (let ((operation (provider-operation-transfer-operation transfer)))
    (with-lock-held (*provider-connection-episodes-lock*)
      (unless (and (eq owner (provider-operation-transfer-owner transfer))
                   (eq operation (provider-operation-owner-operation owner))
                   (eq (provider-operation-state operation) :transferred)
                   (not (provider-operation-owner-published-p owner)))
        (error 'provider-operation-refused :message "The provider transfer is not owned by this work.")))
    (multiple-value-prog1 (funcall publish operation)
      (setf (provider-operation-owner-published-p owner) t))))

(-> provider--connection-owner-generation (t) integer)
(defun provider--connection-owner-generation (owner)
  "Read OWNER's cancellation epoch under the registration lock."
  (with-lock-held (*provider-connection-episodes-lock*)
    (gethash owner *provider-connection-owner-generations* 0)))

(define-condition provider-owner-ended (provider-error)
  ()
  (:documentation "The request owner ended; this condition is never instability."))

(-> provider--caller-deadline () (or null real))
(defun provider--caller-deadline ()
  "Capture the enclosing SBCL deadline before a transport adapter can normalize it."
  (multiple-value-bind (seconds microseconds stop-seconds stop-microseconds deadline-p)
      (sb-sys:decode-timeout nil)
    (declare (ignore seconds microseconds))
    (when deadline-p (+ stop-seconds (/ stop-microseconds 1000000)))))

(define-condition provider-opening-instability (provider-transport-error)
  ((category :initarg :category :reader provider-opening-instability-category
             :documentation "Safe opening-failure category, without the raw condition."))
  (:documentation "An allowlisted raw opening failure, classified before normalization."))

(define-condition provider-opening-rejected (provider-error)
  ()
  (:documentation "A terminal DNS, TLS or socket configuration failure during opening."))

(define-condition provider-observer-failure (error)
  ((cause :initarg :cause :reader provider-observer-failure-cause
          :documentation "Original callback failure, re-signalled outside every retry scope."))
  (:documentation "Internal escape boundary for errors from request observers."))

(defclass provider-connection-event (provider-event)
  ((details :initarg :details :reader provider-connection-event-details
            :documentation "Owned recovery status, containing only structural fields and a sanitized authority."))
  (:documentation "Presentation-only recovery status, never model conversation content."))

(-> provider--safe-endpoint (t) string)
(defun provider--safe-endpoint (endpoint)
  "Reduce ENDPOINT to a validated host and numeric port before retaining or displaying it."
  (or (handler-case
          (when (and (stringp endpoint) (<= (length endpoint) 8192))
            (let* ((parsed (uri endpoint))
                   (host   (uri-host parsed))
                   (scheme (uri-scheme parsed))
                   (port   (or (uri-port parsed)
                               (cond ((equalp scheme "https")
                                      443)
                                     ((equalp scheme "http")
                                      80)))))
              (when (and (member scheme '("http" "https") :test #'equalp)
                         (stringp host) (<= 1 (length host) 253)
                         (every (lambda (character)
                                  (or (find character "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-:")
                                      nil))
                                host)
                         (typep port '(integer 1 65535)))
                (format nil "~A:~D" (if (find #\: host) (format nil "[~A]" host) host) port))))
        (error () nil))
      "endpoint unavailable"))

(-> provider-connection-status-text (list) string)
(defun provider-connection-status-text (details)
  "Format a portable owned recovery notice without printing any condition or request."
  (let ((retry-at (getf details :next-retry-at)))
    (format nil "provider ~A ~A attempt ~D: ~A~@[; next retry ~A~]"
            (getf details :episode) (getf details :endpoint)
            (getf details :attempt) (getf details :phase)
            (when retry-at
              (multiple-value-bind (seconds minutes hours) (decode-universal-time retry-at)
                (format nil "in ~Ds at ~2,'0D:~2,'0D:~2,'0D"
                        (getf details :delay) hours minutes seconds))))))

(defstruct (provider-connection-recovery
             (:constructor make-provider-connection-recovery ()))
  "Mutable state owned by one provider invocation, never a shared circuit breaker."
  (identifier (make-identifier))
  (endpoint "endpoint unavailable" :type string)
  (event-callback nil)
  (last-route-index nil)
  (attempt 0 :type integer)
  (backoff-index 0 :type integer)
  (generation-started-p nil :type boolean)
  (route-index 0 :type integer)
  (route-count 1 :type (integer 1))
  (route-threshold 3 :type (integer 1))
  (route-failures 0 :type integer)
  (deadline (provider--caller-deadline))
  (owner *provider-connection-owner*)
  (owner-generation (provider--connection-owner-generation *provider-connection-owner*))
  (owner-check *provider-connection-owner-check*)
  (lock (make-lock "Provider episode"))
  (wakeup (sb-thread:make-waitqueue :name "Provider episode wakeup"))
  (ended-p nil :type boolean))

(-> provider--check-connection-owner (provider-connection-recovery) (or null real))
(defun provider--check-connection-owner (state)
  "Reject stale ownership and return its remaining lifetime without holding locks."
  (when (and (provider-connection-recovery-owner state)
             (/= (provider-connection-recovery-owner-generation state)
                 (provider--connection-owner-generation (provider-connection-recovery-owner state))))
    (error 'provider-owner-ended :message "The provider caller cancelled this request generation."))
  (when (with-lock-held ((provider-connection-recovery-lock state))
          (provider-connection-recovery-ended-p state))
    (error 'provider-owner-ended :message "The provider request owner has ended."))
  (let* ((check              (provider-connection-recovery-owner-check state))
         (remaining          (and check (funcall check)))
         (deadline           (provider-connection-recovery-deadline state))
         (absolute-remaining (and deadline
                                  (- deadline (/ (get-internal-real-time)
                                                 internal-time-units-per-second)))))
    (when (and absolute-remaining (<= absolute-remaining 0))
      (error 'provider-owner-ended :message "The provider caller deadline elapsed."))
    (when (and remaining (<= remaining 0))
      (error 'provider-owner-ended :message "The provider owner deadline elapsed."))
    (cond ((and remaining absolute-remaining)
           (min remaining absolute-remaining))
          (remaining
           remaining)
          (t
           absolute-remaining))))

(-> provider--end-connection-episode (provider-connection-recovery) null)
(defun provider--end-connection-episode (state)
  "Permanently end this exact episode and notify its owned waiter."
  (with-lock-held ((provider-connection-recovery-lock state))
    (setf (provider-connection-recovery-ended-p state) t)
    (sb-thread:condition-broadcast (provider-connection-recovery-wakeup state)))
  nil)

(-> provider-cancel-connection-owner (t) null)
(defun provider-cancel-connection-owner (owner)
  "Wake episodes belonging to OWNER without holding any controller or mission lock."
  (unless owner (return-from provider-cancel-connection-owner nil))
  (let ((states (with-lock-held (*provider-connection-episodes-lock*)
                  (incf (gethash owner *provider-connection-owner-generations* 0))
                  (remove owner *provider-connection-episodes* :test-not #'eq
                          :key                                           #'provider-connection-recovery-owner))))
    (dolist (state states) (provider--end-connection-episode state)))
  nil)

(-> provider--call-with-connection-episode (provider-connection-recovery function) *)
(defun provider--call-with-connection-episode (state function)
  "Register one generation of ownership, and remove it on every terminal unwind."
  (let ((*provider-connection-episode* state))
    (unwind-protect
         (progn
           (provider--check-connection-owner state)
           (with-lock-held (*provider-connection-episodes-lock*)
             (push state *provider-connection-episodes*))
           (provider--check-connection-owner state)
           (funcall function))
      (provider--end-connection-episode state)
      (with-lock-held (*provider-connection-episodes-lock*)
        (setf *provider-connection-episodes* (remove state *provider-connection-episodes*))))))

(-> provider--wait-on-connection-condition (provider-connection-recovery real) t)
(defun provider--wait-on-connection-condition (state seconds)
  "Wait with STATE's mutex held on entry and on every normal return."
  (let ((lock (provider-connection-recovery-lock state)))
    ;; SBCL's timeout return does not reacquire the mutex. Restore ownership
    ;; before returning to the caller's lock scope or reading protected state.
    (unless (sb-thread:condition-wait (provider-connection-recovery-wakeup state)
                                     lock :timeout seconds)
      (sb-thread:grab-mutex lock)))
  nil)

(-> provider--connection-wait (provider-connection-recovery real) null)
(defun provider--connection-wait (state seconds)
  "Wait on request-owned synchronization, respecting caller expiry and cancellation."
  (let* ((remaining (provider--check-connection-owner state))
         (duration  (if remaining (min remaining seconds) seconds))
         (until     (+ (get-internal-real-time) (* duration internal-time-units-per-second))))
    ;; Preserve the existing injected test clock hook. Production uses its own waiter.
    (if (not (eq *bounded-retry-sleep-function* #'sleep))
        (funcall *bounded-retry-sleep-function* duration)
        (loop
          (provider--check-connection-owner state)
          (let ((left (/ (- until (get-internal-real-time)) internal-time-units-per-second)))
            (unless (plusp left) (return))
            (with-lock-held ((provider-connection-recovery-lock state))
              (unless (provider-connection-recovery-ended-p state)
                (provider--wait-on-connection-condition state left))))))
    (provider--check-connection-owner state))
  nil)

(-> provider--raw-opening-category (condition) (or null keyword))
(defun provider--raw-opening-category (condition)
  "Return only a safe category, without retaining a credential-bearing condition."
  (typecase condition
    ((or usocket:ns-try-again-error sb-bsd-sockets:try-again-error)
     :dns-temporary)
    (sb-bsd-sockets:name-service-error
     (when (eq (sb-bsd-sockets::condition-for-name-service-errno
                (sb-bsd-sockets::name-service-error-errno condition))
               'sb-bsd-sockets:try-again-error)
       :dns-temporary))
    ((or usocket:connection-refused-error usocket:connection-aborted-error
         usocket:connection-reset-error usocket:network-unreachable-error
         usocket:network-down-error usocket:network-reset-error
         usocket:host-down-error usocket:host-unreachable-error)
     :connection)
    ((or usocket:timeout-error sb-sys:deadline-timeout)
     :opening-timeout)
    (cl+ssl::ssl-error-syscall
     :tls-io)
    (t
     nil)))

(-> provider--call-opening (function) *)
(defun provider--call-opening (function)
  "Bound and classify the actual opening operation before transport normalization."
  (when *provider-connection-episode*
    (provider--check-connection-owner *provider-connection-episode*))
  (handler-bind
      ((sb-sys:deadline-timeout
         (lambda (condition)
           (declare (ignore condition))
           (when *provider-connection-episode*
             (let ((remaining (provider--check-connection-owner *provider-connection-episode*)))
               ;; SBCL can signal one internal clock unit before the decoded stop.
               (when (and remaining (<= remaining (/ 1 internal-time-units-per-second)))
                 (error 'provider-owner-ended :message "The provider caller deadline elapsed."))))
           (error 'provider-opening-instability :category ':opening-timeout
                  :message                                "The provider opening reached its independent timeout.")))
       (error
         (lambda (condition)
           (let ((category (provider--raw-opening-category condition)))
             (cond
               (category
                (error 'provider-opening-instability :category category
                       :message                                "The provider connection could not be opened."))
               ((typep condition '(or usocket:socket-error usocket:ns-error
                                      sb-bsd-sockets:name-service-error))
                (error 'provider-opening-rejected
                       :message "The provider address or connection configuration could not be resolved.")))))))
    (provider-call-with-response-deadline *provider-opening-timeout-seconds* function)))

(-> provider--connection-instability-p (condition) t)
(defun provider--connection-instability-p (condition)
  "Separate transport/service instability from semantic and credential retries."
  (and (typep condition 'provider-retryable-error)
       (or (and (typep condition 'provider-transport-error)
                (not (typep condition 'response-stream-limit-error)))
           (let ((status (provider-error-status condition)))
              (and (integerp status) (or (= status 429) (<= 500 status 599))))
           (member (provider-error-code condition)
                   '("overloaded_error" "service_unavailable_error" "rate_limit_error") :test #'equal))))

(-> provider--connection-retry-delay (integer &optional random-state) integer)
(defun provider--connection-retry-delay (index &optional (random-state *random-state*))
  "Return a jittered exponential delay with saturated arithmetic and a 300s cap."
  (let ((base (min 300 (* 5 (ash 1 (min 6 (max 0 index)))))))
    (min 300 (max 1 (round (* base (+ 0.8d0 (random 0.4d0 random-state))))))))

(-> provider--connection-status (provider-connection-recovery keyword &key (:delay (or null integer))) null)
(defun provider--connection-status (state phase &key delay)
  "Publish detached safe state through the request's ordinary observer, outside all locks."
  (let ((callback (provider-connection-recovery-event-callback state)))
    (when callback
      (funcall callback
               (make-instance 'provider-connection-event
                              :details (list :episode       (copy-seq (provider-connection-recovery-identifier state))
                                             :phase         phase
                                             :endpoint      (copy-seq (provider-connection-recovery-endpoint state))
                                             :attempt       (provider-connection-recovery-attempt state)
                                             :route-index   (if (eq phase :waiting)
                                                                (or (provider-connection-recovery-last-route-index state)
                                                                    (provider-connection-recovery-route-index state))
                                                                (provider-connection-recovery-route-index state))
                                             :delay         delay
                                             :next-retry-at (and delay (+ (get-universal-time) delay)))))))
  nil)

(-> provider--call-with-connection-status (provider-connection-recovery function) *)
(defun provider--call-with-connection-status (state function)
  "Terminate presentation on every unwind while preserving exact observer failures."
  (let ((phase :terminated) (observer-failed-p nil))
    (unwind-protect
         (handler-bind ((provider-observer-failure
                          (lambda (condition)
                            (declare (ignore condition))
                            (setf observer-failed-p t)))
                        (provider-owner-ended
                          (lambda (condition)
                            (declare (ignore condition))
                            (setf phase :cancelled))))
           (multiple-value-prog1 (funcall function)
             (setf phase :completed)))
      (unless observer-failed-p
        (provider--connection-status state phase)))))

(-> provider--call-with-connection-recovery (provider-connection-recovery function) *)
(defun provider--call-with-connection-recovery (state attempt-function)
  "Repeat the SAME imported streaming attempt wrapper for eligible opening failures.
Its cumulative generation safety checks therefore apply to every attempt. Once
model generation is observed, return failures to the existing bounded ladder."
  (loop
    (provider--check-connection-owner state)
    (incf (provider-connection-recovery-attempt state))
    (handler-case
        (return (funcall attempt-function))
      (provider-retryable-error (condition)
        (provider--check-connection-owner state)
        (unless (and (provider--connection-instability-p condition)
                     (not (provider-connection-recovery-generation-started-p state))
                     (not (cl-llm-provider-api::provider-retryable-error-generation-started-p condition)))
          (error condition))
        (incf (provider-connection-recovery-route-failures state))
        (when (>= (provider-connection-recovery-route-failures state)
                  (provider-connection-recovery-route-threshold state))
          (setf (provider-connection-recovery-route-failures state) 0
                (provider-connection-recovery-route-index state)
                (mod (1+ (provider-connection-recovery-route-index state))
                     (provider-connection-recovery-route-count state))))
        (let ((delay (provider--connection-retry-delay
                      (provider-connection-recovery-backoff-index state))))
          (setf (provider-connection-recovery-backoff-index state)
                (min 6 (1+ (provider-connection-recovery-backoff-index state))))
          (provider--connection-status state :waiting :delay delay)
          (provider--connection-wait state delay))))))


;;;; -- Fresh Admission After A Supervised Slice --

(-> provider--suspension-configuration (configuration) list)
(defun provider--suspension-configuration (configuration)
  "Capture only the explicit route declaration needed to interpret numeric route history."
  (list (copy-seq (config :provider-fallback-mappings configuration))
        (config :provider-fallback-threshold configuration)
        (copy-seq (config :model-provider configuration))
        (copy-seq (config :model configuration))))

(-> provider--remember-suspension
    (conversation provider-connection-recovery configuration &key (:deadline (or null real))) null)
(defun provider--remember-suspension (conversation state configuration &key deadline)
  "Retain metadata only for the still-live ordinary operation, never dispatch resources."
  (when (and *provider-connection-operation*
             (plusp (provider-connection-recovery-attempt state))
             (not (provider-connection-recovery-generation-started-p state)))
    (let ((snapshot
            (list :conversation  conversation
                  :owner         (provider-connection-recovery-owner state)
                  :epoch         (provider-connection-recovery-owner-generation state)
                  :deadline      (let ((earlier (provider-connection-recovery-deadline state)))
                                   (cond ((and earlier deadline)
                                          (min earlier deadline))
                                         (earlier
                                          earlier)
                                         (t
                                          deadline)))
                  :configuration (provider--suspension-configuration configuration)
                  :attempt       (provider-connection-recovery-attempt state)
                  :backoff       (provider-connection-recovery-backoff-index state)
                  :route         (provider-connection-recovery-route-index state)
                  :failures      (provider-connection-recovery-route-failures state))))
      (with-lock-held (*provider-connection-episodes-lock*)
          (when (and (= (getf snapshot :epoch)
                        (gethash (getf snapshot :owner) *provider-connection-owner-generations* 0))
                     (or (not (provider-operation-p *provider-connection-operation*))
                         (not (eq (provider-operation-state *provider-connection-operation*) :disposed))))
           (setf (gethash *provider-connection-operation* *provider-suspended-connections*) snapshot)
           (when (provider-operation-p *provider-connection-operation*)
             (setf (provider-operation-state *provider-connection-operation*) :suspended
                   (provider-operation-deadline *provider-connection-operation*) (getf snapshot :deadline))))))
    (handler-case (provider--connection-status state :suspended)
      (provider-observer-failure (condition)
        (error (provider-observer-failure-cause condition)))))
  nil)

(-> provider-operation-resumable-p
    (provider-operation conversation configuration) boolean)
(defun provider-operation-resumable-p (operation conversation configuration)
  "Inspect exact ordinary ownership, route declaration and original deadline without consuming."
  (let ((declaration (provider--suspension-configuration configuration)))
    (with-lock-held (*provider-connection-episodes-lock*)
      (let ((pending (gethash operation *provider-suspended-connections*)))
        (and pending
             (not (null (member (provider-operation-state operation) '(:suspended :transferred))))
             (eq conversation (provider-operation-conversation operation))
             (eq conversation (getf pending :conversation))
             (eq (provider-operation-goal operation) (getf pending :owner))
             (= (provider-operation-epoch operation) (getf pending :epoch)
                (gethash (getf pending :owner) *provider-connection-owner-generations* 0))
             (equal declaration (getf pending :configuration))
             (or (null (getf pending :deadline))
                 (> (getf pending :deadline) (/ (get-internal-real-time) internal-time-units-per-second)))
             t)))))

(-> provider--restore-suspension (provider-connection-recovery conversation configuration) null)
(defun provider--restore-suspension (state conversation configuration)
  "Claim numeric history once under exact ordinary ownership and the original deadline."
  (when *provider-connection-operation*
    (let ((declaration (provider--suspension-configuration configuration))
          (snapshot nil) (rejected-p nil) (expired-p nil) (refused-p nil))
      (provider--check-connection-owner state)
      (with-lock-held (*provider-connection-episodes-lock*)
        (let* ((operation *provider-connection-operation*)
               (pending   (gethash operation *provider-suspended-connections*))
               (owner     (when (provider-operation-p operation) (provider-operation-owner-scope operation))))
          (when (and pending owner (provider-operation-owner-active-p owner)
                     (eq operation (provider-operation-owner-operation owner))
                     (eq conversation (getf pending :conversation))
                     (eq (getf pending :owner) (provider-connection-recovery-owner state))
                     (= (getf pending :epoch) (provider-connection-recovery-owner-generation state)
                        (gethash (getf pending :owner) *provider-connection-owner-generations* 0))
                     (member (provider-operation-state operation) '(:suspended :transferred)))
            (cond
              ((not (equal declaration (getf pending :configuration)))
               (setf rejected-p t))
              ((and (getf pending :deadline)
                    (<= (getf pending :deadline) (/ (get-internal-real-time) internal-time-units-per-second)))
               (setf expired-p t))
              (t
               (setf snapshot pending)
               (remhash operation *provider-suspended-connections*))))
          (when (provider-operation-p operation)
            (if (or snapshot (and (null pending) (eq (provider-operation-state operation) ':new)))
                (setf (provider-operation-state operation) :active)
                (setf refused-p t)))))
      (when rejected-p
        (error 'configuration-error :message "The suspended provider route declaration changed."))
      (when expired-p
        (error 'provider-owner-ended :message "The suspended caller deadline elapsed."))
      (when refused-p
        (error 'provider-operation-refused :message "This provider operation has no eligible unclaimed suspension."))
      (when snapshot
        (let ((deadline (getf snapshot :deadline)))
          (when deadline
            (setf (provider-connection-recovery-deadline state)
                  (if (provider-connection-recovery-deadline state)
                      (min deadline (provider-connection-recovery-deadline state)) deadline))))
        (setf (provider-connection-recovery-attempt state) (getf snapshot :attempt)
              (provider-connection-recovery-backoff-index state) (getf snapshot :backoff)
              (provider-connection-recovery-route-index state) (getf snapshot :route)
              (provider-connection-recovery-route-failures state) (getf snapshot :failures))
        (provider--check-connection-owner state))))
  nil)

(-> provider-call-with-suspension
    (conversation function &key (:suspended-p function)) *)
(defun provider-call-with-suspension (conversation function &key suspended-p)
  "Observe ordinary accounting unwind; a later invocation must obtain its own admission."
  (let ((*provider-connection-capture* (list conversation nil nil))
        (deadline
          (let ((caller    (provider--caller-deadline))
                (operation (when (typep *provider-connection-operation* 'provider-operation)
                             (provider-operation-deadline *provider-connection-operation*))))
            (cond ((and caller operation)
                   (min caller operation))
                  (caller
                   caller)
                  (t
                   operation)))))
    (handler-case (funcall function)
      (error (condition)
        (when (and (funcall suspended-p condition) (second *provider-connection-capture*))
          (provider--remember-suspension
           conversation (second *provider-connection-capture*) (third *provider-connection-capture*)
           :deadline deadline))
        (error condition)))))
