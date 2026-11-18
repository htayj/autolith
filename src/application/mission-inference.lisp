(in-package #:autolith)

;;;; -- Concurrent Mission Inference --

(defvar *mission-accounting-controllers* (make-hash-table :test 'equal)
  "Strong original-generation owners for unresolved ordinary reporting only.")

(defvar *mission-accounting-controllers-lock* (make-lock "mission accounting controllers")
  "Leaf lock: release before taking any context, request, lifecycle or persistence lock.")

(-> mission-inference-controller-find (string) (option mission-context))
(defun mission-inference-controller-find (generation)
  "Find retained reporting data by original generation; grant no execution authority."
  (with-lock-held (*mission-accounting-controllers-lock*)
    (gethash generation *mission-accounting-controllers*)))

(defclass mission-inference-request ()
  ((context :initarg :context :reader mission-inference-request-context
            :documentation "Exact original host context.")
   (generation :initarg :generation :reader mission-inference-request-generation
               :documentation "Original generation, never reconstructed at settlement.")
   (identifier :initform (make-identifier) :reader mission-inference-request-identifier
               :documentation "Exact request identity, independent of its reservation amount.")
   (mission-id :initarg :mission-id :reader mission-inference-request-mission-id
               :documentation "Original durable goal identifier.")
   (conversation :initarg :conversation :reader mission-inference-request-conversation
                 :documentation "Original audit destination, including after session replacement.")
   (amount :initarg :amount :reader mission-inference-request-amount
           :documentation "Original reserved output allocation.")
   (allowance :initarg :allowance :reader mission-inference-request-allowance
              :documentation "Exact allowance identity validated at callback.")
   (child :initarg :child :reader mission-inference-request-child
          :documentation "Original child used for missing-usage attribution.")
   (state :initform ':pending :accessor mission-inference-request-state
          :documentation "One-time arithmetic claim and independent append outcome.")
   (reporting-owner :initform ':execution :accessor mission-inference-request-reporting-owner
                    :documentation "Explicit handoff from executing request to original-generation controller.")
   (persistence-phase :initform nil :accessor mission-inference-request-persistence-phase
                      :documentation "Categorical reporting failure, separate from applied arithmetic.")
   (append-uncertain-p :initform nil :accessor mission-inference-request-append-uncertain-p
                       :documentation "A prior append signalled; retry cannot establish physical record uniqueness.")
   (payload :initform nil :accessor mission-inference-request-payload
            :documentation "Once-applied portable accounting evidence, retained on append failure."))
  (:documentation "Host request handle captured before dispatch, with one original accounting route."))

(-> mission-inference-request-handoff (mission-inference-request) null)
(defun mission-inference-request-handoff (request)
  "Retain the exact failed reporting owner before its executing wrapper unwinds.
Caller holds the original request lock; the registry lock is a leaf lock."
  (setf (mission-inference-request-reporting-owner request) ':original-generation-controller)
  (let ((context (mission-inference-request-context request)))
    (with-lock-held (*mission-accounting-controllers-lock*)
      (setf (gethash (mission-context-accounting-generation context) *mission-accounting-controllers*) context)))
  nil)

(defclass mission-candidate-reservation ()
  ((child :initarg :child :reader mission-candidate-reservation-child
          :documentation "Exact admitted host child identity.")
   (allowance :initarg :allowance :reader mission-candidate-reservation-allowance
              :documentation "Exact independent allowance paired at admission.")
   (pin :initarg :pin :reader mission-candidate-reservation-pin
        :documentation "Exact origin ownership pin, independent of numeric allowance.")
   (amount :initarg :amount :reader mission-candidate-reservation-amount
           :documentation "Ordinary numeric output reservation.")
   (request-id :initarg :request-id :reader mission-candidate-reservation-request-id
               :documentation "Host activity identity captured before admission.")
   (ticket :initarg :ticket :reader mission-candidate-reservation-ticket
           :documentation "Exact original review ticket, not authority for another request.")
   (terminal-payload :initform nil :accessor mission-candidate-reservation-terminal-payload
                     :documentation "Exact first attempted terminal retained for same-key absent retry.")
   (audit-condition :initform nil :accessor mission-candidate-reservation-audit-condition
                    :documentation "Optional post-terminal audit failure, separate from append evidence.")
   (condition :initarg :condition :initform nil :accessor mission-candidate-reservation-condition
              :documentation "Original ambiguous settlement condition for exact reconciliation.")
   (state :initform ':pending :accessor mission-candidate-reservation-state
          :documentation "One-time callback state; ambiguous callbacks retain ownership."))
  (:documentation "Opaque in-process reservation pairing; durable identity belongs to the operation ledger."))

(-> mission-reservation-amount ((or integer mission-inference-request mission-candidate-reservation)) integer)
(defun mission-reservation-amount (reservation)
  "Return numeric provider allowance without discarding original settlement identity."
  (etypecase reservation
    (integer
     reservation)
    (mission-inference-request
     (mission-inference-request-amount reservation))
    (mission-candidate-reservation
     (mission-candidate-reservation-amount reservation))))

(-> mission--reserve-inference (mission-context &key (:allowance (option list)))
    (or mission-inference-request mission-candidate-reservation))
(defun mission--reserve-inference (context &key allowance)
  "Capture an exact original host request before dispatch; candidate pins keep their protocol."
  (let ((owner (mission-context-origin-owner context)))
    (unless owner
      (return-from mission--reserve-inference
        (with-recursive-lock-held ((mission-context-request-lock context))
          (with-recursive-lock-held ((mission-context-lock context))
            (when (mission-context-accounting-snapshot context)
              (unless (eq (mission-context-goal context)
                          (application-goal (mission-context-application context)))
                (mission--reject ':replaced "The mission was replaced."))
              (mission--reject ':operation-authority "Original accounting generation is detached."))
            (let ((request (make-instance 'mission-inference-request :context context :amount 0 :allowance allowance
                                          :generation   (mission-context-accounting-generation context)
                                          :mission-id   (getf (mission-context-goal context) :completion-id)
                                          :conversation (or (mission-context-conversation context)
                                                            (application-conversation (mission-context-application context)))
                                          :child        (when (and (typep *mission-admitted-operation* 'task-job)
                                                                   (eq context (task-job-mission-context *mission-admitted-operation*)))
                                                          *mission-admitted-operation*))))
              (when allowance
                (setf (gethash allowance (mission-context-accounting-allowances context)) allowance))
              ;; Allocate the host handle before committing a reservation. An ID
              ;; allocation failure must never strand a persisted numeric hold.
              (sb-sys:without-interrupts
                (setf (slot-value request 'amount) (mission--reserve-inference-counters context :allowance allowance))
                (setf (slot-value request 'mission-id) (getf (mission-context-goal context) :completion-id)
                      (gethash (mission-inference-request-identifier request)
                               (mission-context-accounting-requests context)) request))
              request)))))
    (with-recursive-lock-held ((mission-context-request-lock context))
      ;; Ticket validation precedes identity allocation and ownership pin admission.
      (mission--admit context)
      (candidate-origin-validate owner)
      (let* ((request-id                              (or *mission-candidate-request-id* (make-identifier)))
             (*mission-candidate-request-id*          request-id)
             (pin                                     (candidate-origin-pin owner context))
             (*mission-candidate-append-condition*    nil)
             (*mission-candidate-admission-committed* nil)
             (*mission-candidate-admission-started*   nil))
        (handler-case
            (let ((reservation (make-instance 'mission-candidate-reservation :pin pin :request-id request-id
                                              :ticket    (mission-context-candidate-ticket context)
                                              :child     (when (and (typep *mission-admitted-operation* 'task-job)
                                                                    (eq context (task-job-mission-context *mission-admitted-operation*)))
                                                           *mission-admitted-operation*)
                                              :allowance allowance
                                              :amount    (mission-candidate--admit context request-id allowance))))
              (setf (candidate-origin-pin-evidence pin) reservation
                    (mission-candidate-reservation-condition reservation)
                    (or (mission-candidate-reservation-condition reservation) *mission-candidate-append-condition*))
              reservation)
          (serious-condition (condition)
            (if (and (not *mission-candidate-admission-committed*)
                     (or (not *mission-candidate-admission-started*)
                         (and (typep condition 'candidate-ledger-consistency-error)
                              (eq :absent (candidate-ledger-error-outcome condition)))))
                (candidate-origin-unpin pin)
                (setf (candidate-origin-uncertain-p owner) t
                      (candidate-origin-pin-evidence pin) (list :admission-uncertain request-id condition)))
            (error condition)))))))

(-> mission--settle-inference
    (mission-context (or mission-inference-request mission-candidate-reservation) (option integer)
     &key (:allowance (option list)) (:failure t) (:originating-child (option task-job))) null)
(defun mission--settle-inference (context reservation tokens &key allowance failure originating-child)
  "Settle only the exact originating candidate handle and retain ambiguous callback pins."
  (unless (mission-context-origin-owner context)
    (return-from mission--settle-inference
      (mission--settle-inference-counters context reservation tokens :allowance allowance
                                         :failure failure :originating-child originating-child)))
  (let ((pin (and (typep reservation 'mission-candidate-reservation)
                  (mission-candidate-reservation-pin reservation))))
    (with-recursive-lock-held ((mission-context-request-lock context))
      (unless (and pin
                   (eq context (candidate-origin-pin-context pin))
                   (eq (mission-context-origin-owner context) (candidate-origin-pin-owner pin))
                   (eq reservation (candidate-origin-pin-evidence pin))
                   (eq allowance (mission-candidate-reservation-allowance reservation))
                   (or (null originating-child)
                       (eq originating-child (mission-candidate-reservation-child reservation)))
                   (eq ':pending (mission-candidate-reservation-state reservation))
                   (gethash pin (candidate-origin-pending (candidate-origin-pin-owner pin))))
        (mission--reject ':candidate-reservation "Candidate settlement requires its exact pending host reservation."))
      (setf (mission-candidate-reservation-state reservation) ':settling))
    (let ((*mission-candidate-append-condition* nil))
     (handler-case
         (progn
           (with-recursive-lock-held ((mission-context-request-lock context))
             (let ((payload (mission-candidate--terminal-payload context (mission-reservation-amount reservation)
                                                                tokens :child originating-child)))
               (when (and (mission-candidate-reservation-terminal-payload reservation)
                          (not (equal payload (mission-candidate-reservation-terminal-payload reservation))))
                 (mission--reject :candidate-terminal-retry "Pending terminal retry must retain its original payload."))
               (setf (mission-candidate-reservation-terminal-payload reservation) payload)
               (mission-candidate--terminal context (mission-candidate-reservation-request-id reservation) payload))
             (setf (mission-candidate-reservation-state reservation) :settled
                   (mission-candidate-reservation-condition reservation)
                   (or (mission-candidate-reservation-condition reservation) *mission-candidate-append-condition*))
             (let* ((*mission-candidate-policy-audit* (list nil))
                    (child                            originating-child)
                    (bounded-p                        (and child (null tokens) (plusp (mission-reservation-amount reservation)))))
               (with-recursive-lock-held ((mission-context-lock context))
                 (when allowance
                   (decf (getf allowance :requests-outstanding))
                   (decf (getf allowance :tokens-reserved) (mission-reservation-amount reservation))
                   (cond (tokens
                          (incf (getf allowance :tokens-used) tokens))
                         (bounded-p
                          (incf (getf allowance :tokens-used) (mission-reservation-amount reservation))
                          (incf (getf allowance :reservation-charged-tokens) (mission-reservation-amount reservation)))))
                 (handler-case
                     (mission--settlement-policy context (mission-reservation-amount reservation) tokens
                                                :bounded-p bounded-p :child child
                                                :failure failure :attributed-p t)
                   (serious-condition (condition)
                     (setf (first *mission-candidate-policy-audit*) condition)))
                 (when (first *mission-candidate-policy-audit*)
                   (setf (mission-candidate-reservation-audit-condition reservation)
                         (first *mission-candidate-policy-audit*)
                         (mission-candidate-reservation-condition reservation)
                         (or (mission-candidate-reservation-condition reservation)
                             *mission-candidate-append-condition* (first *mission-candidate-policy-audit*)))))))
           (with-recursive-lock-held ((mission-context-request-lock context))
             (candidate-origin-unpin pin))
           (mission--wake-settlement context)
           ;; Kernel ownership retirement never runs under request or state locks.
           (candidate-origin-retire (candidate-origin-pin-owner pin)))
       (serious-condition (condition)
         (with-recursive-lock-held ((mission-context-request-lock context))
           (unless (eq :settled (mission-candidate-reservation-state reservation))
              (setf (mission-candidate-reservation-condition reservation)
                    (if (typep condition 'candidate-ledger-consistency-error)
                        (or (candidate-ledger-error-original-condition condition) condition) condition)))
           (if (eq :settled (mission-candidate-reservation-state reservation))
               (setf (mission-candidate-reservation-audit-condition reservation) condition
                     (mission-candidate-reservation-condition reservation)
                     (or (mission-candidate-reservation-condition reservation) condition))
               (if (and (typep condition 'candidate-ledger-consistency-error)
                    (eq :absent (candidate-ledger-error-outcome condition)))
                (setf (mission-candidate-reservation-state reservation) :pending)
                (setf (mission-candidate-reservation-state reservation) :uncertain
                      (candidate-origin-uncertain-p (candidate-origin-pin-owner pin)) t))))
         (error condition)))))
  nil)

(-> mission--available-tokens (list) integer)
(defun mission--available-tokens (allowance)
  "Return the unspent token allowance after outstanding output reservations."
  (- (getf allowance :token-limit)
     (getf allowance :tokens-used)
     (getf allowance :tokens-reserved 0)))

(-> mission--output-reservation (list) (integer 1))
(defun mission--output-reservation (allowance)
  "Divide available output tokens among remaining turns, honoring the caller's cap."
  (let ((available (mission--available-tokens allowance)))
    (min available
         (or *provider-maximum-output-tokens*
             (max 1 (floor available
                           (- (getf allowance :turn-limit)
                              (getf allowance :turns-used))))))))

(-> mission--reserve-inference-counters (mission-context &key (:allowance (option list))) (integer 1))
(defun mission--reserve-inference-counters (context &key allowance)
  "Reserve one turn and its output allocation atomically before provider execution."
  (with-recursive-lock-held ((mission-context-request-lock context))
    (with-recursive-lock-held ((mission-context-lock context))
      (mission--ensure-reservations (mission-context-goal context))
      (when allowance (mission--ensure-reservations allowance))
      (mission--admit context)
      (let* ((goal   (mission-context-goal context))
             (limits (remove nil (list goal allowance))))
        (when (and allowance
                   (or (>= (getf allowance :turns-used) (getf allowance :turn-limit))
                       (>= (getf allowance :tokens-used) (getf allowance :token-limit))))
          (mission--reject ':review-budget "Independent reviewer allowance exhausted."))
        (unless (every (lambda (limit) (plusp (mission--available-tokens limit))) limits)
          (mission--reject ':budget-reserved "Outstanding requests hold the remaining output allowance."))
        (let ((reservation (reduce #'min limits :key #'mission--output-reservation)))
          (dolist (limit limits)
            (incf (getf limit :turns-used))
            (incf (getf limit :requests-outstanding 0))
            (incf (getf limit :tokens-reserved 0) reservation))
          (handler-case
              (progn
                                (mission--record context) reservation)
            (error (condition)
              ;; No provider request was dispatched if admission cannot persist.
              (dolist (limit limits)
                (decf (getf limit :turns-used))
                (decf (getf limit :requests-outstanding))
                (decf (getf limit :tokens-reserved) reservation))
              (error condition))))))))

(-> mission--settlement-policy
    (mission-context integer (option integer) &key (:bounded-p boolean) (:child (option task-job))
     (:failure t) (:attributed-p boolean)) null)
(defun mission--settlement-policy (context reservation tokens &key bounded-p child failure attributed-p)
  "Apply ordinary terminal policy after numeric accounting is published."
  (let ((goal (mission-context-goal context)))
       (when failure
         (mission--evidence context :inference-failure (princ-to-string failure)))
       (unless (integerp tokens)
         (unless (or bounded-p attributed-p) (incf (getf goal :unknown-usage)))
         (cond
           (child
            (unless attributed-p
              (mission--attribute-child-unknown context child
                                                :reservation (when bounded-p reservation))))
           ((mission--unknown-usage-blocks-p goal)
            (when (or (eq (getf goal :status) ':active)
                      (and (getf goal :candidate-id) (eq (getf goal :status) ':draft)))
              (mission--transition
               context ':blocked
               "An inference request settled without reported usage; started children may finish, new work needs a new budget."
               :cancel-p nil)))
           (t
            (mission--evidence context ':unknown-usage
                               "An inference request settled without reported usage; recorded by mission policy."))))
       (when (and (or (member (getf goal :status) '(:active :blocked))
                      (and (getf goal :candidate-id) (eq (getf goal :status) ':draft)))
                  (> (getf goal :tokens-used) (getf goal :token-limit))
                  (not (eq (mission-exhaustion--policy context) :evaluate)))
         (mission--transition context ':exhausted "Mission token budget exhausted."))
       (let ((pending (getf (getf goal :exhaustion) :pending)))
         (when (and pending (eq (getf pending :phase) :settling)
                    (zerop (getf goal :requests-outstanding))
                    (not (getf pending :ready-at)))
           (setf (getf pending :ready-at) (get-universal-time))))
       (mission--record context))

  nil)

(-> mission--wake-settlement (mission-context) null)
(defun mission--wake-settlement (context)
  "Wake ordinary completion and input waiters outside mission locks."
  ;; A ready completion may be waiting only for this reservation to settle.
  ;; Wake outside the mission locks: controller admission acquires them in reverse.
  (let* ((agent   (application-agent (mission-context-application context)))
         (service (and agent (task-completion--service agent))))
    (when service (task-completion--wake service)))
  (let ((controller (application-input-controller (mission-context-application context))))
    (when controller
      (with-lock-held ((application-input-controller-lock controller))
        (sb-thread:condition-broadcast
         (application-input-controller-condition-variable controller)))))
  nil)

(-> mission-inference-request-report (mission-context) list)
(defun mission-inference-request-report (context)
  "Read bounded original-generation reporting ownership without granting execution authority."
  (with-recursive-lock-held ((mission-context-request-lock context))
    (let ((requests (loop for request being the hash-values of (mission-context-accounting-requests context)
                          when (eq (mission-inference-request-reporting-owner request) ':original-generation-controller)
                            collect request)))
      (when requests
        (list :owner      ':original-generation-controller
              :generation (mission-context-accounting-generation context)
              :unresolved-count (length requests) :truncated-p (> (length requests) 64)
              :requests
              (loop for request in requests repeat 64
                    collect (list :request-id         (mission-inference-request-identifier request)
                                  :state              (mission-inference-request-state request)
                                  :phase              (mission-inference-request-persistence-phase request)
                                  :append-uncertain-p (mission-inference-request-append-uncertain-p request)
                                  :classification     (getf (rest (mission-inference-request-payload request)) :classification)
                                  :known-tokens       (getf (rest (mission-inference-request-payload request)) :known-tokens)
                                  :reservation-charge (getf (rest (mission-inference-request-payload request)) :reservation-charge))))))))

(-> mission-inference-request-persist (mission-inference-request) null)
(defun mission-inference-request-persist (request)
  "Append only the first applied payload; never repeat arithmetic or terminal policy.

An append error is uncertain even when the writer signalled after committing.
Original-generation controller ownership survives execution/guardian retirement."
  (let ((context (mission-inference-request-context request)))
    (with-recursive-lock-held ((mission-context-request-lock context))
      (when (and (mission-inference-request-payload request)
                 (not (eq (mission-inference-request-state request) ':persisted)))
        (setf (mission-inference-request-state request) ':unpersisted
              (mission-inference-request-persistence-phase request) ':audit-append)
        (handler-case
            (conversation-append-record (mission-inference-request-conversation request)
                                        (mission-inference-request-payload request))
          (serious-condition (condition)
            (setf (mission-inference-request-append-uncertain-p request) t)
            (mission-inference-request-handoff request)
            (error condition)))
        (setf (mission-inference-request-state request) ':persisted
              (mission-inference-request-reporting-owner request) nil
              (mission-inference-request-persistence-phase request) nil)
        (remhash (mission-inference-request-identifier request) (mission-context-accounting-requests context))
        (unless (loop for retained being the hash-values of (mission-context-accounting-requests context)
                      thereis (eq (mission-inference-request-reporting-owner retained) ':original-generation-controller))
          (with-lock-held (*mission-accounting-controllers-lock*)
            (remhash (mission-context-accounting-generation context) *mission-accounting-controllers*))))))
  nil)

(-> mission-inference-request-retry (mission-context string) (values boolean boolean))
(defun mission-inference-request-retry (context identifier)
  "Explicitly retry the controller-owned payload, returning completion and prior append uncertainty.
No automatic retry, arithmetic, policy replay or execution dispatch occurs."
  (with-recursive-lock-held ((mission-context-request-lock context))
    (let ((request (gethash identifier (mission-context-accounting-requests context))))
      (unless (and request (mission-inference-request-payload request))
        (mission--reject ':accounting-identity "No retained applied request matches this original generation."))
      (mission-inference-request-persist request)
      (values t (mission-inference-request-append-uncertain-p request)))))

(-> mission--settle-inference-counters
    (mission-context mission-inference-request (option integer) &key (:allowance (option list)) (:failure t)
     (:originating-child (option task-job))) null)
(defun mission--settle-inference-counters (context request tokens &key allowance failure originating-child)
  "Claim an exact original request once; stale callbacks cannot publish successor policy."
  (unless (or (null tokens) (and (integerp tokens) (not (minusp tokens))))
    (mission--reject ':accounting-usage "Known billable usage must be a nonnegative integer."))
  (with-recursive-lock-held ((mission-context-request-lock context))
    (unless (and (typep request 'mission-inference-request)
                 (eq context (mission-inference-request-context request))
                 (equal (mission-context-accounting-generation context)
                        (mission-inference-request-generation request))
                 (eq allowance (mission-inference-request-allowance request))
                 (or (null originating-child)
                     (eq originating-child (mission-inference-request-child request))))
      (mission--reject ':accounting-identity "Settlement does not match its original host request."))
    (unless (eq (mission-inference-request-state request) ':pending)
      (return-from mission--settle-inference-counters nil))
    (with-recursive-lock-held ((mission-context-lock context))
      (let* ((goal             (mission--accounting-goal context))
             (reservation      (mission-inference-request-amount request))
             (child            (mission-inference-request-child request))
             (bounded-p        (and child (not (integerp tokens))))
             (route            (and allowance (gethash allowance (mission-context-accounting-allowances context))))
             (policy-condition nil))
        (setf (mission-inference-request-state request) ':applied)
        (dolist (limit (remove nil (list goal route)))
          (decf (getf limit :requests-outstanding))
          (decf (getf limit :tokens-reserved) reservation)
          (cond
            ((integerp tokens)
             (incf (getf limit :tokens-used) tokens))
            (bounded-p
             (incf (getf limit :tokens-used) reservation)
             (incf (getf limit :reservation-charged-tokens) reservation))))
        (unless (integerp tokens)
          (unless bounded-p (incf (getf goal :unknown-usage)))
          (when child
            (mission--attribute-child-unknown context child :goal goal
                                              :reservation        (when bounded-p reservation))))
        (when (mission--registered-p context)
          (handler-case
              (mission--settlement-policy context reservation tokens :bounded-p (not (null bounded-p))
                                          :child child :failure failure :attributed-p t)
            (serious-condition (condition) (setf policy-condition condition))))
        (setf (mission-inference-request-payload request)
              (append
               (list :mission-accounting-settlement
                     :mission-id (mission-inference-request-mission-id request)
                     :generation         (mission-inference-request-generation request)
                     :request-id         (mission-inference-request-identifier request)
                     :child-id           (when child (job-identifier child))
                     :classification     (if (integerp tokens) ':known ':unknown)
                     :known-tokens tokens :reservation reservation
                     :reservation-charge (if bounded-p reservation 0)
                     :failure-p          (not (null failure))
                     :snapshot           (mission--accounting-values goal)
                     :opening            (copy-tree (mission-context-accounting-opening context)))
               (when route
                 (list :allowance
                       (list :id          (copy-tree (getf route :id))
                             :turn-limit  (getf route :turn-limit)
                             :token-limit (getf route :token-limit)
                             :snapshot    (mission--accounting-values route))))))
        (when policy-condition
          (setf (mission-inference-request-state request) ':unpersisted
                (mission-inference-request-persistence-phase request) ':settlement-policy)
          (mission-inference-request-handoff request)
          (error policy-condition))))
    (mission-inference-request-persist request))
  (mission--wake-settlement context)
  nil)

(-> mission--account-inference
    (mission-context function function &key (:allowance (option list))) t)
(defun mission--account-inference (context function usage-function &key allowance)
  "Reserve allowances, execute without mission locks, then settle exactly once.

ALLOWANCE optionally charges the same request to an independent reviewer policy.
Provider-reported billable usage includes input tokens. Output allocations do not
bound total provider billing or retry exposure. Preserve all provider return values.
A request refused before invoking FUNCTION or at its first local context-admission
boundary has known zero usage. After entry into an admitted provider request, an
exact child's missing usage charges its finite admitted allocation once and retains
child UNKNOWN audit; unproven uncertainty requires conservative quarantine."
  (execution-lease-accounting-call (lambda ()
                                    (let* ((*mission-candidate-request-id* (make-identifier))
                                           (originating-child
                                            (and (typep *mission-admitted-operation* 'task-job)
                                                 (eq (task-job-mission-context *mission-admitted-operation*) context)
                                                 *mission-admitted-operation*))
                                           (reservation                    nil)
                                           (dispatched-p                   nil)
                                           (admission-state                (list :pending))
                                           (local-refusal-p                nil)
                                           (settled-p                      nil)
                                           (failure                        nil))
                                      (unwind-protect
                                           (progn
                                             (mission-exhaustion--admitting
                                              context (lambda ()
                                                        (sb-sys:without-interrupts
                                                          (setf reservation (mission--reserve-inference context :allowance allowance)))))
                                             (handler-case
                                                  (let* ((*provider-mission-output-allowance* (mission-reservation-amount reservation))
                                                         (results                             (mission--supervise
                                                                                               context "Mission inference"
                                                                                               (lambda ()
                                                                                                 (when (and (typep *mission-candidate-ticket* 'mission-draft-review-claim)
                                                                                                            (not (mission-draft-review-current-p *mission-candidate-ticket*)))
                                                                                                   (mission--reject :review-stale "Reviewer changed before provider entry."))
                                                                                                 (setf dispatched-p t)
                                                                                                 (let ((*provider-context-admission-state* admission-state))
                                                                                                   ;; Capture origin before supervision serializes the condition.
                                                                                                   (handler-bind
                                                                                                       ((error
                                                                                                          (lambda (condition)
                                                                                                            (setf local-refusal-p
                                                                                                                  (eq condition (rest admission-state))))))
                                                                                                     (multiple-value-list (funcall function)))))))
                                                         (tokens                              (rlm-usage-billable-tokens
                                                                                               (provider-usage-normalize (funcall usage-function results)))))
                                                   (sb-sys:without-interrupts
                                                     (setf settled-p t)
                                                     (mission--settle-inference context reservation tokens
                                                                                :allowance allowance :originating-child originating-child))
                                                   (when (and allowance
                                                              (or (null tokens)
                                                                  (> (getf allowance :tokens-used) (getf allowance :token-limit))))
                                                     (mission--reject ':review-budget "Reviewer token allowance exhausted or usage unknown."))
                                                   (values-list results))
                                               (error (condition)
                                                 (setf failure condition)
                                                 (let ((refusal (rest admission-state)))
                                                   ;; Preserve the supervisor failure for settlement and audit.
                                                   (error (if (and local-refusal-p
                                                                   (eq (first admission-state) :pending)
                                                                   (typep refusal 'provider-context-budget-error)
                                                                   (typep condition 'mission-error)
                                                                   (eq (mission-error-reason condition) :failed))
                                                              refusal
                                                              condition))))))
                                        (when (and reservation (not settled-p))
                                          (sb-sys:without-interrupts
                                            (setf settled-p t)
                                            (mission--settle-inference
                                             context reservation
                                             (if (or (not dispatched-p)
                                                     (and local-refusal-p (eq (first admission-state) :pending)))
                                                 0
                                                 nil)
                                             :allowance         allowance
                                             :originating-child originating-child
                                             :failure           (or failure "Inference interrupted before usage settlement.")))))))))
