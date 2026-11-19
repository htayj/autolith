(in-package #:autolith)

;;;; -- Concurrent Mission Inference --

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

(-> mission-reservation-amount ((or integer mission-candidate-reservation)) integer)
(defun mission-reservation-amount (reservation)
  "Return numeric provider allowance without discarding candidate settlement identity."
  (etypecase reservation
    (integer
     reservation)
    (mission-candidate-reservation
     (mission-candidate-reservation-amount reservation))))

(-> mission--reserve-inference (mission-context &key (:allowance (option list)))
    (or (integer 1) mission-candidate-reservation))
(defun mission--reserve-inference (context &key allowance)
  "Pair candidate reservations with an exact ownership pin; working reservations are integers."
  (let ((owner (mission-context-origin-owner context)))
    (unless owner
      (return-from mission--reserve-inference (mission--reserve-inference-counters context :allowance allowance)))
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
    (mission-context (or integer mission-candidate-reservation) (option integer)
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

(-> mission--settle-inference-counters
    (mission-context integer (option integer) &key (:allowance (option list)) (:failure t)
     (:originating-child (option task-job))) null)
(defun mission--settle-inference-counters (context reservation tokens &key allowance failure originating-child)
  "Release RESERVATION once, charging measured usage or an exact child's allowance."
  (with-recursive-lock-held ((mission-context-request-lock context))
    (with-recursive-lock-held ((mission-context-lock context))
      (let* ((goal      (mission-context-goal context))
             (child     (and originating-child
                             (eq (task-job-mission-context originating-child) context)
                             originating-child))
             (bounded-p (and child (not (integerp tokens))
                             (typep reservation '(integer 1)))))
        (dolist (limit (remove nil (list goal allowance)))
          (decf (getf limit :requests-outstanding))
          (decf (getf limit :tokens-reserved) reservation)
          (cond
            ((integerp tokens)
             (incf (getf limit :tokens-used) tokens))
            (bounded-p
             (mission--ensure-reservations limit)
             (incf (getf limit :tokens-used) reservation)
             (incf (getf limit :reservation-charged-tokens) reservation))))
        (mission--settlement-policy context reservation tokens :bounded-p bounded-p :child child
                                    :failure failure))))
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
           :failure           (or failure "Inference interrupted before usage settlement.")))))))
