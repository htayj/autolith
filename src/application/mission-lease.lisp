(in-package #:autolith)

;;;; -- Review-Required Mission Boundary --

(define-condition mission-review-required (mission-error)
  ((context :initarg :context :reader mission-review-required-context
            :documentation "Exact declined mission context.")
   (generation :initarg :generation :reader mission-review-required-generation
               :documentation "Original host context generation.")
   (attempt :initarg :attempt :reader mission-review-required-attempt
            :documentation "The single latched evaluator attempt."))
  (:documentation "A controller must checkpoint without admitting further work."))

(defmethod execution-lease-ledger-for ((context mission-context))
  "Read prepared lineage without taking a lifecycle or mission persistence lock."
  (mission-context-lease-ledger context))

(defmethod execution-lease-prepare ((context mission-context))
  "Prepare exact durable identity before any lifecycle entry predicate."
  (when (and (eq (getf (mission-context-goal context) :exhaustion-policy) ':evaluate)
             (not (getf (mission-context-goal context) :candidate-id)))
    (or (mission-context-lease-ledger context)
        (with-recursive-lock-held ((mission-context-lock context))
          (or (mission-context-lease-ledger context)
              (let ((goal (mission-context-goal context)))
                (unless (getf goal :completion-id)
                  (nconc goal (list :completion-id (make-user-identifier))))
                (setf (mission-context-lease-ledger context)
                      (make-instance 'execution-lease-ledger
                                     :context context :generation (mission-context-accounting-generation context)
                                     :mission-id (getf goal :completion-id)))
                (let ((restored (getf (getf goal :exhaustion) :review-required)))
                  (when restored
                    (setf (execution-lease-attempt (mission-context-lease-ledger context))
                          (or (getf restored :attempt) 1))))
                (mission--record context)
                (mission-context-lease-ledger context)))))))

(defmethod execution-lease-context-current-p ((context mission-context) generation)
  "Validate exact goal, context generation and current application without I/O."
  (let ((ledger      (mission-context-lease-ledger context))
        (goal        (mission-context-goal context))
        (application (mission-context-application context)))
    (and ledger
         (equal generation (execution-lease-generation ledger))
         (equal (execution-lease-mission-id ledger) (getf goal :completion-id))
         (eq goal (application-goal application))
         (with-lock-held (*mission-contexts-lock*)
           (eq context (gethash (application-conversation application) *mission-contexts*)))
         (eq (getf goal :status) ':active))))

(defmethod execution-lease-refuse ((context mission-context) attempt)
  "Distinguish the exact latched review refusal from stale/capped authority."
  (let ((ledger (mission-context-lease-ledger context)))
    (if (and ledger attempt (eql attempt (execution-lease-attempt ledger))
             (execution-lease--current-p ledger))
        (error 'mission-review-required :context context
               :generation (execution-lease-generation ledger) :attempt attempt
               :reason                           ':review-required
               :cause                            (getf (mission-context-evaluation-observation context) :cause)
               :message                          "Mission evaluator requires administrative review; no new work is admitted.")
        (mission--reject ':operation-authority "Original execution authority is closed or no longer current."))))

(-> mission-lease-check-admission (mission-context) null)
(defun mission-lease-check-admission (context)
  "Refuse before another request/order reservation, without destructive fallback."
  (let ((ledger (execution-lease-prepare context)))
    (when ledger
      (let ((closed-p nil) (attempt nil))
        (with-lock-held ((execution-lease-lock ledger))
          (setf closed-p (not (execution-lease--open-p ledger))
                attempt (execution-lease-attempt ledger)))
        (when closed-p (execution-lease-refuse context attempt)))))
  nil)

(-> mission-lease-secondary-call (mission-context keyword function) null)
(defun mission-lease-secondary-call (context category function)
  "Retain categorical publication faults without replacing the evaluator cause."
  (handler-case (funcall function)
    (error ()
      (pushnew category (getf (mission-context-evaluation-observation context) :secondary-faults))))
  nil)

(-> mission-lease-latch (mission-context (integer 1)) null)
(defun mission-lease-latch (context attempt)
  "Latch exact current refusal before attempting secondary publication."
  (let ((ledger (execution-lease-prepare context)))
    (when (and ledger (execution-lease--current-p ledger))
      (multiple-value-bind (ignored changed-p) (execution-lease-latch ledger attempt)
        (declare (ignore ignored))
        (when changed-p
          (setf (getf (getf (mission-context-goal context) :exhaustion) :review-required)
                (list :attempt attempt :generation (execution-lease-generation ledger)))
          (mission-lease-secondary-call context ':goal-record (lambda () (mission--record context)))
          (when (execution-lease--current-p ledger)
            (mission-lease-secondary-call
             context ':terminal-notice
             (lambda ()
               (mission--present-terminal-notice
                context ':review-required
                "Mission deadline or work limit requires administrative review. New work is closed. Use (mission :status) to inspect the decision and retained execution."))))))))
  nil)

(defmethod task-checkpoint-current-p ((context mission-context) generation attempt)
  "Only this exact current latched evaluator attempt can construct a checkpoint."
  (let ((ledger (mission-context-lease-ledger context)))
    (and ledger (execution-lease-context-current-p context generation)
         (eql attempt (execution-lease-attempt ledger)))))

(defmethod task-checkpoint-mission-identifier ((context mission-context))
  "Read the previously persisted goal identity, without creating authority."
  (let ((ledger (mission-context-lease-ledger context)))
    (and ledger (execution-lease-mission-id ledger))))

(-> mission-lease--receipt-current-p (execution-lease-receipt &key (:unwind-p boolean)) boolean)
(defun mission-lease--receipt-current-p (receipt &key unwind-p)
  "Validate captured host execution evidence, with one exact normal unwind exception."
  (let* ((job      (execution-lease-receipt-job receipt))
         (terminal (execution-lease-receipt-terminal receipt))
         (end      (execution-lease-receipt-end receipt)))
    (and (every #'mission-lease--receipt-current-p (execution-lease-receipt-parents receipt))
         (not (execution-lease-receipt-reason receipt))
         (not (job-cancellation-reason job))
         (or (null end) (< (get-internal-real-time) end))
         (eq (execution-lease-receipt-pool receipt) (job-pool job))
         (equal (execution-lease-receipt-owners receipt) (job-owner-identifiers job))
         (or (and (not terminal)
                  (not (execution-lease-receipt-closed-p receipt))
                  (equal (execution-lease-receipt-token receipt) (job-run-token job))
                  (eql (execution-lease-receipt-cap receipt) (job-deadline job)))
             (and unwind-p
                  (eq (execution-lease-receipt-administrative-unwind receipt) ':completed)
                  terminal (eq (getf terminal :state) ':completed)
                  (or (null end) (< (getf terminal :ended-at) end)))))))

(defmethod task-checkpoint-lineage-current-p ((context mission-context) job publication-p)
  "Require actual outer task plus its exact own live or normally unwound scope."
  (let ((ledger (mission-context-lease-ledger context)))
    (when ledger
      (with-lock-held ((execution-lease-lock ledger))
        (let* ((outer (find job (execution-lease-receipts ledger)
                            :key #'execution-lease-receipt-job :test #'eq))
               (inner (find-if
                       (lambda (receipt)
                         (and (eq (execution-lease-receipt-operation receipt) job)
                              (typep (execution-lease-receipt-job receipt) 'execution-lease-inline-job)
                              (eq (first (execution-lease-receipt-parents receipt)) outer)))
                       (execution-lease-receipts ledger))))
          (and outer inner
               (or (not publication-p)
                   (eq (execution-lease-receipt-administrative-candidate inner)
                       (task-job-administrative-checkpoint job)))
               (mission-lease--receipt-current-p outer)
               (mission-lease--receipt-current-p inner :unwind-p publication-p)
               (every #'mission-lease--receipt-current-p
                      (execution-lease-receipt-parents outer))))))))

(-> mission-lease-checkpoint (task-job conversation mission-review-required) t)
(defun mission-lease-checkpoint (job conversation condition)
  "Capture inside the protected child, before ordinary safety cleanup.
No model/tool action is used to checkpoint. Unresolved capped scopes keep ownership."
  (let* ((context (mission-review-required-context condition))
         (ledger  (mission-context-lease-ledger context))
         (scope   *execution-lease-receipt*))
    (unless
        (and ledger scope (eq context (task-job-mission-context job))
             (eq (execution-lease-receipt-operation scope) job)
             (task-checkpoint-current-p context
              (mission-review-required-generation condition)
              (mission-review-required-attempt condition)))
      (error condition))
    (let ((unresolved (execution-lease-drain ledger scope)))
      (when unresolved
        (with-lock-held ((execution-lease-lock ledger))
          (execution-lease--controller-snapshot ledger))
        (mission--reject ':unresolved-execution
                         "Original-cap settlement remains unresolved under its guardian.")))
    (let* ((accounting (mission--accounting-report context))
           (cursor
            (list :conversation-id    (conversation-identifier conversation)
                  :request-count
                  (getf (getf accounting :snapshot) :turns-used 0) :next-action
                  ':review-required :sequence
                  (conversation-next-sequence conversation) :intent
                  (bounded-string
                   (getf (task-job-item job) :task
                         "Review before continuation")
                   :limit 1000)
                  :accounting
                  (loop for key in '(:turns-used :tokens-used
                                     :requests-outstanding :tokens-reserved
                                     :unknown-usage)
                        append (list key
                                     (getf (getf accounting :snapshot) key 0)))
                  :persistence-failed nil))
           (durable-p
            (handler-case
             (progn
              (conversation-append-record conversation
                                          (list :mission-review-checkpoint
                                                :job-id
                                                (session-job-identifier job)
                                                :mission-id
                                                (execution-lease-mission-id
                                                 ledger)
                                                :generation
                                                (execution-lease-generation
                                                 ledger)
                                                :attempt
                                                (execution-lease-attempt
                                                 ledger)
                                                :cursor (copy-tree cursor)
                                                :evidence
                                                (mission-exhaustion--diagnostic
                                                 (mission-context-evaluation-observation
                                                  context)
                                                 (getf
                                                  (getf
                                                   (mission-context-goal
                                                    context)
                                                   :exhaustion)
                                                  :evaluator))))
              t)
             (error nil (setf (getf cursor :persistence-failed) t) nil)))
           (candidate
            (task-job-capture-review-checkpoint job :context context
             :generation (execution-lease-generation ledger) :attempt
             (execution-lease-attempt ledger) :mission-id
             (execution-lease-mission-id ledger) :cursor cursor
             :durable-checkpoint-p                           durable-p)))
      (unless candidate (error condition))
      (with-lock-held ((execution-lease-lock ledger))
        (setf (execution-lease-receipt-administrative-unwind scope) ':expected
              (execution-lease-receipt-administrative-candidate scope)
              candidate))
      candidate)))

(defmethod session-job-cancel :around ((job session-job) reason)
  "Close exact host admission before stop; propagate only accepted cancellation."
  (let* ((ticket (session-job-lease-ticket job))
         (ledger (and ticket (execution-lease-ticket-ledger ticket))))
    (when ledger (execution-lease-close ledger job reason))
    (sb-sys:without-interrupts
      (multiple-value-bind (accepted-p descendants) (call-next-method)
        (when (and accepted-p ledger)
          (with-lock-held ((execution-lease-lock ledger))
            (let ((source (find job (execution-lease-receipts ledger)
                                :key #'execution-lease-receipt-job :test #'eq)))
              (when source
                (setf (execution-lease-work ledger)
                      (nconc (execution-lease-work ledger)
                             (execution-lease--mark-subtree ledger source reason))))))
          (execution-lease--wake ledger))
        (values accepted-p descendants)))))

(defmethod task-call-with-review-boundary ((job task-job) conversation function)
  "Intercept only exact typed review refusal while the real child is protected."
  (handler-case (funcall function)
    (mission-review-required (condition)
      (mission-lease-checkpoint job conversation condition))))

(defmethod execution-lease-revoke-context ((context mission-context) reason)
  "Close the exact old context promptly, leaving cancellation to its guardian."
  (let ((ledger (mission-context-lease-ledger context)))
    (when ledger
      (with-lock-held ((execution-lease-lock ledger))
        (setf (execution-lease-closed-p ledger) t)
        (dolist (receipt (execution-lease-receipts ledger))
          (setf (execution-lease-work ledger)
                (nconc (execution-lease-work ledger)
                       (execution-lease--mark-subtree ledger receipt reason)))))
      (execution-lease--wake ledger)))
  nil)

(defmethod task-controller-review-report ((context mission-context) (job task-job))
  "Expose exact declined ownership as partial evidence when original caps prevent capture."
  (let ((ledger (mission-context-lease-ledger context)))
    (when (and ledger (eq context (task-job-mission-context job)) (execution-lease-attempt ledger))
      (let ((accounting (mission--accounting-report context)))
        (append (with-lock-held ((execution-lease-lock ledger))
                  (execution-lease--controller-snapshot ledger))
                (list :accounting accounting))))))

(defmethod task-controller-accounting-report ((context mission-context) (job task-job))
  "Publish retained ordinary reporting ownership without changing administrative outcome."
  (when (eq context (task-job-mission-context job))
    (mission-inference-request-report context)))
