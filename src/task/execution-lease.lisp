(in-package #:autolith)

(defparameter *execution-lease-accounting-receipts* nil
  "One request's exact retained scopes, released only after settlement cleanup.")

(defparameter *execution-lease-receipt-limit* 4096
  "Hard host bound for retained entered scopes, including unresolved settlement ancestry.")

;;;; -- Exact Execution Leases --

(defclass execution-lease-ledger ()
  ((controller-report :initform nil :accessor execution-lease-controller-report
                      :documentation "Bounded partial evidence, never permission or termination proof.")
   (context :initarg :context :reader execution-lease-context
            :documentation "Exact originating host context.")
   (generation :initarg :generation :reader execution-lease-generation
               :documentation "Nontransferable context generation.")
   (mission-id :initarg :mission-id :reader execution-lease-mission-id
               :documentation "Durable goal identity.")
   (lock :initform (make-lock "Execution leases") :reader execution-lease-lock
         :documentation "Bounded admission and revocation gate.")
   (changed :initform (make-condition-variable) :reader execution-lease-changed
            :documentation "Registration, closure and cancellation notification.")
   (waiters :initform nil :accessor execution-lease-waiters
            :documentation "Separate finite-drain condition variables.")
   (work :initform nil :accessor execution-lease-work
         :documentation "Cancellation work independently owned by the guardian.")
   (attempt :initform nil :accessor execution-lease-attempt
            :documentation "One latched evaluator attempt, absent before refusal.")
   (closed-p :initform nil :accessor execution-lease-closed-p
             :documentation "Admission permanently revoked for this context.")
   (receipts :initform nil :accessor execution-lease-receipts
             :documentation "Entered scopes and their retained original ancestors.")
   (guardian :initform nil :accessor execution-lease-guardian
             :documentation "Independent original-cap controller, or a starting marker.")
   (listeners :initform nil :accessor execution-lease-listeners
              :documentation "Exact pool/listener pairs installed outside the host gate."))
  (:documentation "Host-only finite lineage, original caps and revocation ownership."))

(defstruct execution-lease-ticket
  "A reservation is not an entered lease. JOB is associated before body admission."
  ledger parents operation job (state ':reserved))

(defstruct execution-lease-receipt
  "Immutable execution evidence with separately mutable closure bookkeeping."
  ticket job pool execution owners token started cap end parents operation
  (closed-p nil) (reason nil) (revocation-source nil) (terminal nil) (body-returned-p nil)
  (administrative-unwind nil) (administrative-candidate nil) (settlements 0))

(defgeneric execution-lease-ledger-for (context)
  (:documentation "Return prepared host lineage state without allocation or I/O."))

(defmethod execution-lease-ledger-for (context)
  "Nonmission contexts have no execution lease protocol."
  (declare (ignore context))
  nil)

(defgeneric execution-lease-prepare (context)
  (:documentation "Prepare durable mission identity outside lifecycle/host admission locks."))

(defmethod execution-lease-prepare (context)
  "Ordinary work requires no mission ledger."
  (declare (ignore context))
  nil)

(defgeneric execution-lease-context-current-p (context generation)
  (:documentation "Check exact generation/currentness without I/O or lifecycle reentry."))

(defmethod execution-lease-context-current-p (context generation)
  "Unknown contexts cannot grant execution authority."
  (declare (ignore context generation))
  nil)

(defgeneric execution-lease-refuse (context attempt)
  (:documentation "Signal the typed refusal outside bounded admission predicates."))

(defgeneric task-checkpoint-lineage-current-p (context job publication-p)
  (:documentation "Validate host checkpoint lineage without acquiring job admission."))

(defmethod task-checkpoint-lineage-current-p (context job publication-p)
  "Nonmission checkpoint producers retain their own explicit authority protocol."
  (declare (ignore context job publication-p))
  t)

(defclass execution-lease-inline-job (job)
  ((ticket :initarg :lease-ticket :reader execution-lease-inline-ticket
           :documentation "Exact host reservation for this private inline scope."))
  (:documentation "Host-owned private mission scope with explicit parent edges."))

(defclass execution-lease-cooperative-inline-job (execution-lease-inline-job) ()
  (:documentation "An inline wrapper whose owning executor reaps cooperatively."))

(defmethod job-interrupt-on-cancellation-p ((job execution-lease-cooperative-inline-job))
  "Never interrupt around the cooperative executor's cleanup boundary."
  nil)

(-> execution-lease--wake (execution-lease-ledger) null)
(defun execution-lease--wake (ledger)
  "Wake the independent controller after bounded host state changes."
  (with-lock-held ((execution-lease-lock ledger))
    (condition-notify (execution-lease-changed ledger))
    (dolist (waiter (execution-lease-waiters ledger))
      (condition-notify waiter)))
  nil)

(-> execution-lease--current-p (execution-lease-ledger) boolean)
(defun execution-lease--current-p (ledger)
  "Check only host identity, without job operations or persistence."
  (execution-lease-context-current-p (execution-lease-context ledger)
                                     (execution-lease-generation ledger)))

(-> execution-lease--open-p (execution-lease-ledger) boolean)
(defun execution-lease--open-p (ledger)
  "Read the admission latch under its caller's host gate."
  (and (not (execution-lease-closed-p ledger))
       (not (execution-lease-attempt ledger))
       (execution-lease--current-p ledger)))

(-> execution-lease-reserve (t function &key (:count (integer 1))) (values list list))
(defun execution-lease-reserve (context reserve-function &key (count 1))
  "Serialize bounded order reservation against the latch; submit jobs afterward.
RESERVE-FUNCTION must only reserve in-memory order numbers, without job operations."
  (let ((ledger (execution-lease-prepare context))
        (orders nil) (tickets nil) (refused nil))
    (when (and ledger *mission-admitted-operation*
               (null *execution-lease-receipt*))
      (execution-lease-refuse context (execution-lease-attempt ledger)))
    (if (null ledger)
        (setf orders (funcall reserve-function))
        (with-lock-held ((execution-lease-lock ledger))
          (if (execution-lease--open-p ledger)
              (setf orders (funcall reserve-function)
                    tickets (loop repeat count
                                  collect (make-execution-lease-ticket
                                           :ledger  ledger
                                           :parents (when *execution-lease-receipt*
                                                      (list *execution-lease-receipt*)))))
              (setf refused t))))
    (when refused
      (execution-lease-refuse context (execution-lease-attempt ledger)))
    (values orders tickets)))

(-> execution-lease-ticket-create (t &key (:operation t)) (option execution-lease-ticket))
(defun execution-lease-ticket-create (context &key operation)
  "Reserve a private scope without granting permission to enter its body."
  (multiple-value-bind (orders tickets)
      (execution-lease-reserve context (lambda () nil))
    (declare (ignore orders))
    (let ((ticket (first tickets)))
      (when ticket (setf (execution-lease-ticket-operation ticket) operation))
      ticket)))

(-> execution-lease-associate ((option execution-lease-ticket) job) null)
(defun execution-lease-associate (ticket job)
  "Associate the actual allocated object before its queued body can be entered."
  (when ticket
    (with-lock-held ((execution-lease-lock (execution-lease-ticket-ledger ticket)))
      (unless (execution-lease-ticket-job ticket)
        (setf (execution-lease-ticket-job ticket) job))))
  nil)

(defmethod initialize-instance :after ((job execution-lease-inline-job) &key)
  "Bind an actual private inline allocation to its exact reservation."
  (execution-lease-associate (execution-lease-inline-ticket job) job))

(-> execution-lease--descendant-p (execution-lease-receipt execution-lease-receipt) boolean)
(defun execution-lease--descendant-p (receipt ancestor)
  "Follow captured host edges, never friendly identifiers or pool lookups."
  (or (eq receipt ancestor)
      (some (lambda (parent) (execution-lease--descendant-p parent ancestor))
            (execution-lease-receipt-parents receipt))))

(-> execution-lease--mark-subtree (execution-lease-ledger execution-lease-receipt keyword) list)
(defun execution-lease--mark-subtree (ledger source reason)
  "Close exact dependents once under the host gate, returning cancellation work."
  (let ((operation (execution-lease-receipt-operation source)) (work nil))
    (dolist (receipt (execution-lease-receipts ledger))
      (when (and (or (execution-lease--descendant-p receipt source)
                     (and operation
                          (not (eq (getf (execution-lease-receipt-terminal source) :state) ':completed))
                          (or (eq (execution-lease-receipt-job receipt) operation)
                              (some (lambda (ancestor)
                                      (and (eq (execution-lease-receipt-job ancestor) operation)
                                           (execution-lease--descendant-p receipt ancestor)))
                                    (execution-lease-receipts ledger)))))
                 (not (execution-lease-receipt-reason receipt)))
        (setf (execution-lease-receipt-closed-p receipt) t
              (execution-lease-receipt-reason receipt) reason
              (execution-lease-receipt-revocation-source receipt) source)
        (unless (execution-lease-receipt-terminal receipt)
          (push (cons (execution-lease-receipt-job receipt) reason) work))))
    work))

(-> execution-lease--observe (execution-lease-ledger job) null)
(defun execution-lease--observe (ledger job)
  "Read terminal evidence outside the host gate, then wake bounded bookkeeping."
  (let ((snapshot (job-snapshot job)))
    (when (member (getf snapshot :state) '(:completed :failed :aborted))
      (with-lock-held ((execution-lease-lock ledger))
        (let ((receipt (find job (execution-lease-receipts ledger)
                             :key #'execution-lease-receipt-job :test #'eq)))
          (when receipt
            (setf (execution-lease-receipt-terminal receipt)
                  (list :state               (getf snapshot :state)
                        :ended-at            (cl-jobpond:job-ended-at job)
                        :cancellation-reason (getf snapshot :cancellation-reason))
                  (execution-lease-receipt-closed-p receipt) t)
            (when (and (eq (execution-lease-receipt-administrative-unwind receipt) ':expected)
                       (eq (getf snapshot :state) ':completed)
                       (not (getf snapshot :cancellation-reason))
                       (or (null (execution-lease-receipt-end receipt))
                           (< (cl-jobpond:job-ended-at job) (execution-lease-receipt-end receipt))))
              (setf (execution-lease-receipt-administrative-unwind receipt) ':completed)))))
      (execution-lease--wake ledger)))
  nil)

(-> execution-lease--listen (execution-lease-ledger job) null)
(defun execution-lease--listen (ledger job)
  "Install exact pool closure notifications without holding the host gate."
  (let* ((pool      (job-pool job))
         (listener  (lambda (channel event)
                      (when (eq channel ':job-lifecycle)
                        (execution-lease--observe ledger (getf event :job)))))
         (install-p nil))
    (with-lock-held ((execution-lease-lock ledger))
      (unless (assoc pool (execution-lease-listeners ledger) :test #'eq)
        (push (cons pool listener) (execution-lease-listeners ledger))
        (setf install-p t)))
    (when install-p (job-pool-add-listener pool listener)))
  nil)

(-> execution-lease--cancellation-order (execution-lease-ledger list) list)
(defun execution-lease--cancellation-order (ledger work)
  "Order the finite cancellation worklist from leaves to their controllers."
  (let ((depths (make-hash-table :test #'eq))
        (jobs   (make-hash-table :test #'eq)))
    (labels ((depth (receipt)
               (or (gethash receipt depths)
                   (setf (gethash receipt depths)
                         (1+ (reduce #'max (execution-lease-receipt-parents receipt)
                                     :key #'depth :initial-value 0))))))
      (with-lock-held ((execution-lease-lock ledger))
        (dolist (receipt (execution-lease-receipts ledger))
          (setf (gethash (execution-lease-receipt-job receipt) jobs) (depth receipt)))))
    (stable-sort work #'> :key (lambda (entry) (gethash (first entry) jobs 0)))))

(-> execution-lease--source-reason (execution-lease-receipt integer) (values (option keyword) integer))
(defun execution-lease--source-reason (receipt now)
  "Resolve terminal chronology before a delayed guardian attributes cap expiry."
  (let* ((terminal (execution-lease-receipt-terminal receipt))
         (ended    (getf terminal :ended-at))
         (cap      (execution-lease-receipt-cap receipt))
         (reason   (or (getf terminal :cancellation-reason)
                       (when (eq (getf terminal :state) ':failed) ':parent-finished))))
    (cond ((and reason (or (null cap) (and ended (< ended cap))))
           (values reason (or ended now)))
          ((and cap (<= cap now))
           (values ':timeout cap))
          (reason
           (values reason (or ended now)))
          (t
           (values nil most-positive-fixnum)))))

(-> execution-lease--guardian-run (execution-lease-ledger) null)
(defun execution-lease--guardian-run (ledger)
  "Revoke absolute original caps across pools, independent of parent interruption."
  (loop
    (let ((work nil) (nearest nil) (done-p nil) (listeners nil))
      (with-lock-held ((execution-lease-lock ledger))
        (setf work (execution-lease-work ledger)
              (execution-lease-work ledger) nil)
        (execution-lease--prune ledger)
        (let ((now (get-internal-real-time)))
          ;; Earlier genuine terminal causes precede delayed cap observation.
          (dolist (source (stable-sort (copy-list (execution-lease-receipts ledger)) #'<
                                      :key (lambda (receipt)
                                             (nth-value 1 (execution-lease--source-reason receipt now)))))
            (let ((reason (execution-lease--source-reason source now)))
              (when reason
                (setf work (nconc work (execution-lease--mark-subtree ledger source reason))))))
          (dolist (receipt (execution-lease-receipts ledger))
            (let* ((reason (execution-lease--source-reason receipt now))
                   (cap    (execution-lease-receipt-cap receipt)))
              (cond
                ((not (execution-lease--current-p ledger))
                 (setf work (nconc work (execution-lease--mark-subtree ledger receipt ':replaced))))
                (reason
                 (setf work (nconc work (execution-lease--mark-subtree ledger receipt reason)))))
              ;; A completed ancestor's original cap still bounds live dependents.
              (when (and cap (not (execution-lease-receipt-reason receipt)))
                (if (<= cap now)
                    (setf work (nconc work (execution-lease--mark-subtree ledger receipt ':timeout)))
                    (setf nearest (if nearest (min nearest cap) cap))))))
          (execution-lease--controller-snapshot ledger)
          (setf done-p (and (null work)
                            (every (lambda (receipt)
                                     (and (execution-lease-receipt-terminal receipt)
                                          (zerop (execution-lease-receipt-settlements receipt))))
                                   (execution-lease-receipts ledger))))
          (when done-p
            (setf listeners (execution-lease-listeners ledger)
                  (execution-lease-listeners ledger) nil
                  (execution-lease-guardian ledger) nil))
          (unless (or work done-p)
            (condition-wait (execution-lease-changed ledger) (execution-lease-lock ledger)
                            :timeout (when nearest
                                       (/ (max 0 (- nearest now))
                                          (float internal-time-units-per-second 1d0)))))))
      ;; Request cancellation of dependents before interrupting their controllers.
      (setf work (execution-lease--cancellation-order ledger work))
      (dolist (entry work)
        ;; Public cancellation is best effort. Retain ownership until terminal evidence.
        (job-cancel (first entry) :reason (rest entry)))
      (when done-p
        (dolist (entry listeners) (job-pool-remove-listener (first entry) (rest entry)))
        (return))))
  nil)

(-> execution-lease--start-guardian (execution-lease-ledger) null)
(defun execution-lease--start-guardian (ledger)
  "Start at most one independent controller after actual entry registration."
  (let ((start-p nil))
    (with-lock-held ((execution-lease-lock ledger))
      (unless (execution-lease-guardian ledger)
        (setf (execution-lease-guardian ledger) t start-p t)))
    (when start-p
      (let ((thread (make-thread (lambda () (execution-lease--guardian-run ledger))
                                 :name "Mission original-cap guardian")))
        (with-lock-held ((execution-lease-lock ledger))
          (when (eq (execution-lease-guardian ledger) t)
            (setf (execution-lease-guardian ledger) thread))))))
  nil)

(-> execution-lease-call ((option execution-lease-ticket) job function) t)
(defun execution-lease-call (ticket job function)
  "Enter one actual scope under lifecycle -> host lock order; invoke outside locks."
  (if (null ticket)
      (funcall function)
      (let* ((ledger  (execution-lease-ticket-ledger ticket))
             (receipt nil))
        (cl-jobpond:job-call-with-admission
         job
         (lambda ()
           (with-lock-held ((execution-lease-lock ledger))
             (let* ((now     (get-internal-real-time))
                    (parents (execution-lease-ticket-parents ticket))
                    (cap     (job-deadline job))
                    (ends    (remove nil (cons cap (mapcar #'execution-lease-receipt-end parents)))))
               (when (and (eq job (execution-lease-ticket-job ticket))
                          (eq (execution-lease-ticket-state ticket) ':reserved)
                          (execution-lease--open-p ledger)
                          (< (length (execution-lease-receipts ledger)) *execution-lease-receipt-limit*)
                          (job-run-token job)
                          (every (lambda (parent) (execution-lease--ancestry-current-p ledger parent now)) parents)
                          (or (null cap) (< now cap)))
                 (setf receipt
                       (make-execution-lease-receipt
                        :ticket ticket :job job :pool (job-pool job)
                        :execution (if (typep job 'session-job)
                                       (session-job-execution-identifier job) (job-identifier job))
                        :owners    (copy-list (job-owner-identifiers job))
                        :token (job-run-token job) :started (cl-jobpond:job-started-at job)
                        :cap cap :end (when ends (reduce #'min ends))
                        :parents parents :operation (execution-lease-ticket-operation ticket))
                       (execution-lease-ticket-state ticket) ':entered)
                 (push receipt (execution-lease-receipts ledger))
                 (when *execution-lease-accounting-receipts*
                   (incf (execution-lease-receipt-settlements receipt))
                   (push receipt (rest *execution-lease-accounting-receipts*))))))))
        (unless receipt
          (execution-lease-refuse (execution-lease-context ledger) (execution-lease-attempt ledger)))
        (execution-lease--listen ledger job)
        (execution-lease--start-guardian ledger)
        (execution-lease--wake ledger)
        (let ((*execution-lease-receipt* receipt))
          (unwind-protect (funcall function)
            (with-lock-held ((execution-lease-lock ledger))
              (setf (execution-lease-receipt-body-returned-p receipt) t))
            (execution-lease--wake ledger))))))

(-> execution-lease-latch (execution-lease-ledger (integer 1)) (values null boolean))
(defun execution-lease-latch (ledger attempt)
  "Freeze entry against one exact evaluator attempt; report the unique first latch."
  (let ((changed-p nil))
    (with-lock-held ((execution-lease-lock ledger))
      (unless (execution-lease-attempt ledger)
        (setf (execution-lease-attempt ledger) attempt changed-p t)))
    (execution-lease--wake ledger)
    (values nil changed-p)))

(-> execution-lease-close (execution-lease-ledger job keyword) null)
(defun execution-lease-close (ledger job reason)
  "Close host admission only before a lifecycle publication claim can win."
  (declare (ignore reason))
  (cl-jobpond:job-call-with-admission
   job
   (lambda ()
     (with-lock-held ((execution-lease-lock ledger))
       (let ((source (find job (execution-lease-receipts ledger)
                           :key #'execution-lease-receipt-job :test #'eq)))
         (when source (setf (execution-lease-receipt-closed-p source) t))))))
  (execution-lease--wake ledger)
  nil)

(-> execution-lease-cancel (execution-lease-ledger job keyword) boolean)
(defun execution-lease-cancel (ledger job reason)
  "Close exact admission, request cancellation and hand accepted propagation off."
  (execution-lease-close ledger job reason)
  (sb-sys:without-interrupts
    (let ((accepted-p (job-cancel job :reason reason)))
      (when accepted-p
        (with-lock-held ((execution-lease-lock ledger))
          (let ((source (find job (execution-lease-receipts ledger)
                              :key #'execution-lease-receipt-job :test #'eq)))
            (when source
              (setf (execution-lease-work ledger)
                    (nconc (execution-lease-work ledger)
                           (execution-lease--mark-subtree ledger source reason)))))))
      (execution-lease--wake ledger)
      accepted-p)))

(-> execution-lease-cleanup-reason (execution-lease-ledger job keyword) keyword)
(defun execution-lease-cleanup-reason (ledger job fallback)
  "Preserve accepted cancellation or original-cap expiry before failure cleanup."
  (with-lock-held ((execution-lease-lock ledger))
    (let* ((outer  (find job (execution-lease-receipts ledger)
                         :key #'execution-lease-receipt-job :test #'eq))
           (scopes (cons outer
                         (remove-if-not
                          (lambda (receipt)
                            (and outer
                                 (eq (execution-lease-receipt-operation receipt) job)
                                 (eq (first (execution-lease-receipt-parents receipt)) outer)))
                          (execution-lease-receipts ledger))))
           (now    (get-internal-real-time)))
      (or (job-cancellation-reason job)
          (some (lambda (receipt)
                  (and receipt (job-cancellation-reason (execution-lease-receipt-job receipt))))
                scopes)
          (when (some (lambda (receipt)
                        (and receipt (execution-lease-receipt-end receipt)
                             (<= (execution-lease-receipt-end receipt) now))) scopes)
            ':timeout)
          (some (lambda (receipt) (and receipt (execution-lease-receipt-reason receipt))) scopes)
          fallback))))

(-> execution-lease-await-closure (execution-lease-ledger job) t)
(defun execution-lease-await-closure (ledger job)
  "Observe cleanup only within its immutable original end; retain unresolved ownership."
  (let ((end (with-lock-held ((execution-lease-lock ledger))
               (let ((receipt (find job (execution-lease-receipts ledger)
                                    :key #'execution-lease-receipt-job :test #'eq)))
                 (and receipt (execution-lease-receipt-end receipt))))))
    (if end
        (job-await job :timeout-seconds
                   (/ (max 0 (- end (get-internal-real-time)))
                      (float internal-time-units-per-second 1d0)))
        (job-snapshot job))))

(-> execution-lease-drain (execution-lease-ledger execution-lease-receipt) list)
(defun execution-lease-drain (ledger own-scope)
  "Wait only for the frozen entered dependents, bounded by original ends.
Expired noncooperative bodies retain their receipts and guardian ownership."
  (let ((waiter (make-condition-variable)) (retained nil))
    (unwind-protect
         (with-lock-held ((execution-lease-lock ledger))
           (setf retained
                 (remove-if-not
                  (lambda (receipt)
                    (and (not (eq receipt own-scope))
                         (execution-lease--descendant-p receipt own-scope)))
                  (copy-list (execution-lease-receipts ledger))))
           (push waiter (execution-lease-waiters ledger))
           (loop
              (let* ((pending (remove-if (lambda (receipt)
                                          (and (execution-lease-receipt-terminal receipt)
                                               (zerop (execution-lease-receipt-settlements receipt))))
                                        retained))
                     (now     (get-internal-real-time))
                     (ends    (remove nil (mapcar #'execution-lease-receipt-end pending))))
               (when (or (null pending)
                         (some (lambda (receipt)
                                 (or (execution-lease-receipt-reason receipt)
                                     (let ((end (execution-lease-receipt-end receipt)))
                                       (and end (<= end now))))) pending))
                 (return pending))
               ;; Unbounded original authority is not a bounded administrative drain.
               (unless ends (return pending))
               (condition-wait waiter (execution-lease-lock ledger)
                               :timeout (/ (max 0 (- (reduce #'min ends) now))
                                           (float internal-time-units-per-second 1d0))))))
      (with-lock-held ((execution-lease-lock ledger))
        (setf (execution-lease-waiters ledger)
              (remove waiter (execution-lease-waiters ledger) :test #'eq))))))

(defgeneric task-call-with-review-boundary (job conversation function)
  (:documentation "Invoke protected child work before its unconditional safety cleanup."))

(defmethod task-call-with-review-boundary (job conversation function)
  "Ordinary children preserve normal failure and cleanup behavior."
  (declare (ignore job conversation))
  (funcall function))

(defgeneric execution-lease-revoke-context (context reason)
  (:documentation "Revoke a replaced exact context and notify its independent guardian."))

(defmethod execution-lease-revoke-context (context reason)
  "Ordinary contexts have no registered execution authority to revoke."
  (declare (ignore context reason))
  nil)

(-> execution-lease--ancestry-current-p (execution-lease-ledger execution-lease-receipt integer) boolean)
(defun execution-lease--ancestry-current-p (ledger receipt now)
  "Check every original host edge and captured execution identity under the host gate."
  (let ((job (execution-lease-receipt-job receipt)))
    (and (eq ledger (execution-lease-ticket-ledger (execution-lease-receipt-ticket receipt)))
         (not (execution-lease-receipt-closed-p receipt))
         (not (execution-lease-receipt-reason receipt))
         (not (job-cancellation-reason job))
         (eq (execution-lease-receipt-pool receipt) (job-pool job))
         (equal (execution-lease-receipt-token receipt) (job-run-token job))
         (equal (execution-lease-receipt-owners receipt) (job-owner-identifiers job))
         (eql (execution-lease-receipt-cap receipt) (job-deadline job))
         (or (null (execution-lease-receipt-end receipt)) (< now (execution-lease-receipt-end receipt)))
         (every (lambda (parent) (execution-lease--ancestry-current-p ledger parent now))
                (execution-lease-receipt-parents receipt)))))

(-> execution-lease-accounting-call (function) t)
(defun execution-lease-accounting-call (function)
  "Retain execution through settlement or explicit original-controller reporting handoff."
  (let ((*execution-lease-accounting-receipts* (list nil)))
    (when *execution-lease-receipt*
      (let* ((receipt *execution-lease-receipt*)
             (ledger  (execution-lease-ticket-ledger (execution-lease-receipt-ticket receipt))))
        (with-lock-held ((execution-lease-lock ledger))
          (incf (execution-lease-receipt-settlements receipt))
          (push receipt (rest *execution-lease-accounting-receipts*)))))
    (unwind-protect (funcall function)
      (sb-sys:without-interrupts
        (dolist (receipt (rest *execution-lease-accounting-receipts*))
          (let ((ledger (execution-lease-ticket-ledger (execution-lease-receipt-ticket receipt))))
            (with-lock-held ((execution-lease-lock ledger))
              (decf (execution-lease-receipt-settlements receipt)))
            (execution-lease--wake ledger)))))))

(-> execution-lease--prune (execution-lease-ledger) null)
(defun execution-lease--prune (ledger)
  "Prune settled terminal leaves, retaining unresolved edges and publication scopes."
  (loop
    (let* ((receipts (execution-lease-receipts ledger))
           (removable
             (remove-if-not
              (lambda (receipt)
                (let* ((operation (execution-lease-receipt-operation receipt))
                       (owner     (and operation (find operation receipts :key #'execution-lease-receipt-job :test #'eq))))
                  (and (execution-lease-receipt-terminal receipt)
                       (zerop (execution-lease-receipt-settlements receipt))
                       (or (not (and (typep operation 'task-job)
                                     (typep (execution-lease-receipt-job receipt) 'execution-lease-inline-job)
                                     (eq owner (first (execution-lease-receipt-parents receipt)))))
                           (execution-lease-receipt-terminal owner))
                       (notany (lambda (dependent) (member receipt (execution-lease-receipt-parents dependent) :test #'eq)) receipts))))
              receipts)))
      (unless removable (return))
      (setf (execution-lease-receipts ledger) (set-difference receipts removable :test #'eq))))
  nil)

(defgeneric task-controller-accounting-report (context job)
  (:documentation "Read controller-owned unsettled reporting without changing terminal authority."))

(defmethod task-controller-accounting-report (context job)
  "Ordinary tasks without an accounting owner have no reporting handoff."
  (declare (ignore context job))
  nil)

(defgeneric task-controller-review-report (context job)
  (:documentation "Read controller-only partial evidence without granting execution authority."))

(defmethod task-controller-review-report (context job)
  "Ordinary tasks have no controller-only review evidence."
  (declare (ignore context job))
  nil)

(-> execution-lease--controller-snapshot (execution-lease-ledger) (option list))
(defun execution-lease--controller-snapshot (ledger)
  "Build bounded partial evidence under the host gate, without I/O or job operations."
  (let* ((now      (get-internal-real-time))
         (retained (remove-if (lambda (receipt)
                                (and (execution-lease-receipt-terminal receipt)
                                     (zerop (execution-lease-receipt-settlements receipt))))
                              (execution-lease-receipts ledger))))
    (when (and retained
               (some (lambda (receipt)
                       (and (execution-lease-receipt-end receipt)
                            (<= (execution-lease-receipt-end receipt) now))) retained))
      (setf (execution-lease-controller-report ledger)
            (list :kind ':controller-only :generation (execution-lease-generation ledger)
                  :mission-id (execution-lease-mission-id ledger)
                  :attempt (execution-lease-attempt ledger) :observed-at now
                  :execution-state ':unresolved :physical-termination-proven-p nil
                  :retained-count (length retained) :truncated-p (> (length retained) 64)
                  :executions
                  (loop for receipt in retained repeat 64
                        for source = (execution-lease-receipt-revocation-source receipt)
                        collect (list :execution        (execution-lease-receipt-execution receipt)
                                      :original-cap     (execution-lease-receipt-cap receipt)
                                      :effective-end    (execution-lease-receipt-end receipt)
                                      :body-returned-p  (execution-lease-receipt-body-returned-p receipt)
                                      :terminal-state   (getf (execution-lease-receipt-terminal receipt) :state)
                                      :settlements      (execution-lease-receipt-settlements receipt)
                                      :reason           (execution-lease-receipt-reason receipt)
                                      :source-execution (and source (execution-lease-receipt-execution source))
                                      :source-cap       (and source (execution-lease-receipt-cap source)))))))
    (copy-tree (execution-lease-controller-report ledger))))
