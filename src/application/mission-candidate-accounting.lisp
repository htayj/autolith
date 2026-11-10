(in-package #:autolith)

;;;; -- Durable Candidate Accounting --

(defparameter *mission-candidate-append-condition* nil
  "Exact original append condition retained after confirmed commit reconciliation.")

(defparameter *mission-candidate-admission-committed* nil
  "Confirmed admission staging marker, before fallible publication.")

(defparameter *mission-candidate-admission-started* nil
  "Whether admission crossed the fallible durable append boundary.")

(-> mission-candidate--entry (mission-context) (option list))
(defun mission-candidate--entry (context)
  "Read detached accounting for the exact candidate in its canonical origin."
  (conversation-candidate-ledger-lookup
   (candidate-origin-writer (mission-context-origin-owner context))
   (getf (mission-context-goal context) :candidate-id)))

(-> mission-candidate--baseline (list) list)
(defun mission-candidate--baseline (goal)
  "Capture ordinary legacy counters once; never reconstruct historical operations."
  (list :migration-version 1 :turns-used (getf goal :turns-used 0)
        :tokens-used                (getf goal :tokens-used 0)
        :reservation-charged-tokens (getf goal :reservation-charged-tokens 0)
        :unknown-usage              (+ (getf goal :unknown-usage 0) (getf goal :requests-outstanding 0))
        :child-charges
        (loop for (id . charge) in (getf goal :reservation-charges-by-child)
              collect (list :child-id id :tokens (getf charge :tokens 0)))
        :legacy-reservations
        (when (plusp (getf goal :requests-outstanding 0))
          (list (list :id "legacy-unresolved" :reserved (getf goal :tokens-reserved 0) :quarantined t)))
        :child-unknown-usage
        (loop for (id . count) in (getf goal :unknown-usage-by-child)
              collect (list :child-id id :requests count))
        :child-charge-requests
        (loop for (id . charge) in (getf goal :reservation-charges-by-child)
              collect (list :child-id id :requests (getf charge :requests 0)))
        :legacy-requests            (getf goal :requests-outstanding 0)))

(-> mission-candidate--append (mission-context string keyword &key (:payload t) (:baseline t)) (values list keyword t))
(defun mission-candidate--append (context operation-id event &key payload baseline)
  "Confirm a ledger append before publishing any application accounting."
  (let* ((owner  (mission-context-origin-owner context))
         (writer (candidate-origin-writer owner))
         (record (list :candidate-ledger :version 1
                       :origin           (conversation-candidate-ledger-origin writer)
                       :candidate-id     (getf (mission-context-goal context) :candidate-id)
                       :operation-id operation-id :event event :baseline baseline :payload payload)))
    (candidate-origin-validate owner)
    (handler-case
        (multiple-value-bind (stored outcome original) (conversation-candidate-ledger-append writer record)
          (when original (setf *mission-candidate-append-condition* original))
          (values stored outcome original))
      (candidate-ledger-consistency-error (condition)
        (unless (eq :absent (candidate-ledger-error-outcome condition))
          (setf (candidate-origin-uncertain-p owner) t
                (candidate-origin-uncertainty owner)
                (list :condition condition :context context :record record)))
        (error condition)))))

(-> mission-candidate--fold (list) list)
(defun mission-candidate--fold (entry)
  "Fold immutable legacy attribution and exact operation contributions."
  (let* ((baseline (getf entry :baseline))
         (result   (list :turns-used                 (getf baseline :turns-used)
                         :tokens-used                (getf baseline :tokens-used)
                         :reservation-charged-tokens (getf baseline :reservation-charged-tokens)
                         :unknown-usage              (getf baseline :unknown-usage)
                         :requests-outstanding 0 :tokens-reserved 0
                         :unknown-usage-by-child
                         (loop for row in (getf baseline :child-unknown-usage)
                               collect (cons (getf row :child-id) (getf row :requests)))
                         :reservation-charges-by-child
                         (loop for row in (getf baseline :child-charges)
                               collect (cons (getf row :child-id)
                                             (list :tokens (getf row :tokens)
                                                   :requests
                                                   (getf (find (getf row :child-id)
                                                               (getf baseline :child-charge-requests)
                                                               :key  (lambda (item) (getf item :child-id))
                                                               :test #'equal) :requests 0)))))))
    (dolist (operation (getf entry :operations))
      (incf (getf result :turns-used))
      (let ((terminal (getf operation :terminal)))
        (cond
          (terminal
           (case (getf terminal :kind)
             (:measured
              (incf (getf result :tokens-used) (getf terminal :tokens)))
             (:exact-child
              (incf (getf result :tokens-used) (getf terminal :tokens))
              (incf (getf result :reservation-charged-tokens) (getf terminal :tokens)))
             (:unknown
              (incf (getf result :unknown-usage))))
           (when (getf terminal :child-id)
             (let* ((id  (getf terminal :child-id))
                    (row (assoc id (getf result :unknown-usage-by-child) :test #'equal)))
               (if row (incf (rest row))
                   (push (cons id 1) (getf result :unknown-usage-by-child)))
               (when (eq :exact-child (getf terminal :kind))
                 (let ((charge (assoc id (getf result :reservation-charges-by-child) :test #'equal)))
                   (unless charge
                     (setf charge (cons id (list :tokens 0 :requests 0)))
                     (push charge (getf result :reservation-charges-by-child)))
                   (incf (getf (rest charge) :tokens) (getf terminal :tokens))
                   (incf (getf (rest charge) :requests)))))))
          ((getf operation :recoveries)
           (incf (getf result :unknown-usage)))
          (t
           (incf (getf result :requests-outstanding))
           (incf (getf result :tokens-reserved) (getf (getf operation :admission) :reserved))))))
    result))

(-> mission-candidate--apply (mission-context list) null)
(defun mission-candidate--apply (context entry)
  "Apply accounting fields without replacing specification, claim or a BLOCKED latch."
  (let ((goal (mission-context-goal context)))
    (with-recursive-lock-held ((mission-context-lock context))
      (loop for (key value) on (mission-candidate--fold entry) by (lambda (tail) (nthcdr 2 tail))
            do (unless (nth-value 2 (get-properties goal (list key)))
                 (nconc goal (list key nil)))
               (setf (getf goal key) value))
      (when (and (eq :draft (getf goal :status))
                 (or (getf entry :quarantined-p)
                     (and (mission--unknown-usage-blocks-p goal)
                          (or (plusp (getf (getf entry :baseline) :unknown-usage))
                              (some (lambda (operation)
                                      (and (getf operation :recoveries) (not (getf operation :terminal))))
                                    (getf entry :operations))))))
        (setf (getf goal :status) :blocked))))

  nil)

(-> mission-candidate--publish (mission-context) null)
(defun mission-candidate--publish (context)
  "Refresh registered same-ID views only; never reinstall a replaced or cleared candidate."
  (let* ((owner (mission-context-origin-owner context))
         (entry (mission-candidate--entry context))
         (id    (getf entry :candidate-id)))
    (dolist (view (candidate-origin-views owner))
      (when (equal id (getf (mission-context-goal view) :candidate-id))
        (mission-candidate--apply view entry))))
  nil)

(-> mission-candidate--restore (mission-context) boolean)
(defun mission-candidate--restore (context)
  "Quarantine interrupted operations separately from terminals and fold onto the latest specification."
  (let ((entry (mission-candidate--entry context)))
    (unless entry (return-from mission-candidate--restore nil))
    (dolist (operation (getf entry :operations))
      (unless (or (getf operation :terminal) (getf operation :recoveries))
        (mission-candidate--append context (getf operation :operation-id) :recovery :payload
                                  '(:kind :interrupted :evidence "Candidate restored before terminal evidence."))))
    (mission-candidate--publish context)
    (let* ((goal      (mission-context-goal context))
           (unknown-p (and (eq (getf goal :status) :draft)
                           (mission--unknown-usage-blocks-p goal))))
      (when (and (some (lambda (operation) (getf operation :terminal)) (getf entry :operations))
                 (member (getf goal :status) '(:draft :blocked))
                 (or unknown-p
                     (and (> (getf goal :tokens-used) (getf goal :token-limit))
                          (not (eq (mission-exhaustion--policy context) :evaluate)))))
        ;; Durable terminals survive a lost optional policy snapshot.
        ;; Reconstruct policy without replaying any operation contribution.
        (let ((*mission-candidate-policy-audit* (list nil)))
          (mission--settlement-policy context 0 (unless unknown-p 0) :attributed-p t))))
    t))

(-> mission-candidate--admit (mission-context string (option list)) integer)
(defun mission-candidate--admit (context request-id allowance)
  "Atomically establish the baseline and first admission, refusing UUID reuse before authority."
  (with-recursive-lock-held ((mission-context-lock context))
    (let* ((goal   (mission-context-goal context))
           (owner  (mission-context-origin-owner context))
           (writer (candidate-origin-writer owner))
           (limits (remove nil (list goal allowance))))
      (mission--ensure-reservations goal)
      (when allowance (mission--ensure-reservations allowance))
      (when (find request-id (getf (conversation-candidate-ledger-snapshot writer) :records)
                  :key (lambda (record) (getf (candidate-ledger--properties record) :operation-id)) :test #'equal)
        (mission--reject :candidate-operation-collision "Candidate operation UUID already exists."))
      (when (and allowance
                 (or (>= (getf allowance :turns-used) (getf allowance :turn-limit))
                     (>= (getf allowance :tokens-used) (getf allowance :token-limit))))
        (mission--reject :review-budget "Independent reviewer allowance exhausted."))
      (unless (every (lambda (limit) (plusp (mission--available-tokens limit))) limits)
        (mission--reject :budget-reserved "Outstanding requests hold the remaining output allowance."))
      (let* ((amount (reduce #'min limits :key #'mission--output-reservation))
             (ticket (mission-context-candidate-ticket context)))
        (setf *mission-candidate-admission-started* t)
        (mission-candidate--append
         context request-id :admission :payload
         (list :revision (getf goal :specification-revision) :digest (getf goal :specification-digest)
               :reserved amount :review-id (mission-draft-ticket-review-id ticket)
               :job-id       (job-identifier (mission-draft-ticket-job ticket))
               :execution-id (when (typep *mission-admitted-operation* 'task-job)
                               (task-job-execution-identifier *mission-admitted-operation*)))
         :baseline                     (unless (mission-candidate--entry context) (mission-candidate--baseline goal)))
        (setf *mission-candidate-admission-committed* t)
        (mission-candidate--publish context)
        (when allowance
          (incf (getf allowance :turns-used))
          (incf (getf allowance :requests-outstanding))
          (incf (getf allowance :tokens-reserved) amount))
        amount))))

(-> mission-candidate--terminal-payload (mission-context integer (option integer) &key (:child (option task-job))) list)
(defun mission-candidate--terminal-payload (context amount tokens &key child)
  "Authenticate ordinary measured, exact-child or unknown terminal contribution."
  (when (and tokens (not (typep tokens '(integer 0))))
    (mission--reject :candidate-usage "Measured usage must be a nonnegative integer."))
  (let ((exact-child (and child (eq context (task-job-mission-context child)) child)))
    (cond (tokens
           (list :kind :measured :tokens tokens))
          (exact-child
           (append (if (plusp amount) (list :kind :exact-child :tokens amount) (list :kind :unknown))
                   (list :child-id (job-identifier exact-child)
                         :evidence (task-job-execution-identifier exact-child))))
          (t
           (list :kind :unknown)))))

(-> mission-candidate--terminal (mission-context string list) null)
(defun mission-candidate--terminal (context request-id payload)
  "Confirm terminal durability before replacing provisional accounting."
  (mission-candidate--append context request-id :terminal :payload payload)
  (mission-candidate--publish context)
  nil)
