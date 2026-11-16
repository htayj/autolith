(in-package #:autolith)

;;;; -- In-Process Task Orchestration --

(defparameter *task-default-maximum-concurrency* 8
  "The default number of child agents that may run concurrently.")

(defparameter *task-maximum-concurrency* 32
  "The largest supported child-agent worker pool.")

(defparameter *task-default-maximum-runtime-milliseconds* 3600000
  "The default one-hour wall-clock cap for one child; zero disables the deadline.")

(defparameter *task-maximum-batch-size* 16
  "The largest task batch accepted atomically.")

(defparameter *task-maximum-live-jobs* 64
  "The maximum combined queued and running task jobs.")

(defparameter *task-hurry-up-maximum-agents* 2
  "The total child-agent admissions allowed during one hurry-up interval.")

(defparameter *task-terminal-retention-limit* 64
  "The maximum terminal task summaries retained in one session.")

(defparameter *task-artifact-retention-limit* 64
  "The maximum completed child artifact directories retained per conversation.")

(defparameter *tool-execution-default-maximum-concurrency* 4
  "The default number of asynchronous tool executions that may run concurrently.")

(defparameter *tool-execution-maximum-concurrency* 16
  "The largest supported asynchronous tool-execution worker pool.")

(defparameter *tool-execution-maximum-live-jobs* 64
  "The maximum combined queued and running asynchronous tool executions.")

(defparameter *tool-execution-terminal-retention-limit* 64
  "The maximum terminal tool-execution summaries retained in one session.")

(defparameter *tool-execution-summary-limit* 500
  "The operation-summary characters retained for one tool execution.")

(defparameter *tool-execution-result-limit* 8000
  "The result characters retained for one terminal tool execution.")

(defparameter *tool-execution-current-job* nil
  "The supervised tool job executing on this worker, or NIL outside execution.")

(defparameter *tool-execution-blocking-grace-seconds* 10
  "Seconds a default execution may block before handing off its existing job.")

(defparameter *task-shutdown-timeout-seconds* 10
  "The maximum time allowed for task worker shutdown.")

(defparameter *task-default-maximum-depth* 2
  "The default maximum child-agent depth below the primary agent.")

(defparameter *task-inherited-reference-maximum-bytes* 32768
  "The maximum UTF-8 wire bytes inherited by one child as parent reference.")

(defparameter *task-inherited-reference-context-divisor* 4
  "The minimum child-context fraction reserved outside inherited reference.")

(defparameter *task-default-maximum-output-bytes* 500000
  "The default maximum UTF-8 bytes retained from one child result.")

(defparameter *task-default-maximum-output-lines* 5000
  "The default maximum lines retained from one child result.")

(defparameter *task-progress-output-limit* 8000
  "The assistant-text tail retained in a live child progress snapshot.")

(defparameter *task-progress-recent-tool-limit* 8
  "The completed tool names retained in one live child activity trace.")

(defparameter *task-result-preview-limit* 6000
  "The result characters shown inline before referring to an artifact.")

(defparameter *task-identifier-maximum-characters* 64
  "The maximum friendly task identifier fragment retained by the scheduler.")

(defparameter *task-retained-assignment-limit* 1000
  "The assignment characters retained after a task becomes terminal.")

(defparameter *task-retained-output-limit* 2000
  "The result output characters retained after artifact publication.")

(defparameter *task-retained-progress-output-limit* 1000
  "The streamed output characters retained for a terminal job.")

(defparameter *task-retained-structured-output-limit* 2000
  "The readable structured-result characters retained outside its artifact.")

(defparameter *task-tool-content-limit* 16000
  "The maximum provider-visible characters returned by task and job tools.")

(defparameter *task-agent-page-default* 16
  "The default number of task-agent discovery records returned at once.")

(defparameter *task-agent-page-maximum* 32
  "The largest task-agent discovery page accepted by the provider tool.")

(defparameter *task-job-wait-maximum-seconds* 3600
  "The longest blocking wait accepted by job.wait.")

(defparameter *task-job-page-default* 32
  "The default number of job.list records returned at once.")

(defparameter *task-job-page-maximum* 64
  "The largest job.list page accepted by the provider tool.")

(defparameter *task-result-label-maximum-characters* 256
  "The maximum child yield label length accepted and retained.")

(defparameter *task-steering-maximum-items* 32
  "The most accepted child steering messages awaiting durable append.")

(defparameter *task-steering-maximum-characters* 131072
  "The largest individual steering message accepted for a running child.")

(defparameter *task-steering-maximum-total-characters* 262144
  "The largest combined queued and in-flight child steering text.")

(defparameter *task-response-promotion-maximum-items* 32
  "The most steered child responses awaiting one verbal result each.")

(defclass task-completion nil
  ((called-p :initform nil :accessor task-completion-called-p :type
             boolean :documentation
             "True after the child accepted one terminal yield.")
   (status :initform nil :accessor task-completion-status :type
           (option keyword) :documentation
           "The success, failed, or aborted yield status.")
   (text :initform nil :accessor task-completion-text :type
         (option string) :documentation
         "The optional human-readable yield result.")
   (data :initform nil :accessor task-completion-data :type t
         :documentation "The raw validated provider JSON yield value.")
   (data-present-p :initform nil :accessor task-completion-data-present-p
                   :type boolean :documentation
                   "True when the child explicitly supplied yield data, including null.")
   (error :initform nil :accessor task-completion-error :type
          (option string) :documentation
          "The optional child-reported failure text.")
   (label :initform nil :accessor task-completion-label :type
          (option string) :documentation
          "The optional concise result label."))
  (:documentation
   "The explicit terminal protocol state of one child agent."))

(defclass task-progress nil
  ((lock :initform (make-lock "Autolith task progress") :reader
         task-progress-lock :documentation
         "The lock protecting snapshots read by job tools.")
   (status :initform ':queued :accessor task-progress-status :type
           keyword :documentation
           "The queued, running, completed, failed, or aborted state.")
   (provider-connections
    :initform nil :accessor task-progress-provider-connections :type list
    :documentation "Active safe recovery statuses, keyed by exact episode within this job.")
   (provider-wait-note-at
    :initform nil :accessor task-progress-provider-wait-note-at :type (option integer)
    :documentation "Last monotonic wait-note reservation across all this job's requests.")
   (current-tool
    :initform      nil
    :accessor      task-progress-current-tool
    :type          (option string)
    :documentation "The tool currently executing in the child.")
   (current-tool-started-at
    :initform      nil
    :accessor      task-progress-current-tool-started-at
    :type          (option integer)
    :documentation "The internal real time at which the current tool began.")
   (recent-tools
    :initform      (make-deque :maximum-count *task-progress-recent-tool-limit*)
    :reader        task-progress-recent-tools
    :type          deque
    :documentation "The completed child tools retained in chronological order.")
   (output-tail
    :initform      ""
    :accessor      task-progress-output-tail
    :type          string
    :documentation "The bounded tail of streamed assistant text.")
   (request-count
    :initform      0
    :accessor      task-progress-request-count
    :type          (integer 0)
    :documentation "The provider requests started by the child.")
   (usage :initform nil :accessor task-progress-usage :type t
          :documentation "The cumulative portable provider usage for the child.")
   (started-at :initform nil :accessor task-progress-started-at :type t
               :documentation "The internal real time at which execution began.")
   (updated-at :initform (get-internal-real-time) :accessor
               task-progress-updated-at :type integer :documentation
               "The internal real time of the newest progress event."))
  (:documentation "A normalized, thread-safe child progress snapshot."))

(defclass task-orchestrator nil
  ((pool
    :initarg       :pool
    :reader        task-orchestrator-pool
    :type          job-pool
    :documentation "The supervised worker pool running this session's children.")
   (execution-pool
    :initarg       :execution-pool
    :reader        task-orchestrator-execution-pool
    :type          job-pool
    :documentation "The supervised worker pool running asynchronous tool calls.")
   (lock
    :initform      (make-lock "Autolith task orchestrator")
    :accessor      task-orchestrator-lock
    :documentation "The lock protecting naming, ordering, hurry-up, and listeners.")
   (artifact-lock
    :initform      (make-lock "Autolith task artifacts")
    :reader        task-orchestrator-artifact-lock
    :documentation "The lock serializing artifact publication and retention cleanup.")
   (closed-p
    :initform      nil
    :accessor      task-orchestrator-closed-p
    :type          boolean
    :documentation "Whether runtime shutdown has begun and ordinary refresh is forbidden.")
   (hurry-up-p
    :initarg       :hurry-up-p
    :initform      nil
    :accessor      task-orchestrator-hurry-up-p
    :type          boolean
    :documentation "Whether urgent session limits govern new child work.")
   (hurry-up-admission-count
    :initform      0
    :accessor      task-orchestrator-hurry-up-admission-count
    :type          (integer 0)
    :documentation "The children admitted or reserved during the current hurry-up interval.")
   (maximum-depth
    :initarg       :maximum-depth
    :accessor      task-orchestrator-maximum-depth
    :type          (integer 1)
    :documentation "The maximum child depth below the primary agent.")
   (next-name-index
    :initform      0
    :accessor      task-orchestrator-next-name-index
    :type          (integer 0)
    :documentation "The source of readable names for children given none.")
   (next-session-order
    :initform      0
    :accessor      task-orchestrator-next-session-order
    :type          (integer 0)
    :documentation "The source of ordering shared by child and tool jobs.")
   (completion-lock
    :initform      (make-lock "Autolith completion services")
    :reader        task-orchestrator-completion-lock
    :documentation "Serializes creation and closure of owner completion services.")
   (completion-services
    :initform      (make-hash-table :test #'equal)
    :reader        task-orchestrator-completion-services
    :documentation "Durable notification subscriptions keyed by owning conversation.")
   (listeners
    :initform      nil
    :accessor      task-orchestrator-listeners
    :type          list
    :documentation "Callbacks receiving portable task and execution events."))
  (:documentation
   "Session-scoped child and asynchronous tool execution state.

The two CL-JOBPOND pools own queueing, workers, deadlines, shutdown, and job
tables. The orchestrator owns cross-pool ordering, child naming, hurry-up policy,
nesting depth, and lifecycle listeners."))

(defclass session-job (job)
  ((lease-ticket :initarg :lease-ticket :initform nil :reader session-job-lease-ticket
                 :documentation "Host admission reservation, never a returned tool value.")
   (cancellation-observation
    :initform      nil
    :accessor      session-job-cancellation-observation
    :documentation "Autolith cancellation timing and once-only warning, under the orchestrator lock.")
   (orchestrator
    :initarg       :orchestrator
    :reader        session-job-orchestrator
    :type          task-orchestrator
    :documentation "The session orchestrator owning this job.")
   (execution-identifier
    :initarg       :execution-identifier
    :reader        session-job-execution-identifier
    :type          non-empty-string
    :documentation "The process-independent execution identity.")
   (session-order
    :initarg       :session-order
    :initform      nil
    :reader        session-job-explicit-order
    :type          (option (integer 1))
    :documentation "The optional cross-pool admission order.")
   (public-identifier
    :initarg       :public-identifier
    :initform      nil
    :reader        session-job-public-identifier
    :type          (option non-empty-string)
    :documentation "The optional identifier exposed instead of the pool identifier.")
   (parent-call-id
    :initarg       :parent-call-id
    :initform      nil
    :reader        session-job-parent-call-id
    :type          (option string)
    :documentation "The provider tool call that created this job.")
   (detached-p
    :initarg       :detached-p
    :initform      t
    :accessor      session-job-detached-p
    :type          boolean
    :documentation "True when the caller is no longer waiting for this job.")
   (completion-owner-conversation
    :initarg       :completion-owner-conversation
    :initform      nil
    :reader        session-job-completion-owner-conversation
    :type          (option non-empty-string)
    :documentation "The exact parent conversation authorized to receive this outcome.")
   (completion-policy
    :initarg       :completion-policy
    :initform      ':continue
    :reader        session-job-completion-policy
    :type          (member :notify :continue)
    :documentation "Notify at a safe boundary, or also request an automatic continuation.")
   (completion-mission-id
    :initarg       :completion-mission-id
    :initform      nil
    :reader        session-job-completion-mission-id
    :type          (option non-empty-string)
    :documentation "The admitted mission identity, preventing a completion from reviving another goal."))
  (:documentation
   "Common session ownership, identity, ordering, and waiting state for a job."))

(-> task-job--steering-entry-characters (agent-steering-input) (integer 0))
(defun task-job--steering-entry-characters (entry)
  "Return the text character count retained by steering ENTRY."
  (length
   (user-message-input-text
    (agent-steering-input-content entry))))

(defclass task-job (session-job)
  ((execution-metadata
    :initform      nil
    :accessor      task-job-execution-metadata
    :type          list
    :documentation "Portable identity and attempted model captured before child execution.")
   (administrative-checkpoint
    :initform      nil
    :accessor      task-job-administrative-checkpoint
    :documentation "Host-constructed checkpoint authority, never obtained from a returned plist.")
   (telemetry-run
    :initarg       :telemetry-run
    :initform      nil
    :accessor      task-job-telemetry-run
    :type          (option telemetry-run)
    :documentation "The run observed by this job, borrowed or created locally.")
   (telemetry-owner
    :initform      nil
    :accessor      task-job-telemetry-owner
    :documentation "Opaque finish token retained only when this job creates its run.")
   (telemetry-terminal
    :initform      nil
    :accessor      task-job-telemetry-terminal
    :type          list
    :documentation "Execution failure metadata retained until terminal publication finishes.")
   (definition :initarg :definition :accessor task-job-definition :type
               (option task-agent-definition) :documentation
               "The full child role while this job remains live.")
   (definition-summary
    :initform      nil
    :accessor      task-job-definition-summary
    :type          (option list)
    :documentation "Compact non-instruction role metadata retained at terminal state.")
   (item :initarg :item :accessor task-job-item :type list :documentation
         "The normalized assignment plist.")
   (parent-agent
    :initarg       :parent-agent
    :accessor      task-job-parent-agent
    :type          (option agent)
    :documentation "The parent session while this job remains live.")
   (mission-context
    :initarg       :mission-context
    :initform      nil
    :reader        task-job-mission-context
    :type          t
    :documentation "Exact mission authority captured at admission, before any queue delay.")
   (review-checkpoint
    :initarg       :review-checkpoint
    :initform      nil
    :reader        task-job-review-checkpoint
    :type          t
    :documentation "Optional mission reviewer budget captured before the child starts.")
   (inherited-reference-p
    :initarg       :inherited-reference-p
    :initform      nil
    :accessor      task-job-inherited-reference-p
    :type          boolean
    :documentation "Whether this job may receive its captured parent reference.")
   (inherited-reference-items
    :initarg       :inherited-reference-items
    :initform      nil
    :accessor      task-job-inherited-reference-items
    :type          list
    :documentation "The filtered parent reference messages captured at admission.")
   (command-authorization-function
    :initarg       :command-authorization-function
    :initform      nil
    :accessor      task-job-command-authorization-function
    :type          (option function)
    :documentation "The parent capability used to authorize child shell commands.")
   (tool-authorization-function
    :initarg  :tool-authorization-function
    :initform nil
    :accessor task-job-tool-authorization-function
    :type     (option function)
    :documentation
    "The parent capability used to authorize child external tool calls.")
   (steering-lock
    :initform (make-lock "Autolith task steering")
    :reader   task-job-steering-lock
    :documentation
    "The lock serializing steering, response promotion, and terminal claims.")
   (steering-items
    :initform      (make-deque :weight-function #'task-job--steering-entry-characters)
    :reader        task-job-steering-items
    :type          deque
    :documentation "Accepted steering entries waiting for the next safe boundary.")
   (steering-in-flight-items
    :initform      (make-deque :weight-function #'task-job--steering-entry-characters)
    :reader        task-job-steering-in-flight-items
    :type          deque
    :documentation "Steering entries drained but not yet durably acknowledged.")
   (response-promotion-pending-identifiers
    :initform      (make-deque)
    :reader        task-job-response-promotion-pending-identifiers
    :type          deque
    :documentation "Promoted steering identifiers waiting for durable acknowledgment.")
   (response-promotion-identifiers
    :initform      (make-deque)
    :reader        task-job-response-promotion-identifiers
    :type          deque
    :documentation "FIFO steering identifiers awaiting one durable verbal response.")
   (steering-closed-p
    :initform      nil
    :accessor      task-job-steering-closed-p
    :type          boolean
    :documentation "Whether this child has atomically stopped accepting steering.")
   (progress :initform (make-instance 'task-progress) :reader
             task-job-progress :type task-progress :documentation
             "The normalized progress visible to job inspection."))
  (:documentation
   "One synchronous or detached child-agent execution.

The lifecycle lock, state, publication claim, worker thread, run token, result,
deadline, and timings are inherited from CL-JOBPOND:JOB. Child role, assignment,
parent, and borrowed capabilities are released at terminal state."))

(defclass tool-execution-job (session-job)
  ((tool-name
    :initarg       :tool-name
    :reader        tool-execution-job-tool-name
    :type          non-empty-string
    :documentation "The canonical shell or Lisp tool name being executed.")
   (description
    :initarg       :description
    :initform      nil
    :reader        tool-execution-job-description
    :type          (option string)
    :documentation "The optional model-authored purpose shown during execution.")
   (summary
    :initarg       :summary
    :accessor      tool-execution-job-summary
    :type          string
    :documentation "The bounded operation summary retained for inspection.")
   (activity
    :initform      nil
    :accessor      tool-execution-job-activity
    :type          (option string)
    :documentation "The latest bounded content-free execution phase, under the job lock.")
   (activity-at
    :initform      nil
    :accessor      tool-execution-job-activity-at
    :type          (option integer)
    :documentation "The monotonic time of the latest real phase update, under the job lock.")
   (operation-function
    :initarg       :operation-function
    :accessor      tool-execution-job-operation-function
    :type          (option function)
    :documentation "The live operation called once and cleared at terminal state."))
  (:documentation "One inspectable asynchronous shell or Lisp tool execution."))

(-> task-job-orchestrator (task-job) task-orchestrator)
(defun task-job-orchestrator (job)
  "Return the session orchestrator owning task JOB."
  (session-job-orchestrator job))

(-> task-job-execution-identifier (task-job) non-empty-string)
(defun task-job-execution-identifier (job)
  "Return task JOB's process-independent execution identity."
  (session-job-execution-identifier job))

(-> task-job-parent-call-id (task-job) (option string))
(defun task-job-parent-call-id (job)
  "Return the task.run call that created task JOB."
  (session-job-parent-call-id job))

(-> task-job-detached-p (task-job) boolean)
(defun task-job-detached-p (job)
  "Return true when the parent is not waiting for task JOB."
  (session-job-detached-p job))

(-> task-job--steering-pending-p (task-job) boolean)
(defun task-job--steering-pending-p (job)
  "Return true when locked JOB has queued or in-flight steering."
  (or (not (deque-empty-p (task-job-steering-items job)))
      (not (deque-empty-p (task-job-steering-in-flight-items job)))))

(-> task-job--claim-normal-completion (task-job) boolean)
(defun task-job--claim-normal-completion (job)
  "Atomically close running JOB for normal completion unless steering is pending."
  (cl-jobpond:job-call-with-admission
   job
   (lambda ()
     (with-lock-held ((task-job-steering-lock job))
       (cond
         ((task-job-steering-closed-p job)
          t)
         ((task-job--steering-pending-p job)
          nil)
         (t
          (setf (task-job-steering-closed-p job) t)
          t))))))

(-> task-job--call-with-yield-claim
    (task-job function)
    (values boolean keyword))
(defun task-job--call-with-yield-claim (job function)
  "Call FUNCTION under JOB's terminal-yield claim, or return its rejection reason."
  (cl-jobpond:job-call-with-admission
   job
   (lambda ()
     (with-lock-held ((task-job-steering-lock job))
       (cond
         ((task-job-steering-closed-p job)
          (values nil ':closed))
         ((task-job--steering-pending-p job)
          (values nil ':steering-pending))
         (t
          (setf (task-job-steering-closed-p job) t)
          (funcall function)
          (values t ':accepted)))))))

(defclass task-child-agent (agent)
  ((definition :initarg :definition :reader task-child-agent-definition
               :type task-agent-definition :documentation
               "The role and policy configuring this child.")
   (identity :initarg :identity :reader task-child-agent-identity :type
             list :documentation "The stable identity of this child.")
   (peer-endpoint :initform nil :accessor task-child-agent-peer-endpoint
                  :documentation "Immutable peer endpoint captured when this child is initialized.")
   (depth :initarg :depth :reader task-child-agent-depth :type
          (integer 1) :documentation
          "The explicit child depth below the primary agent.")
   (completion :initarg :completion :reader task-child-agent-completion
               :type task-completion :documentation
               "The required terminal yield state.")
   (orchestrator
    :initarg       :orchestrator
    :reader        task-child-agent-orchestrator
    :type          task-orchestrator
    :documentation "The shared session task orchestrator.")
   (job :initarg :job :reader task-child-agent-job :type task-job
        :documentation
        "The lifecycle and progress record for this child."))
  (:documentation
   "A real in-process agent session that must finish through yield.submit."))

(defmethod agent-hurry-up-p ((agent task-child-agent))
  "Return the live hurry-up policy shared by AGENT's task orchestrator."
  (task-orchestrator-hurry-up-p (task-child-agent-orchestrator agent)))

(defvar *task-admission-parent-locked-p* nil
  "True while nested task admission has already checked its parent job.")

(-> task--condition-broadcast (t) null)
(defun task--condition-broadcast (condition-variable)
  "Wake every waiter on CONDITION-VARIABLE through the narrow SBCL adapter."
  #+sbcl
  (sb-thread:condition-broadcast condition-variable)
  #-sbcl
  (condition-notify condition-variable)
  nil)

(defmethod agent-turn-complete-p
    ((agent task-child-agent) (result provider-result))
  "Return true after AGENT atomically yields or claims an unsteered stop."
  (or (task-completion-called-p (task-child-agent-completion agent))
      (and (call-next-method)
           (task-job--claim-normal-completion
            (task-child-agent-job agent)))))

(defmethod agent-turn-completion-details ((agent task-child-agent))
  "Identify whether AGENT completed through its explicit yield protocol."
  (list :yielded-p
        (task-completion-called-p (task-child-agent-completion agent))))

(defclass task-tool-result (tool-result)
  ((details :initarg :details :reader task-tool-result-details
            :reader tool-result-details :type t
            :documentation
            "Portable machine-readable task or job orchestration details."))
  (:documentation
   "A normal tool result carrying structured orchestration metadata."))

(defclass task-orchestrator-tool (tool)
  ((orchestrator
    :initarg       :orchestrator
    :reader        task-orchestrator-tool-orchestrator
    :type          task-orchestrator
    :documentation "The session-scoped scheduler shared by task and job tools."))
  (:documentation "A provider tool backed by one shared task orchestrator."))

(defclass task-run-tool (task-orchestrator-tool) nil
  (:documentation
   "Spawn one child agent or a concurrency-limited batch."))

(defclass task-agents-tool (task-orchestrator-tool) nil
  (:documentation "Discover effective child roles and rejected role files."))

(defclass task-job-tool (task-orchestrator-tool) nil
  (:documentation "Inspect, wait for, or cancel child and tool execution jobs."))

(defmethod tool-storm-guard-exempt-p ((tool task-agents-tool))
  "Exempt child-role discovery from the mutating-call storm guard."
  t)

(-> task-run-tool-orchestrator (task-run-tool) task-orchestrator)
(defun task-run-tool-orchestrator (tool)
  "Return TOOL's shared task orchestrator."
  (task-orchestrator-tool-orchestrator tool))

(-> task-agents-tool-orchestrator (task-agents-tool) task-orchestrator)
(defun task-agents-tool-orchestrator (tool)
  "Return TOOL's shared task orchestrator."
  (task-orchestrator-tool-orchestrator tool))

(-> task-job-tool-orchestrator (task-job-tool) task-orchestrator)
(defun task-job-tool-orchestrator (tool)
  "Return TOOL's shared task orchestrator."
  (task-orchestrator-tool-orchestrator tool))

(defclass task-yield-tool (tool) nil
  (:documentation
   "Submit the required terminal result from a child agent."))

(defclass task-note-tool (tool) nil
  (:documentation
   "Post one bounded interim child note to the parent agent."))

(defmethod tool-storm-guard-exempt-p ((tool task-yield-tool))
  "Exempt the required terminal child yield from the mutating-call storm guard."
  t)

(defmethod tool-execution-policy ((tool task-yield-tool))
  "Execute the terminal child yield without concurrent sibling calls."
  (declare (ignore tool))
  ':exclusive)

(defmethod tool-decode-arguments ((tool task-run-tool) source)
  "Decode task.run booleans without conflating JSON false and null."
  (declare (ignore tool))
  (task-json-decode source :tool-name "task.run"))

(defmethod tool-decode-arguments ((tool task-yield-tool) source)
  "Decode yield values without conflating JSON false and null."
  (declare (ignore tool))
  (task-json-decode source :tool-name "yield.submit"))

(defmethod tool-decode-arguments ((tool task-agents-tool) source)
  "Decode task.agents values without conflating JSON false and null."
  (declare (ignore tool))
  (task-json-decode source :tool-name "task.agents"))

(defmethod tool-decode-arguments ((tool task-job-tool) source)
  "Decode job tool values without conflating JSON false and null."
  (task-json-decode source :tool-name (tool-canonical-name tool)))


;;;; -- Administrative Checkpoints --

(defstruct (task-administrative-checkpoint
             (:constructor task--make-administrative-checkpoint (context generation attempt data)))
  "Ephemeral host authority. DATA is inspection evidence, never restart authority."
  (context nil :read-only t)
  (generation nil :read-only t)
  (attempt nil :read-only t)
  (data nil :read-only t))

(defgeneric task-checkpoint-current-p (context generation attempt)
  (:documentation "Validate the exact host mission generation and latched review attempt, without I/O."))

(defmethod task-checkpoint-current-p (context generation attempt)
  "Unknown origins cannot mint administrative results."
  (declare (ignore context generation attempt))
  nil)

(defgeneric task-checkpoint-mission-identifier (context)
  (:documentation "Read the exact host mission identity without I/O or lifecycle reentry."))

(defmethod task-checkpoint-mission-identifier (context)
  "Unknown origins have no checkpoint mission identity."
  (declare (ignore context))
  nil)

(-> task-checkpoint-data-p (t) boolean)
(defun task-checkpoint-data-p (data)
  "Recognize bounded portable checkpoint evidence, not execution authority."
  (and (listp data)
       (handler-case
            (and (eql 24 (list-length data))
                 (equal (loop for (key) on data by (lambda (tail) (rest (rest tail))) collect key)
                        '(:version :job-id :execution-id :generation :mission-id :run-token
                          :original-cap :owners :attempt :cursor :next-action :durable-checkpoint-p))
                 (eql 1 (getf data :version))
                (every (lambda (key)
                         (let ((value (getf data key)))
                           (and (stringp value) (<= 1 (length value) 128))))
                       '(:job-id :execution-id :generation :mission-id :run-token))
                (typep (getf data :attempt) '(integer 1))
                (or (null (getf data :original-cap)) (integerp (getf data :original-cap)))
                (listp (getf data :owners))
                (typep (list-length (getf data :owners)) '(integer 0 64))
                (every (lambda (value) (and (stringp value) (<= 1 (length value) 128)))
                       (getf data :owners))
                (typep (getf data :durable-checkpoint-p) 'boolean)
                (eq (getf data :next-action) ':review-required)
                (task-checkpoint-cursor-p (getf data :cursor))
                t)
         (error () nil))))

(-> task-review-required-result-p (t &key (:record list)) boolean)
(defun task-review-required-result-p (result &key record)
  "Validate an administrative projection, optionally against its durable ownership record.
  This predicate grants no replay, provider, tool or continuation authority."
  (and (listp result)
       (handler-case
           (let ((data (getf result :administrative-checkpoint)))
             (and (eq (getf result :status) ':suspended)
                  (eq (getf result :outcome) ':review-required)
                  (eq (getf result :execution-state) ':aborted)
                  (not (getf result :yielded-p))
                  (task-checkpoint-data-p data)
                  (equal (getf result :id) (getf data :job-id))
                  (eql (getf result :durable-checkpoint-p)
                       (getf data :durable-checkpoint-p))
                  (eq (getf result :next-action) (getf data :next-action))
                  (equal (getf result :cursor) (getf data :cursor))
                  (or (null record)
                      (and (equal (getf data :job-id) (getf record :job-id))
                           (equal (getf data :execution-id) (getf record :execution-id))
                           (equal (getf data :owners) (getf record :owners))
                           (equal data (getf record :administrative-checkpoint))))
                  t))
         (error () nil))))

(-> task-job-capture-review-checkpoint
    (task-job &key (:context t) (:generation string) (:attempt (integer 1))
                  (:mission-id string) (:cursor list) (:durable-checkpoint-p boolean))
    (option task-administrative-checkpoint))
(defun task-job-capture-review-checkpoint
    (job &key context generation attempt mission-id cursor durable-checkpoint-p)
  "Capture one host checkpoint while JOB still owns its original live execution.
The context protocol must validate the exact latched attempt without I/O. This
constructor performs bounded bookkeeping only, never storage or model work.
Privileged Lisp can subvert this in-process boundary; returned tool data cannot."
  (cl-jobpond:job-call-with-admission
   job
   (lambda ()
     (when (and (eq context (task-job-mission-context job))
                (task-checkpoint-current-p context generation attempt)
                (task-checkpoint-lineage-current-p context job nil)
                (equal mission-id (task-checkpoint-mission-identifier context))
                (or (null (job-deadline job))
                    (< (get-internal-real-time) (job-deadline job)))
                (not (task-job-administrative-checkpoint job)))
       (let ((data (list :version              1
                         :job-id               (session-job-identifier job)
                         :execution-id         (session-job-execution-identifier job)
                         :generation generation :mission-id mission-id
                         :run-token            (job-run-token job)
                         :original-cap         (job-deadline job)
                         :owners               (copy-list (job-owner-identifiers job))
                         :attempt attempt :cursor (copy-tree cursor)
                         :next-action          ':review-required
                         :durable-checkpoint-p durable-checkpoint-p)))
         (when (task-checkpoint-data-p data)
           (setf (task-job-administrative-checkpoint job)
                 (task--make-administrative-checkpoint
                  context generation attempt data))))))))

(-> task-job--checkpoint-data (task-job) (option list))
(defun task-job--checkpoint-data (job)
  "Validate retained host authority during terminal publication, without lifecycle admission.
  The library already owns the publication claim. Accepted cancellation wins."
  (let ((candidate (task-job-administrative-checkpoint job)))
    (when (and (task-administrative-checkpoint-p candidate)
               (not (job-cancellation-reason job)))
      (let ((data (task-administrative-checkpoint-data candidate)))
        (when (and (task-checkpoint-data-p data)
                   (task-checkpoint-lineage-current-p
                    (task-administrative-checkpoint-context candidate) job t)
                   (eq (task-job-mission-context job)
                       (task-administrative-checkpoint-context candidate))
                   (task-checkpoint-current-p
                    (task-administrative-checkpoint-context candidate)
                    (task-administrative-checkpoint-generation candidate)
                    (task-administrative-checkpoint-attempt candidate))
                   (equal (getf data :mission-id)
                          (task-checkpoint-mission-identifier
                           (task-administrative-checkpoint-context candidate)))
                   (equal (getf data :generation)
                          (task-administrative-checkpoint-generation candidate))
                   (eql (getf data :attempt)
                        (task-administrative-checkpoint-attempt candidate))
                   (equal (getf data :job-id) (session-job-identifier job))
                   (equal (getf data :execution-id) (session-job-execution-identifier job))
                   (equal (getf data :owners) (job-owner-identifiers job))
                   (equal (getf data :run-token) (job-run-token job))
                   (eql (getf data :original-cap) (job-deadline job))
                   (or (null (job-deadline job))
                       (< (get-internal-real-time) (job-deadline job))))
          (copy-tree data))))))

(-> task-checkpoint-projection (t &key (:record list)) list)
(defun task-checkpoint-projection (result &key record)
  "Return the single administrative projection shared by progress, continuity and notices."
  (when (task-review-required-result-p result :record record)
    (list :administrative-status ':suspended :outcome ':review-required
          :execution-state ':aborted :next-action ':review-required
          :cursor               (copy-tree (getf result :cursor))
          :durable-checkpoint-p (getf result :durable-checkpoint-p)
          :persistence-failed   (getf result :persistence-failed))))

(-> task-checkpoint-cursor-p (t) boolean)
(defun task-checkpoint-cursor-p (cursor)
  "Recognize bounded legacy and sequence-addressed review cursors as evidence only."
  (handler-case
      (and (listp cursor)
           (member (list-length cursor) '(6 14))
           (let ((keys (loop for (key) on cursor by (lambda (tail) (rest (rest tail))) collect key)))
             (or (equal keys '(:conversation-id :request-count :next-action))
                 (and (equal keys '(:conversation-id :request-count :next-action :sequence :intent :accounting :persistence-failed))
                      (typep (getf cursor :sequence) '(integer 0))
                      (stringp (getf cursor :intent)) (<= (length (getf cursor :intent)) 1000)
                      (typep (getf cursor :persistence-failed) 'boolean)
                      (let ((accounting (getf cursor :accounting)))
                        (and (listp accounting) (eql (list-length accounting) 10)
                             (equal (loop for (key) on accounting by (lambda (tail) (rest (rest tail))) collect key)
                                    '(:turns-used :tokens-used :requests-outstanding :tokens-reserved :unknown-usage))
                             (every (lambda (value) (typep value '(integer 0)))
                                    (loop for tail on accounting by (lambda (tail) (rest (rest tail))) collect (second tail))))))))
           (let ((id (getf cursor :conversation-id)))
             (and (stringp id) (<= 1 (length id) 128)))
           (typep (getf cursor :request-count) '(integer 0))
           (eq (getf cursor :next-action) ':review-required)
           t)
    (error () nil)))
