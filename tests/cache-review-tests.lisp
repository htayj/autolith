(in-package #:autolith)

;;;; -- Cache Lifetime and Final Wire Observation --

(defclass cache-review-test-provider (openrouter-chat-completions-provider) ()
  (:documentation "Construct real OpenRouter requests with an inert transport."))

(defmethod provider-open-response-stream
    ((provider cache-review-test-provider) (request hash-table) &key credentials conversation)
  "Accept the final request without credentials or network access."
  (declare (ignore provider request credentials conversation))
  (values nil 200 nil))

(-> test-cache-review-retention () null)
(defun test-cache-review-retention ()
  "Use Claude's requested lifetime for warm resumes and preserve override precedence."
  (with-test-configuration (configuration)
    (setf (configuration-validation-deferred-p configuration) t)
    (dolist (route '((anthropic-api-key-provider "claude-sonnet-5")
                     (openrouter-chat-completions-provider "openrouter/anthropic/claude-sonnet-4.5")))
      (destructuring-bind (class model) route
        (setf (config :model configuration) model)
        (let ((provider (if (eq class 'anthropic-api-key-provider)
                            (anthropic-provider-create configuration)
                            (openrouter-provider-create configuration))))
          (dolist (case '(("5m" 300) ("1h" 3600)))
            (destructuring-bind (ttl seconds) case
              (setf (config :prompt-cache-ttl configuration) ttl)
              (test-assert (= seconds (provider-prompt-cache-retention-seconds provider configuration))
                           "the local estimate follows the requested wire lifetime")
              (let* ((identity     (list (provider-family provider) model))
                     (conversation (conversation-create configuration)))
                (conversation-append-user-message conversation "start")
                (conversation-append-provider-metadata conversation
                                                      (list :request-number 1 :request-identity identity))
                (conversation-append-provider-item conversation (precompaction-tests--call "large"))
                (conversation-append-tool-result conversation "large" :tool-name "shell.run"
                                                 :output                         (make-string 10000 :initial-element #\x)
                                                 :success-p                      nil)
                (conversation-append-user-message conversation "next")
                (let ((loaded (conversation-load (conversation-pathname conversation))))
                  (test-assert (zerop (conversation-prepare-provider-request
                                       loaded :identity identity :idle-seconds seconds))
                               "a prompt Claude resume preserves its warm prefix"))
                (let ((loaded (conversation-load (conversation-pathname conversation))))
                  (test-assert (= 1 (conversation-prepare-provider-request
                                     loaded :identity identity :idle-seconds seconds
                                     :now (+ (get-universal-time) seconds 1)))
                               "a cold Claude resume records bounded output")))))
          (setf (config :prompt-cache-retention-seconds configuration) 1234)
          (test-assert (= 1234 (provider-prompt-cache-retention-seconds provider configuration))
                       "an explicit retention estimate overrides the requested TTL")
          (setf (config :prompt-cache-retention-seconds configuration) 0))))
    (setf (config :model configuration) "openrouter/openai/gpt-test")
    (test-assert (null (provider-prompt-cache-retention-estimate
                        (make-instance 'openrouter-chat-completions-provider :configuration configuration
                                                                            :family         ':openrouter)))
                 "unrelated OpenRouter routes have no implicit Claude lifetime"))
  nil)

(-> test-cache-review-final-wire () null)
(defun test-cache-review-final-wire ()
  "Observe routing fields and marker locations only after final request construction."
  (with-test-configuration (configuration)
    (setf (configuration-validation-deferred-p configuration) t)
    (setf (config :model configuration) "openrouter/anthropic/claude-sonnet-4.5")
    (let* ((provider                         (make-instance 'cache-review-test-provider :configuration configuration
                                                                                     :family           ':openrouter))
           (conversation                     (conversation-create configuration))
           (observed                         nil)
           (*provider-cache-request-prefix*  nil)
           (*provider-cache-prefix-function* (lambda (prefix) (setf observed prefix))))
      (conversation-append-user-message conversation "durable prefix")
      (flet ((request ()
               "Encode and observe one final inert transport request."
               (setf observed nil)
               (let ((request (provider-request-object provider conversation #())))
                 (test-assert (null observed) "intermediate encoding does not report a final wire plan")
                 (provider-open-response-stream provider request)
                 (test-assert (equal (getf observed :wire)
                                     (provider-cache-wire-plan
                                      request
                                      :volatile-items (getf observed :volatile-items 0)))
                              "the observed wire plan matches the final transport request")
                 (test-assert (plusp (provider-cache-prefix-characters observed))
                              "final routing preserves reusable history fingerprints")
                 observed)))
        (let ((base (request)))
          (setf (config :openrouter-provider-routing configuration) "{\"*\":{\"order\":[\"Anthropic\"]}}")
          (let* ((routed (request))
                 (change (provider-cache-wire-change (getf base :wire) (getf routed :wire))))
            (test-assert (member "provider" (getf change :fields) :test #'equal)
                         "routing-only changes are named in the final-request difference")
            (conversation-append-user-message conversation "appended durable item")
            (let* ((extended (request))
                   (change   (provider-cache-wire-change (getf routed :wire) (getf extended :wire))))
              (test-assert (getf change :breakpoints-moved-p)
                           "the observed difference includes final history cache marker movement")))))))
  nil)


;;;; -- Unfiltered Controller Request Prefix Probes --

(-> cache-hourly--lcp (sequence sequence) unsigned-byte)
(defun cache-hourly--lcp (left right)
  "Count the identical leading bytes or characters without filtering either input."
  (or (mismatch left right) (min (length left) (length right))))

(-> cache-hourly--input-slices (string) list)
(defun cache-hourly--input-slices (body)
  "Extract exact JSON item substrings from the actual encoded input array."
  (let* ((key   (search "\"input\"" body))
         (start (and key (position #\[ body :start (+ key 7))))
         (depth 0) (quoted nil) (escaped nil) (begin nil) (slices nil))
    (unless start (error "No input array in captured request."))
    (loop for index from (1+ start) below (length body)
          for character = (char body index)
          do (cond
               (quoted
                (cond (escaped
                       (setf escaped nil))
                      ((char= character #\\)
                       (setf escaped t))
                      ((char= character #\")
                       (setf quoted nil))))
               ((char= character #\")
                (setf quoted t))
               ((find character "{[")
                (when (zerop depth) (setf begin index))
                (incf depth))
               ((find character "}]")
                (when (zerop depth) (return))
                (decf depth)
                (when (zerop depth)
                  (push (subseq body begin (1+ index)) slices)))))
    (nreverse slices)))

(-> cache-hourly--write (string t) null)
(defun cache-hourly--write (name value)
  "Retain only synthetic fixture evidence when the private probe directory is set."
  (let ((directory (uiop:getenv "AUTOLITH_CACHE_PROBE_DIR")))
    (when directory
      (let ((path (merge-pathnames name (uiop:ensure-directory-pathname directory))))
        (ensure-directories-exist path)
        (with-open-file (stream path :direction :output :if-exists :supersede)
          (if (stringp value)
              (write-string value stream)
              (let ((*print-readably* t) (*print-pretty* nil))
                (write value :stream stream)))))))
  nil)

(-> cache-hourly--capture (agent application string &key (:response-call (option hash-table))) list)
(defun cache-hourly--capture (agent application label &key response-call)
  "Run the controller through final admission and actual Codex UTF-8 encoding.

Replace only HTTP. Optionally return one inert tool-call SSE response, then
capture the real controller's next request after real lookup and tool dispatch."
  (let* ((conversation (agent-conversation agent))
         (goal         (application-goal-context application))
         (captured     nil))
    (test-call-with-function-replacements
     (list (list 'provider-post-event-stream
                 (lambda (url body &rest options)
                   (declare (ignore url options))
                   (let* ((text    (babel:octets-to-string body :encoding :utf-8))
                          (items   (cache-hourly--input-slices text))
                          (name    (if response-call (concatenate 'string label "-before") label))
                          (history (mapcar #'json-encode
                                           (conversation-input-items-for-family
                                            conversation (provider-family (agent-provider agent))))))
                     (let* ((array (format nil "[~{~A~^,~}]" items))
                            (start (search array text)))
                       (test-assert start "every unfiltered exact item slice reconstructs the final encoded input array")
                       (setf captured (list :label name :body text :items items :goal goal :history history
                                            :envelope (list (subseq text 0 start)
                                                            (subseq text (+ start (length array)))))))
                     (cache-hourly--write (format nil "~A.body.json" name) text)
                     (cache-hourly--write (format nil "~A.items.sexp" name) items)
                     (cache-hourly--write (format nil "~A.goal.txt" name) goal)
                     (cache-hourly--write (format nil "~A.history.sexp" name) history)
                     (cache-hourly--write (format nil "~A.envelope.sexp" name) (getf captured :envelope))
                     (if response-call
                         (let* ((call   response-call)
                                (source (concatenate
                                         'string
                                         (test-sse-event-string
                                          (json-object "type" "response.output_item.done" "item" call))
                                         (test-sse-event-string
                                          (json-object "type" "response.completed"
                                                       "response" (json-object "id" name "output" (vector call)
                                                                               "usage" (json-object "input_tokens" 1 "output_tokens" 1)))))))
                           (setf response-call nil)
                           (values (make-instance 'test-character-input-stream :source source) 200 nil))
                         (throw 'cache-hourly-captured t))))))
     (lambda ()
       (catch 'cache-hourly-captured
         (agent--run-provider-loop agent (make-instance 'agent-observer) :goal-context goal))))
    (test-assert captured "the real encoded transport edge was reached")
    captured))

(-> cache-hourly--compare (list list) list)
(defun cache-hourly--compare (left right)
  "Report all-item and complete-body first differences, retaining mutable tails."
  (let* ((a (getf left :items)) (b (getf right :items))
         (index  (or (mismatch a b :test #'string=) (min (length a) (length b))))
         (before (nth index a)) (after (nth index b))
         (result (list :from (getf left :label) :to (getf right :label)
                       :item-counts   (list (length a) (length b))
                       :first-item    index
                       :left-role     (and before (json-get (json-decode before) "role"))
                       :right-role    (and after (json-get (json-decode after) "role"))
                       :body-lcp      (cache-hourly--lcp
                                       (babel:string-to-octets (getf left :body) :encoding :utf-8)
                                       (babel:string-to-octets (getf right :body) :encoding :utf-8))
                       :all-items-lcp (cache-hourly--lcp
                                       (babel:string-to-octets (format nil "~{~A~}" a) :encoding :utf-8)
                                       (babel:string-to-octets (format nil "~{~A~}" b) :encoding :utf-8))
                       :goal-equal    (equal (getf left :goal) (getf right :goal)))))
    (format t "~&CACHE-HOURLY ~S~%" result)
    result))

(-> cache-hourly--assistant (string) hash-table)
(defun cache-hourly--assistant (text)
  "Make a synthetic durable Responses assistant output item."
  (json-object "type" "message" "role" "assistant"
               "content" (vector (json-object "type" "output_text" "text" text))))

(-> cache-hourly--protocol-group (list string) list)
(defun cache-hourly--protocol-group (snapshot call-id)
  "Read one genuine completed call/output pair from unfiltered final wire items."
  (let* ((items  (getf snapshot :items))
         (index  (position-if
                  (lambda (item)
                    (let ((object (json-decode item)))
                      (and (equal "function_call" (json-get object "type"))
                           (equal call-id (json-get object "call_id")))))
                  items))
         (output (and index (nth (1+ index) items))))
    (test-assert (and output
                      (equal "function_call_output" (json-get (json-decode output) "type"))
                      (equal call-id (json-get (json-decode output) "call_id")))
                 "the genuine final wire preserves the known complete protocol group")
    (when output (subseq items index (+ index 2)))))

(-> cache-hourly--developer-texts (list) list)
(defun cache-hourly--developer-texts (snapshot)
  "Inspect current owners without using a filtered list for prefix acceptance."
  (loop for item in (getf snapshot :items)
        for object = (json-decode item)
        when (equal "developer" (json-get object "role"))
          append (loop for part across (json-get object "content")
                       collect (json-get part "text"))))

(-> cache-hourly--active-round
    (agent application &key (:job task-job) (:number integer)) list)
(defun cache-hourly--active-round (agent application &key job number)
  "Change three real current-state owners, then dispatch another real steering tool."
  (let* ((configuration (agent-configuration agent))
         (goal          (application-goal application))
         (state         (agenda-load configuration))
         (item          (first (workspace-agenda-items (agenda-current configuration state))))
         (label         (format nil "Active~D" number)))
    (test-assert (eq ':running (job-state job)) "the same owned planner remains active")
    (plan-update configuration
                 (list (list :text (format nil "Active planner plan round ~D" number) :status "doing")))
    (agenda-update configuration state (agenda-item-identifier item)
                   :text (format nil "Active planner agenda round ~D" number))
    (setf (getf (getf goal :accounting) :requests) (+ 2 number)
          (getf goal :evidence) (list (format nil "Active planner receipt round ~D" number)))
    (prog1
        (cache-hourly--capture
         agent application label
         :response-call (agent-test-call
                         :call-id (format nil "hourly-active-~D" number) :namespace "job" :name "send"
                         :arguments (json-encode
                                     (json-object "id" (job-identifier job)
                                                  "message" (format nil "Owned active planner steering ~D" number)))))
      (test-assert (task-job-take-steering job) "the additional real steering reached the active planner")
      (test-assert (eq ':running (job-state job)) "steering did not complete or replace the planner"))))

(-> cache-hourly--assert-active-frontier
    (list list &key (:prefix list) (:previous-frontier integer)
                   (:previous-call string) (:current-call string) (:number integer)) list)
(defun cache-hourly--assert-active-frontier
    (previous current &key prefix previous-frontier previous-call current-call number)
  "Require advancing exact durable prefixes with a fixture-owned protocol oracle.

PREFIX consists of the original durable messages and completed groups captured
in earlier real requests. No placement helper, anchor index or filtered prefix
supplies this oracle. Only the two newest known call/output groups may follow
current owners; a rewind before the previous durable frontier fails."
  (let* ((items       (getf current :items))
         (frontier    (length prefix))
         (texts       (cache-hourly--developer-texts current))
         (comparison  (cache-hourly--compare previous current))
         (first-owner (position-if
                       (lambda (item) (equal "developer" (json-get (json-decode item) "role")))
                       items))
         (old-group   (cache-hourly--protocol-group current previous-call))
         (new-group   (cache-hourly--protocol-group current current-call))
         (body-bound  (length
                       (babel:string-to-octets
                        (format nil "~A[~{~A~^,~}"
                                (first (getf previous :envelope))
                                (subseq (getf previous :items) 0 previous-frontier))
                        :encoding :utf-8))))
    (test-assert (equal old-group (cache-hourly--protocol-group previous previous-call))
                 "the previous newest complete group also retains its exact final wire bytes")
    (test-assert (and (> frontier previous-frontier)
                      (equal prefix (subseq items 0 (min frontier (length items)))))
                 "older completed protocol groups become an advancing byte-stable leading prefix")
    (test-assert (eql first-owner frontier)
                 "state relocation is bounded to the two newest protocol groups, not an obsolete early owner")
    (test-assert (equal items
                        (append prefix (subseq items frontier (+ frontier (length texts))) old-group new-group))
                 "every unfiltered final item belongs to the exact older prefix, current owners, or genuine newest groups")
    (test-assert (and (= 4 (length texts))
                      (= 1 (count (getf current :goal) texts :test #'equal))
                      (= 1 (count-if (lambda (text) (search (format nil "Active planner plan round ~D" number) text)) texts))
                      (= 1 (count-if (lambda (text) (search (format nil "Active planner agenda round ~D" number) text)) texts))
                      (= 1 (count-if (lambda (text) (search "Outstanding asynchronous work" text)) texts)))
                 "goal, edited plan, edited agenda and active planner owner occur exactly once")
    (dolist (text (cache-hourly--developer-texts previous))
      (when (or (equal text (getf previous :goal))
                (search "<active_plan_state" text) (search "* Agenda" text))
        (test-assert (not (member text texts :test #'equal))
                     "superseded current-state versions are absent rather than accumulated")))
    (test-assert (equal (getf previous :history)
                        (subseq (getf current :history) 0 (length (getf previous :history))))
                 "all prior durable family-history bytes including roles, IDs and outputs are retained")
    (test-assert (>= (getf comparison :first-item) previous-frontier)
                 "the unfiltered byte mismatch never rewinds behind the prior durable frontier")
    (test-assert (>= (getf comparison :body-lcp) body-bound)
                 "complete UTF-8 wire bytes preserve the entire previous durable frontier")
    (test-assert (equal (getf previous :envelope) (getf current :envelope))
                 "the complete non-input wire envelope is byte-identical during active planner churn")
    (append comparison (list :previous-frontier previous-frontier :durable-frontier frontier
                             :body-rewind-bound body-bound :current-owner-count (length texts)
                             :previous-call previous-call :current-call current-call))))

(-> cache-hourly--task-flow (agent application) list)
(defun cache-hourly--task-flow (agent application)
  "Exercise real task.run/job.send dispatch and scheduler/completion projection.

The job-pool admission edge constructs registered jobs but queues no worker."
  (let* ((registry (agent-tool-registry agent))
         (runtime  (tool-registry-runtime-binding registry 'task-orchestrator))
         (job nil) (captures nil))
    (test-call-with-function-replacements
     (list
      (list 'job-pool-submit-batch
            (lambda (pool entries)
              (declare (ignore pool))
              (mapcar
               (lambda (entry)
                 (setf job
                       (task-tests--attach-job
                        runtime
                        (lambda (identifier index)
                          (apply #'make-instance 'task-job
                                 :pool                         (task-orchestrator-pool runtime)
                                 :identifier identifier :index index :name (getf entry :name)
                                 :payload nil :owner-identifiers (getf entry :owner-identifiers)
                                 :root-identifier              (getf entry :root-identifier)
                                 :body-function                (getf entry :function)
                                 :terminal-result-function     (getf entry :terminal-result-function)
                                 :maximum-runtime-milliseconds 0
                                 (getf entry :initargs)))
                        :name "planner-fixture"))
                 (with-lock-held ((cl-jobpond::job--lock job))
                   (setf (job-state job) ':running))
                 (task-job--set-progress-state job ':running)
                 (cache-hourly--write "planner-child-goal.txt"
                                      (task-child-goal-context job (agent-configuration agent)))
                 job)
               entries))))
     (lambda ()
       (push (cache-hourly--capture
              agent application "Task"
              :response-call (agent-test-call
                              :call-id "hourly-task" :namespace "task" :name "run"
                              :arguments "{\"agent\":\"task\",\"name\":\"planner-fixture\",\"task\":\"Plan the owned synthetic mission\",\"context\":\"Own inert planner fixture\",\"blocking\":false}"))
             captures)))
    (test-assert job "real task.run reached the inert pool-admission seam")
    (when job
      (push (cache-hourly--capture
             agent application "Send"
             :response-call (agent-test-call
                             :call-id "hourly-send" :namespace "job" :name "send"
                             :arguments (json-encode
                                         (json-object "id" (job-identifier job)
                                                      "message" "Refine the synthetic planner evidence"))))
            captures)
      (test-assert (task-job-take-steering job) "real job.send enqueued child steering")
      (let* ((send              (first captures))
             (task              (second captures))
             (history           (getf task :history))
             (task-index        (position-if
                                 (lambda (item) (equal "hourly-task" (json-get (json-decode item) "call_id")))
                                 history))
             (prefix            (subseq history 0 task-index))
             (previous          send)
             (previous-frontier (length prefix))
             (pending-call      "hourly-task")
             (previous-call     "hourly-send")
             (observations      nil))
        (test-assert (equal (getf task :items)
                            (subseq (getf send :items) 0 (length (getf task :items))))
                     "all fifteen Task items are the exact leading prefix of Send")
        (loop for number from 1 to 3
              for current-call = (format nil "hourly-active-~D" number)
              do (setf prefix (append prefix (cache-hourly--protocol-group previous pending-call)))
                 (let* ((current     (cache-hourly--active-round agent application :job job :number number))
                        (observation (cache-hourly--assert-active-frontier
                                      previous current :prefix prefix :previous-frontier previous-frontier
                                      :previous-call previous-call :current-call current-call :number number)))
                   (push observation observations)
                   (push current captures)
                   (setf previous current previous-frontier (length prefix)
                         pending-call previous-call previous-call current-call)))
        (setf observations (nreverse observations))
        (test-assert (and (< (getf (first observations) :body-lcp)
                             (getf (second observations) :body-lcp)
                             (getf (third observations) :body-lcp))
                          (< (getf (first observations) :all-items-lcp)
                             (getf (second observations) :all-items-lcp)
                             (getf (third observations) :all-items-lcp)))
                     "complete body and unfiltered item shared prefixes grow over repeated active-planner owner churn")
        (cache-hourly--write "active-frontiers.sexp" observations))
      (task-tests--publish-terminal job ':completed
                                    '(:status :success :response "Synthetic planner completion receipt"))
      (let ((*task-completion-coalescing-seconds* 0))
        (test-assert (task-completion-deliver agent) "real completion projection appended an owned receipt"))
      (push (cache-hourly--capture agent application "D") captures))
    (nreverse captures)))

(-> test-cache-hourly-mission-request-prefix-growth () null)
(defun test-cache-hourly-mission-request-prefix-growth ()
  "Compare exact controller requests while different current-state owners change."
  (with-test-configuration (base root)
    (let* ((configuration (configuration-copy base :working-directory root))
           (conversation  (conversation-create configuration :identifier "cache-hourly-fixture"))
           (provider      (make-instance 'codex-subscription-provider :configuration configuration
                                          :credential-manager                        (credential-manager-create configuration)))
           (registry      (task-augment-tool-registry (make-default-tool-registry :configuration configuration)))
           (runtime       (tool-registry-runtime-binding registry 'task-orchestrator))
           (agent         (agent-create :configuration configuration :conversation conversation
                                        :provider provider :tool-registry registry :worker ':unused))
           (application   (make-instance 'application :configuration configuration
                                         :conversation conversation :tool-registry registry :worker nil))
           (goal          (list :kind ':mission :status ':active :objective "Own synthetic hourly fixture"
                                :acceptance-policy "Require local evidence" :accounting (list :requests 0)
                                :evidence nil)))
      (unwind-protect
           (progn
             (setf (configuration-validation-deferred-p configuration) t
                   (config :model configuration) "gpt-5.1-codex"
                   (application-goal application) goal)
             (credential-source-save (credential-manager-primary-source
                                      (provider-credential-manager provider))
                                     (provider-tests--credentials configuration))
             (conversation-append-user-message conversation "Run the synthetic hourly mission.")
             (dotimes (index 5)
               (conversation-append-provider-item
                conversation (cache-hourly--assistant
                              (format nil "Stable fixture evidence ~D ~A" index (make-string 1000 :initial-element #\x)))))
             (plan-update configuration (list (list :text "Inspect synthetic evidence" :status "doing")))
             (agenda-add :configuration configuration :state (agenda-load configuration)
                         :text "Inspect synthetic hourly mission")
             (let ((warm (cache-hourly--capture agent application "Warm")))
               (conversation-append-provider-item conversation (cache-hourly--assistant "Warm append with no state mutation."))
               (let* ((a  (cache-hourly--capture agent application "A"))
                      (a0 (cache-hourly--capture agent application "A0")))
                 (test-assert (equal (getf a :body) (getf a0 :body))
                              "a same-history no-op has identical complete encoded bytes")
                 (test-assert (not (equal "developer"
                                          (json-get (json-decode (first (last (getf a :items)))) "role")))
                              "a warm Codex append ends with durable history rather than unchanged developer state")
                 (conversation-append-provider-item conversation (cache-hourly--assistant "First newly completed work."))
                 (plan-update configuration (list (list :text "Review synthetic evidence" :status "doing")))
                 (let ((b (cache-hourly--capture agent application "B")))
                   (conversation-append-provider-item conversation (cache-hourly--assistant "Second newly completed work."))
                   (setf (getf (getf goal :accounting) :requests) 2
                         (getf goal :evidence) '("Second local receipt"))
                   (let* ((c     (cache-hourly--capture agent application "C"))
                          (state (agenda-load configuration))
                          (item  (first (workspace-agenda-items (agenda-current configuration state)))))
                     (agenda-update configuration state (agenda-item-identifier item) :status ':doing)
                     (let* ((agenda (cache-hourly--capture agent application "Agenda"))
                            (tasks  (cache-hourly--task-flow agent application))
                            (wa     (cache-hourly--compare warm a))
                            (ab     (cache-hourly--compare a b))
                            (bc     (cache-hourly--compare b c))
                            (ca     (cache-hourly--compare c agenda)))
                       (let ((d0 (cache-hourly--capture agent application "D0")))
                         (test-assert (equal (getf (first (last tasks)) :body) (getf d0 :body))
                                      "the completed-worker same-history repeat has identical complete bytes"))
                       (test-assert (equal (getf b :history) (butlast (getf c :history)))
                                    "goal mutation does not rerender or rewrite the prior durable family history")
                       (test-assert (equal (getf c :history) (getf agenda :history))
                                    "agenda mutation leaves every durable projected history byte identical")
                       (test-assert
                        (some (lambda (item)
                                (let ((message (json-decode item)))
                                  (and (equal "developer" (json-get message "role"))
                                       (find (getf c :goal) (coerce (json-get message "content") 'list)
                                             :key (lambda (part) (json-get part "text")) :test #'equal))))
                              (getf c :items))
                        "the final request contains the current regenerated mission state")
                       (cache-hourly--write "observations.sexp"
                                            (append (list wa ab bc ca)
                                                    (loop for left in (cons agenda tasks)
                                                          for right in tasks
                                                          collect (cache-hourly--compare left right))))
                       (test-assert (not (equal (getf b :goal) (getf c :goal)))
                                    "fresh application mission accounting is rendered")
                       (test-assert (> (getf bc :all-items-lcp) (getf ab :all-items-lcp))
                                    "the unfiltered exact-byte shared prefix grows after further durable append")))))))
        (task-orchestrator-close runtime))))
  nil)

(-> test-cache-current-state-group-lifecycle () null)
(defun test-cache-current-state-group-lifecycle ()
  "Close changed state over tool groups without retaining superseded snapshots."
  (with-test-configuration (configuration)
    (let* ((*context-contributors*        nil)
           (*context-resolver*            (cl-llm-provider-api:make-context-resolver))
           (*context-last-deliveries*     (make-hash-table :test #'equal))
           (*context-last-delivery-order* nil)
           (conversation                  (conversation-create configuration))
           (provider                      (provider-create configuration))
           (instruction                   "fixture-current-v0")
           (goal                          "fixture-goal-v0")
           (prewire                       nil)
           (capture-number                0)
           (project                       (symbol-function 'context-delivery-project-items)))
      (setf (configuration-validation-deferred-p configuration) t)
      (register-context-contributor
       "cache-group-lifecycle"
       (lambda (context)
         (declare (ignore context))
         (when instruction
           (make-context-contribution :identifier  "current"
                                      :instruction instruction
                                      :class       ':mandatory
                                      :lifetime    ':turn))))
      (conversation-append-user-message conversation "owned lifecycle fixture")
      (test-call-with-function-replacements
       (list (list 'context-delivery-project-items
                   (lambda (&rest arguments)
                     (let ((results (multiple-value-list (apply project arguments))))
                       (setf prewire (mapcar #'json-encode (first results)))
                       (values-list results)))))
       (lambda ()
         (labels ((request ()
                    "Capture real placement separately from final Responses serialization."
                    (let* ((request (provider-request-object provider conversation #() :goal-context goal))
                           (items   (coerce (json-get request "input") 'list)))
                      (test-assert (equal prewire (mapcar #'json-encode items))
                                   "all pre-wire placement bytes equal the unfiltered final Codex input")
                     (incf capture-number)
                     (cache-hourly--write (format nil "lifecycle-~D.prewire.sexp" capture-number) prewire)
                     (cache-hourly--write (format nil "lifecycle-~D.body.json" capture-number)
                                          (babel:octets-to-string (json-encode-utf8 request) :encoding :utf-8))
                     request))

                  (occurrences (text items)
                    "Count context-bearing items without filtering the sent input."
                    (count-if (lambda (item) (search text (json-encode item))) items)))
           (request)
           (test-assert
            (zerop
             (context--current-state-index
              (list (first (conversation-input-items conversation))
                    (json-object "type" "function_call" "name" "sample"
                                 "call_id" "repaired" "arguments" "{}")
                    (function-call-output-item "repaired" *conversation-interrupted-tool-output*)
                    (function-call-output-item "repaired" "actual late result"))
              :turn-start 0))
            "boundary closure preserves the existing replaceable interrupted-output policy")
           (dotimes (round 3)
             (let ((reasoning (compaction-tail-tests--reasoning "owned private context")))
               (conversation-append-provider-item conversation reasoning)
               (dolist (suffix '("a" "b"))
                 (conversation-append-provider-item
                  conversation (json-object "type" "function_call" "name" "sample"
                                            "call_id" (format nil "group-~D-~A" round suffix)
                                            "arguments" "{}")))
               (dolist (suffix '("a" "b"))
                 (conversation-append-provider-item
                  conversation (json-object "type" "function_call_output"
                                            "call_id" (format nil "group-~D-~A" round suffix)
                                            "output" "owned result")))
               (setf instruction (format nil "fixture-current-v~D" (1+ round))
                     goal        (format nil "fixture-goal-v~D" (1+ round)))
               (let* ((current  (request))
                      (items    (coerce (json-get current "input") 'list))
                      (boundary (position (json-encode reasoning) prewire :test #'equal :from-end t))
                      (state    (position-if (lambda (item) (search goal (json-encode item))) items)))
                 (test-assert (and boundary state (< state boundary)
                                   (= 1 (occurrences goal items))
                                   (= 1 (occurrences instruction items))
                                   (equal "function_call_output" (json-get (first (last items)) "type")))
                              "fresh state precedes the whole parallel call/reasoning group exactly once")
                 (test-assert (= 2 (length (context-anchors-current conversation)))
                              "repeated state versions retain bounded anchor metadata, not old snapshots")
                 (test-assert (equalp (json-encode-utf8 current) (json-encode-utf8 (request)))
                              "an unchanged tool-round request retains every encoded byte")
                 (setf conversation (conversation-load (conversation-pathname conversation)))
                 (test-assert (equalp (json-encode-utf8 current) (json-encode-utf8 (request)))
                              "resume reloads the group boundary and reproduces exact request bytes"))))
           (setf instruction nil)
           (let ((partial (json-get (request) "input")))
             (test-assert (and (= 1 (occurrences goal partial))
                               (zerop (occurrences "fixture-current-" partial)))
                          "removing one owner retains only the current surviving goal"))
           (setf goal nil)
           (test-assert (and (zerop (occurrences "fixture-goal-" (json-get (request) "input")))
                             (null (context-anchors-current conversation)))
                        "removing every owner drops all group messages and metadata")
           (conversation-append-summary conversation "owned genuine compaction handoff")
           (conversation-append-user-message conversation "new owned request epoch")
           (setf goal "fixture-goal-after-compaction" instruction "fixture-current-after-compaction")
           (let ((items (json-get (request) "input")))
             (test-assert (and (= 1 (occurrences goal items))
                               (= 1 (occurrences instruction items))
                               (zerop (occurrences "owned result" items)))
                          "actual compaction and new-user epochs discard old groups and render current state"))
           (let* ((api     (make-instance 'openai-api-provider :configuration configuration))
                  (request (provider-request-object api conversation #() :goal-context goal))
                  (items   (json-get request "input")))
             (test-assert (and (= 1 (occurrences goal items))
                               (= 1 (occurrences instruction items))
                               (equal "developer" (json-get (aref items (1- (length items))) "role")))
                          "a family switch retains the generic API route's ordinary developer suffix"))
           (test-assert (= 1 (occurrences goal (json-get (request) "input")))
                        "returning to Codex reconstructs one fresh group after the family epoch"))))))
  nil)
