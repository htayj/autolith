(in-package #:autolith)

;;;; -- Continuity Ownership and Recovery Boundaries --

(-> task-continuity-tests--fixture (function) t)
(defun task-continuity-tests--fixture (function)
  "Run FUNCTION with a durable orphan specification and ordinary task registry."
  (with-test-configuration (configuration root)
    (let* ((configuration (configuration-copy configuration :working-directory root))
           (registry      (task-augment-tool-registry
                           (make-default-tool-registry :configuration configuration)))
           (provider      (make-instance 'task-test-provider :mode ':concurrent))
           (parent        (agent-create :configuration configuration :provider provider
                                        :tool-registry registry :worker nil))
           (run-tool      (tool-registry-find registry "task" "run"))
           (orchestrator  (task-run-tool-orchestrator run-tool))
           (definition    (task-agent-definition-create
                           :name "task" :description "continuity fixture"
                           :instructions "Return the result through yield.submit." :tools ':all))
           (job           (task-tests--make-job
                           orchestrator :identifier "orphan" :parent-agent parent
                           :definition definition :detached-p t
                           :item (list :agent "task" :task "Return the result." :async t)))
           (context       (make-instance 'tool-context :configuration configuration
                                         :agent parent :registry registry :worker nil
                                         :conversation                (agent-conversation parent)
                                         :call-id                     "continuity-test"))
           (path          (task-continuity-record-job job parent)))
      (unless (tool-registry-find registry "job" "continuity")
        (tool-registry-register
         registry (make-instance 'task-continuity-tool :namespace "job" :name "continuity"
                                 :description  "Test continuity decisions."
                                 :parameters   (task-continuity-parameters)
                                 :orchestrator orchestrator)))
      (unwind-protect
           (funcall function parent orchestrator context job path)
        (task-orchestrator-close orchestrator)))))

(-> task-continuity-tests--refused-p (function &key (:code (option keyword))) boolean)
(defun task-continuity-tests--refused-p (function &key code)
  "Return true when FUNCTION signals a typed continuity refusal with optional CODE."
  (handler-case (progn (funcall function) nil)
    (task-continuity-error (condition)
      (or (null code) (eq code (tool-failure-code condition))))))

(-> test-task-continuity-classification () null)
(defun test-task-continuity-classification ()
  "Classify lost work, safe declarations, missing checkpoints and abandonment."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator context job path)
     (declare (ignore context))
     (let ((execution (session-job-execution-identifier job)))
       (test-assert
        (eq :dead-needing-decision
            (getf (first (task-continuity-inventory parent orchestrator)) :classification))
        "An opaque unfinished task needs a decision")
       (task-continuity-declare parent orchestrator execution
                                :safety "Read-only test fixture; no external effects.")
       (test-assert
        (eq :safe-restartable-specification
            (getf (first (task-continuity-inventory parent orchestrator)) :classification))
        "An explicit safe specification survives a reread")
       (test-assert
        (equal execution (getf (task-continuity--read path) :execution-id))
        "Durable identity links the original execution")
       (test-assert
        (task-continuity-tests--refused-p
         (lambda () (task-continuity-declare parent orchestrator execution
                                             :image "missing-image" :repl "recovery")))
        "Missing worker images cannot become recovery evidence")
       (task-continuity-abandon parent orchestrator execution)
       (test-assert
        (eq :abandoned
            (getf (first (task-continuity-inventory parent orchestrator)) :classification))
        "Abandonment persists explicitly")
       (let ((fresh (task-orchestrator-create)))
         (unwind-protect
              (test-assert
               (eq :abandoned
                   (getf (first (task-continuity-inventory parent fresh)) :classification))
               "Classification survives a new runtime")
           (task-orchestrator-close fresh))))))
  (task-continuity-tests--paging)
  nil)

(-> test-task-continuity-revive () null)
(defun test-task-continuity-revive ()
  "Revive a stored task through actual child execution once, with explicit authority."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator context job path)
     (let ((execution (session-job-execution-identifier job)))
       (task-continuity-declare parent orchestrator execution
                                :safety "Deterministic read-only provider fixture.")
       (test-assert
        (task-continuity-tests--refused-p
         (lambda () (task-continuity-revive context orchestrator execution)) :code ':authority)
        "Replay requires a fresh explicit authorization")
       (test-assert
        (not (probe-file (merge-pathnames "revive.sexp"
                                          (uiop:pathname-directory-pathname path))))
        "An unauthorized attempt does not claim replay")
       (let* ((result  (task-continuity-revive context orchestrator execution :authorized-p t))
              (revived (first (task-orchestrator-list-jobs orchestrator))))
         (test-assert (tool-result-success-p result) "The ordinary task.run pathway admits recovery")
         (test-assert revived "Recovery creates a real supervised child")
         (multiple-value-bind (snapshot terminal-p) (task-job-await revived 10)
           (test-assert (and terminal-p (eq :success (getf (getf snapshot :result) :status)))
                        (format nil "Recovered child yields successfully: ~S" snapshot)))
         (test-assert
          (not (equal execution (session-job-execution-identifier revived)))
          "A restarted execution receives a fresh identity")
         (let* ((row      (task-continuity-classify
                           (task-continuity--find parent execution) parent orchestrator))
                (recovery (getf row :recovery-result))
                (link     (first (getf recovery :new-executions))))
           (test-assert (and (eq :restart (getf row :decision))
                             (eq :dispatched (getf row :recovery-outcome)))
                        "Inventory exposes the durable recovery decision and dispatch outcome")
           (test-assert (and (equal execution (getf recovery :execution-id))
                             (equal (session-job-execution-identifier revived)
                                    (getf link :execution-id))
                             (equal (session-job-parent-call-id revived)
                                    (getf recovery :dispatch-call-id)))
                        "Recovery links original execution, correlated dispatch and new execution")
           (let ((fresh (task-orchestrator-create)))
             (unwind-protect
                  (test-assert
                   (equal recovery
                          (getf (task-continuity-classify
                                 (task-continuity--find parent execution) parent fresh)
                                :recovery-result))
                   "Recovery linkage survives without the original live job pool")
               (task-orchestrator-close fresh))))
         (test-assert
          (task-continuity-tests--refused-p
           (lambda () (task-continuity-revive context orchestrator execution :authorized-p t)))
          "A consumed claim prevents duplicate side effects")
         (test-assert (= 1 (length (task-orchestrator-list-jobs orchestrator)))
                      "Duplicate recovery admits no additional child")))))
  nil)

(-> test-task-continuity-ownership () null)
(defun test-task-continuity-ownership ()
  "Reject foreign owners and child replay authority; preserve owned live attachment."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator context job path)
     (declare (ignore path))
     (let* ((configuration (agent-configuration parent))
            (other         (agent-create :configuration configuration
                                         :provider      (make-instance 'model-provider)
                                         :tool-registry (agent-tool-registry parent) :worker nil))
            (child         (task-tests--child-viewer configuration job))
            (execution     (session-job-execution-identifier job)))
       (test-assert (null (task-continuity-records other)) "Another conversation has no durable ownership")
       (test-assert (null (task-continuity-records child)) "A child cannot inspect its parent or siblings")
       (test-assert
        (task-continuity-tests--refused-p
         (lambda () (task-continuity-declare child orchestrator execution :safety "read-only")) :code ':authority)
        "Children cannot grant replay authority")
       (test-assert
        (task-continuity-tests--refused-p
         (lambda () (task-continuity-abandon child orchestrator execution)) :code ':authority)
        "Child abandonment is an authority refusal")
       (let ((*worker-host-tool-policy* (list :restricted-p t :allowlist nil)))
         (test-assert
          (task-continuity-tests--refused-p
           (lambda () (task-continuity-dispatch context "resource" "read"
                                                :arguments (json-object "uri" "workspace:file"))) :code ':authority)
          "A withheld recovery capability is an authority refusal"))
       (with-lock-held ((cl-jobpond::job-pool--lock (task-orchestrator-pool orchestrator)))
         (setf (gethash (job-identifier job)
                        (cl-jobpond::job-pool--jobs (task-orchestrator-pool orchestrator))) job))
       (test-assert
        (eq :live-owned-detached
            (getf (first (task-continuity-inventory parent orchestrator)) :classification))
        "Actual live ownership dominates orphan classification")
       (test-assert
        (tool-result-success-p
         (task-continuity-revive context orchestrator execution :authorized-p t))
        "Owned attachment returns ordinary job.get rather than replaying")
       (test-assert (= 1 (length (task-orchestrator-list-jobs orchestrator)))
                    "Reattachment does not duplicate execution"))))
  nil)

(-> task-continuity-tests--paging () null)
(defun task-continuity-tests--paging ()
  "Exercise whole-form pagination and bounded persisted metadata through the registry."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator context job path)
     (declare (ignore job))
     (let* ((record (task-continuity--read path))
            (root   (uiop:pathname-parent-directory-pathname
                     (uiop:pathname-directory-pathname path)))
            (body   (make-string 20000 :initial-element #\x)))
       (loop for index below 12
             for execution = (format nil "page-~2,'0D" index)
             for target = (merge-pathnames (format nil "~A/continuity.sexp" execution) root)
             for copy = (copy-tree record)
             do (setf (getf copy :execution-id) execution
                      (getf copy :job-id) execution
                      (getf (getf copy :specification) :task) body)
                (ensure-directories-exist target)
                (task-continuity--write target copy))
       (task-continuity--write (merge-pathnames "result.sexp" (uiop:pathname-directory-pathname path))
                       (list :status ':success :output body :structured-output body
                             :duration-ms 321 :request-count 7
                             :usage                  '(("input_tokens" 42))
                             :worktree-artifact-path "patch.sexp"))
       (let* ((row    (task-continuity-classify (task-continuity--find parent "orphan")
                                                parent orchestrator))
              (result (getf row :result)))
         (test-assert (and (= 321 (getf result :duration-ms))
                           (= 7 (getf result :request-count))
                           (equal '(("input_tokens" 42)) (getf result :usage))
                           (equal "patch.sexp" (getf result :worktree-artifact-path)))
                      "Inventory retains reported timing, usage and artifact metadata")
         (test-assert (and (null (getf result :output))
                           (null (getf result :structured-output))
                           (< (length (task--write-readable-sexp row)) 6000))
                      "Inventory omits full terminal result bodies"))
       (let ((*task-tool-content-limit* 8000)
             (offset                    0)
             (seen                      nil))
         (loop
           (let* ((response    (task-continuity-dispatch
                                context "job" "continuity"
                                :arguments (json-object "action" "list" "offset" offset "limit" 100)))
                  (content     (tool-result-content response))
                  (page        (tool-result-details response))
                  (*read-eval* nil))
             (test-assert (tool-result-success-p response) "Paged inventory succeeds at the registry boundary")
             (with-input-from-string (stream content)
               (test-assert (and (equal page (read stream)) (eq :eof (read stream nil :eof)))
                            "Every native page is exactly one complete readable form"))
             (test-assert (<= (length content) 8000) "Native page respects the character budget")
             (setf seen (append seen (mapcar (lambda (row) (getf row :execution-id))
                                            (getf page :entries))))
             (if (getf page :more-p)
                 (progn
                   (test-assert (> (getf page :next-offset) offset) "Pagination makes progress")
                   (setf offset (getf page :next-offset)))
                 (return))))
         (test-assert (and (= 13 (length seen))
                           (= 13 (length (remove-duplicates seen :test #'equal))))
                      "Character-bounded pages enumerate each durable execution once")
         (let ((page (task-continuity-page parent orchestrator :offset 999)))
           (test-assert (and (null (getf page :entries)) (= 13 (getf page :next-offset))
                             (not (getf page :more-p)))
                        "Beyond-end pages return a complete empty result at EOF"))
         (test-assert (= 1 (length (getf (task-continuity-page parent orchestrator :limit 1) :entries)))
                      "The requested count also bounds each page")
         (dolist (arguments '((:offset -1) (:limit 0) (:limit 101)))
           (test-assert
            (task-continuity-tests--refused-p
             (lambda () (apply #'task-continuity-page parent orchestrator arguments)))
            "Invalid page bounds fail before enumeration"))))))
  nil)


;;;; -- Producer-Owned Overflow Contexts --

(defclass child-overflow-test-tool (tool) ()
  (:documentation "Return caller-selected text through the real result spill path."))

(defmethod tool-child-safe-p ((tool child-overflow-test-tool))
  "Admit the deterministic producer through the actual child tool policy."
  (declare (ignore tool))
  t)

(defmethod tool-execute ((tool child-overflow-test-tool)
                         (context tool-context) (arguments hash-table))
  "Emit the complete test payload using ordinary tool result bounding."
  (declare (ignore tool context))
  (tool-success (gethash "text" arguments)))

(-> child-overflow-tests--call (tool-context string string &key (:arguments hash-table)) tool-result)
(defun child-overflow-tests--call (context namespace name &key (arguments (json-object)))
  "Dispatch a real tool call with CONTEXT's authority."
  (tool-registry-execute-call
   (tool-context-registry context)
   (json-object "namespace" namespace "name" name "arguments" (json-encode arguments)) context))

(-> child-overflow-tests--emit (tool-context string) string)
(defun child-overflow-tests--emit (context text)
  "Run an oversized producer and extract its emitted resource URI."
  (let* ((result  (child-overflow-tests--call context "test" "overflow-owned"
                                               :arguments (json-object "text" text)))
         (content (tool-result-content result))
         (start   (search "context:" content)))
    (test-assert (and (tool-result-success-p result) start (< (length content) (length text)))
                 "An actual oversized child tool result emits a bounded context reference")
    (assert start)
    (subseq content start (+ start 72))))

(-> child-overflow-tests--read (tool-context string) tool-result)
(defun child-overflow-tests--read (context uri)
  "Read the payload tail through the public resource tool boundary."
  (child-overflow-tests--call context "resource" "read"
                              :arguments (json-object "uri" uri "start-line" 695 "line-count" 10)))

(-> test-child-overflow-context-ownership () null)
(defun test-child-overflow-context-ownership ()
  "Authorize actual producer spills, reject other owners, and reconstruct durable grants."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator primary job path)
     (let* ((configuration (agent-configuration parent))
            (registry      (tool-context-registry primary))
            (sibling       (task-tests--make-job orchestrator :identifier "sibling"
                                                 :parent-agent parent :definition (task-job-definition job)
                                                 :item                    (list :agent "task" :task "Sibling")))
            (sibling-path  (task-continuity-record-job sibling parent))
            (text          (tool-tests--overflow-text)))
       (tool-registry-register
        registry (make-instance 'child-overflow-test-tool :namespace "test" :name "overflow-owned"
                                 :description "Emit a large test payload."
                                 :parameters  (json-object "type" "object" "required" '("text")
                                                           "properties" (json-object "text" (json-object "type" "string"))
                                                           "additionalProperties" (json-false))))
       (labels ((child-context (owner)
                  (let* ((child-tools (task-child-tool-registry registry (task-job-definition owner)
                                                               :orchestrator (session-job-orchestrator owner)
                                                               :depth        1))
                         (child       (task-tests--child-viewer configuration owner :registry child-tools)))
                    (make-instance 'tool-context :configuration configuration :agent child
                                   :registry child-tools :worker nil :conversation (agent-conversation child))))

                (readable-p (context uri)
                  (let ((result (child-overflow-tests--read context uri)))
                    (unless (tool-result-success-p result)
                      (format t "Overflow read denied: ~S ~A~%" (tool-result-error-code result)
                              (tool-result-content result)))
                    (and (tool-result-success-p result)
                         (search "overflow line 700" (tool-result-content result)) t)))

                (denied-p (context uri)
                  (let ((result (child-overflow-tests--read context uri)))
                    (and (not (tool-result-success-p result))
                         (eq ':access-denied (tool-result-error-code result))))))
         (let* ((producer (child-context job))
                (other    (child-context sibling))
                (uri      (child-overflow-tests--emit producer text)))
           (test-assert (readable-p producer uri) "The producing child can read its actual emitted tail")
           (test-assert (denied-p other uri) "A sibling knowing the emitted URI has no grant")
           (let* ((nested-parent (tool-context-agent producer))
                  (nested-job    (task-tests--make-job orchestrator :identifier "nested"
                                                       :parent-agent            nested-parent
                                                       :definition              (task-job-definition job)
                                                       :root-identifier         (session-job-root-conversation-identifier job)
                                                       :owner-identifiers       (list (job-identifier job))
                                                       :item                    (list :agent "task" :task "Nested"))))
             (task-continuity-record-job nested-job nested-parent)
             (test-assert (denied-p (child-context nested-job) uri)
                          "A nested child does not inherit its producing parent's grants"))
           (dolist (forbidden (list "context:index" (format nil "context:~64,'0X" 0)
                                   (format nil "context:~A" (rlm-context-object-digest
                                                              (rlm-context-intern configuration "foreign payload")))))
             (test-assert (denied-p producer forbidden) "Children cannot read an index, fabricated or foreign digest"))
           (let ((parent-uri (child-overflow-tests--emit primary (concatenate 'string text "parent"))))
             (test-assert (denied-p producer parent-uri) "Parent emission conveys no child grant")
             (test-assert (readable-p primary parent-uri) "The primary can read its own spill"))
           (let ((other-uri (child-overflow-tests--emit other (concatenate 'string text "sibling"))))
             (test-assert (denied-p producer other-uri) "Another child's emitted digest is denied"))
           (test-assert (string= uri (child-overflow-tests--emit other text)) "Identical payloads deduplicate")
           (test-assert (readable-p other uri) "Deduplicated content becomes readable only after own emission")
           (test-assert (readable-p primary uri) "Primary reads retain ordinary context access")
           (let ((*resource-readable-schemes* '("workspace")))
             (test-assert (denied-p producer uri) "An explicit view scheme restriction still denies own contexts"))
           (let ((gate    (make-lock "Concurrent overflow emission"))
                 (threads nil))
             (with-lock-held (gate)
               (dolist (suffix '("concurrent-a" "concurrent-b"))
                 (let ((payload (concatenate 'string text suffix)))
                   (push (make-thread
                          (lambda ()
                            (with-lock-held (gate) nil)
                            (child-overflow-tests--call producer "test" "overflow-owned"
                                                        :arguments (json-object "text" payload)))) threads))))
             (dolist (thread threads)
               (let* ((result  (join-thread thread))
                      (content (tool-result-content result))
                      (start   (search "context:" content)))
                 (test-assert (and (tool-result-success-p result) start)
                              "Concurrent real tool emissions each produce a context reference")
                 (when start
                   (test-assert (readable-p producer (subseq content start (+ start 72)))
                                "Concurrent emissions retain both durable grants")))))
           (test-assert (tool-result-success-p
                         (child-overflow-tests--call primary "resource" "read"
                                                     :arguments (json-object "uri" "context:index")))
                        "The primary retains index access")
           (task-continuity-declare parent orchestrator (session-job-execution-identifier job)
                                    :safety "Deterministic overflow fixture.")
           (test-assert (readable-p producer uri) "Recovery declarations retain producer grants")
           (let* ((target      (format nil "workspace:~A" (namestring path)))
                  (observation (child-overflow-tests--call producer "resource" "read"
                                                          :arguments (json-object "uri" target)))
                  (content     (tool-result-content observation))
                  (marker      "Revision: ")
                  (start       (+ (or (search marker content) (error "Missing edit observation")) (length marker)))
                  (revision    (subseq content start (position #\Newline content :start start)))
                  (edit        (child-overflow-tests--call producer "resource" "edit"
                                                          :arguments
                                                          (json-object "uri" target "base-revision" revision
                                                                       "operations" (vector (json-object "op" "replace-lines"
                                                                                                            "start-line" 1 "end-line" 1
                                                                                                            "content" "(:overflow-context-digests (fabricated))"))))))
             (test-assert (and (not (tool-result-success-p edit))
                               (eq ':access-denied (tool-result-error-code edit)))
                          "Even an observed in-workspace owner record cannot be edited to grant access"))
           (let ((record         (task-continuity--read path))
                 (sibling-record (task-continuity--read sibling-path))
                 (fresh          (task-orchestrator-create)))
             (unwind-protect
                  (let ((reconstructed
                          (make-instance 'task-job :pool  (task-orchestrator-pool fresh)
                                         :identifier (job-identifier job) :index 1 :name "restored"
                                         :payload nil :owner-identifiers (job-owner-identifiers job)
                                         :root-identifier (session-job-root-conversation-identifier job)
                                         :body-function #'task-job--run :maximum-runtime-milliseconds 0
                                         :orchestrator fresh :execution-identifier (session-job-execution-identifier job)
                                         :definition (task-job-definition job) :item (task-job-item job) :parent-agent parent)))
                    (test-assert (readable-p (child-context reconstructed) uri)
                                 "A reconstructed same-owner job rereads its durable emitted grant")
                    (task-continuity--write sibling-path record)
                    (test-assert (denied-p other uri) "Replaying another owner's durable record grants nothing")
                    (task-continuity--write sibling-path sibling-record)
                    (remf record :overflow-context-digests)
                    (task-continuity--write path record)
                    (test-assert (denied-p (child-context reconstructed) uri)
                                 "Legacy records without grants default closed"))
               (task-orchestrator-close fresh))))))))
  nil)

(-> test-child-overflow-continuity-capacity () null)
(defun test-child-overflow-continuity-capacity ()
  "Refuse a real spill before UTF-8 growth can destroy earlier durable grants."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator primary job path)
     (let* ((configuration (agent-configuration parent))
            (registry      (tool-context-registry primary))
            (text          (tool-tests--overflow-text)))
       (tool-registry-register
        registry (make-instance 'child-overflow-test-tool :namespace "test" :name "overflow-owned"
                                 :description "Emit a large test payload."
                                 :parameters  (json-object "type" "object" "required" '("text")
                                                           "properties" (json-object "text" (json-object "type" "string"))
                                                           "additionalProperties" (json-false))))
       (let* ((child-tools (task-child-tool-registry registry (task-job-definition job)
                                                    :orchestrator orchestrator :depth 1))
              (child       (task-tests--child-viewer configuration job :registry child-tools))
              (producer    (make-instance 'tool-context :configuration configuration :agent child
                                          :registry child-tools :worker nil :conversation (agent-conversation child)))
              (uri         (child-overflow-tests--emit producer text))
              (record      (task-continuity--read path)))
         ;; Seed trusted near-capacity state, not thousands of actual emissions.
         (setf (getf record :overflow-context-digests)
               (append (getf record :overflow-context-digests)
                       (loop for index below 15500 collect (format nil "~64,'0X" index)))
               (getf record :restart-safety) "Earlier recovery declaration."
               (getf record :capacity-padding) (make-string 128 :initial-element #\GREEK_SMALL_LETTER_LAMDA))
         (let* ((initial (format nil "~A~%" (task--write-readable-sexp record)))
                (padding (- *task-continuity-maximum-octets* 20
                            (length (utf8-string-to-octets initial)))))
           (assert (plusp padding))
           (setf (getf record :capacity-padding)
                 (concatenate 'string (getf record :capacity-padding)
                              (make-string padding :initial-element #\x))))
         (task-continuity--write path record)
         (let* ((before      (uiop:read-file-string path :external-format :utf-8))
                (next-text   (concatenate 'string text "capacity crossing"))
                (next-digest (rlm-context-object-digest (rlm-context-intern configuration next-text)))
                (prospective (copy-tree record)))
           (push next-digest (getf prospective :overflow-context-digests))
           (let ((serialized (format nil "~A~%" (task--write-readable-sexp prospective))))
             (test-assert (and (<= (length serialized) *task-continuity-maximum-octets*)
                               (> (length (utf8-string-to-octets serialized))
                                  *task-continuity-maximum-octets*))
                          "The next grant crosses the real octet bound but not a character bound"))
           (test-assert (equal record (task-continuity--read path))
                        "Seeded metadata is valid under the production bounded reader")
           (test-assert (tool-result-success-p (child-overflow-tests--read producer uri))
                        "Earlier actual emission is publicly readable before capacity rejection")
           (let ((result (child-overflow-tests--call producer "test" "overflow-owned"
                                                     :arguments (json-object "text" next-text))))
             (test-assert (and (tool-result-success-p result)
                               (not (search "context:" (tool-result-content result))))
                          "The bounded spill fallback returns no unusable resource URI"))
           (test-assert (string= before (uiop:read-file-string path :external-format :utf-8))
                        "Rejected grant publication preserves the exact previous UTF-8 file")
           (test-assert (handler-case (equal record (task-continuity--read path))
                          (error () nil))
                        "Earlier grants and recovery metadata remain readable and intact")
           (test-assert (tool-result-success-p (child-overflow-tests--read producer uri))
                        "Earlier emission remains publicly readable after the rejected append")
           (let ((result (child-overflow-tests--read producer (format nil "context:~A" next-digest))))
             (test-assert (and (not (tool-result-success-p result))
                               (eq :access-denied (tool-result-error-code result)))
                          "The ungranted failed emission cannot be read by digest knowledge"))
           (test-assert (task-continuity-tests--refused-p
                         (lambda ()
                           (task-continuity-declare parent orchestrator
                                                    (session-job-execution-identifier job)
                                                    :safety (make-string 200 :initial-element #\s)))
                         :code :storage-limit)
                        "Recovery declarations share the safe capacity refusal")
           (test-assert (string= before (uiop:read-file-string path :external-format :utf-8))
                        "Rejected recovery metadata preserves the previous record")
           (let* ((result  (child-overflow-tests--call producer "test" "overflow-owned"
                                                       :arguments (json-object "text" text)))
                  (content (tool-result-content result)))
             (test-assert (and (tool-result-success-p result) (search uri content))
                          "Re-emitting an existing digest needs no additional capacity"))
           (test-assert (string= before (uiop:read-file-string path :external-format :utf-8))
                        "A duplicate grant does not change earlier metadata")))))))


;;;; -- Administrative Publication Contract --

(defclass task-checkpoint-test-context (mission-context)
  ((current-p :initform t :accessor task-checkpoint-test-current-p
              :documentation "Whether the fixture's original attempt is current."))
  (:documentation "Inert host authority for publication tests, not a mission producer."))

(defmethod task-checkpoint-current-p ((context task-checkpoint-test-context) generation attempt)
  "Validate the single exact fixture generation and attempt without I/O."
  (and (task-checkpoint-test-current-p context)
       (equal generation "generation-1") (eql attempt 1)))

(defmethod task-checkpoint-mission-identifier ((context task-checkpoint-test-context))
  "Return the inert fixture's exact mission identity."
  (declare (ignore context))
  "mission-1")

(-> task-checkpoint-tests--publication (keyword function) t)
(defun task-checkpoint-tests--publication (mode inspect)
  "Run a real admitted task and claimed host publication, then call INSPECT."
  (task-continuity-tests--fixture
   (lambda (parent orchestrator context template path)
     (declare (ignore context path))
     (let* ((origin   (make-instance 'task-checkpoint-test-context :application nil :goal nil))
            (start    (bt:make-semaphore))
            (captured (bt:make-semaphore))
            (release  (bt:make-semaphore))
            (claimed  (bt:make-semaphore))
            (bodies 0) (cleanups 0) (publications 0)
            (job
              (first
               (job-pool-submit-batch
                (task-orchestrator-pool orchestrator)
                (list
                 (list :name                         "Checkpoint publication fixture"
                       :maximum-runtime-milliseconds 30000
                       :root-identifier              (task-parent-root-conversation-identifier parent)
                       :owner-identifiers            nil
                       :function
                       (lambda (job)
                         (unwind-protect
                              (progn
                                (assert (bt:wait-on-semaphore start :timeout 10))
                                (incf bodies)
                                (unless (eq mode ':forged)
                                  (let* ((cursor (list :conversation-id
                                                       (conversation-identifier (agent-conversation parent))
                                                       :request-count 0 :next-action ':review-required))
                                         (durable-p
                                           (handler-case
                                               (progn
                                                 (when (eq mode ':checkpoint-storage)
                                                   (error 'task-error :tool-name "job.get"
                                                          :message               "Injected checkpoint storage failure."))
                                                 (task-continuity--write
                                                  (merge-pathnames "checkpoint.sexp"
                                                                   (task--artifact-root (agent-configuration parent) job))
                                                  cursor :require-absent t)
                                                 t)
                                             (task-error () nil)))
                                         (candidate
                                           (task-job-capture-review-checkpoint
                                            job :context origin :generation "generation-1"
                                            :attempt 1 :mission-id "mission-1" :cursor cursor
                                            :durable-checkpoint-p durable-p)))
                                    (assert candidate)
                                    (test-assert
                                     (null (task-job-capture-review-checkpoint
                                            job :context origin :generation "generation-1"
                                            :attempt 1 :mission-id "mission-1" :cursor nil))
                                     "A competing construction cannot replace the issued checkpoint")
                                    (case mode
                                      (:stale
                                       (setf (task-checkpoint-test-current-p origin) nil))
                                      ((:owners :generation :mission-id :execution-id :run-token :attempt)
                                       (let ((data (task-administrative-checkpoint-data candidate)))
                                         (setf (getf data mode)
                                               (case mode (:owners
                                                           '("foreign")) (:attempt
                                                                          2)
                                                     (otherwise
                                                      "foreign"))))))))
                                (bt:signal-semaphore captured)
                                (when (eq mode ':cancel-before)
                                  (assert (bt:wait-on-semaphore release :timeout 10)))
                                (list :status ':suspended :output "Review required"
                                      :administrative-checkpoint '(:forged t)
                                      :outcome ':review-required :yielded-p nil))
                           (incf cleanups)))
                       :terminal-result-function
                       (lambda (job state result report)
                         (incf publications)
                         (when (eq mode ':cancel-after)
                           (bt:signal-semaphore claimed)
                           (assert (bt:wait-on-semaphore release :timeout 10)))
                         (flet ((publish ()
                                  (task-job--terminal-record job :state state :result result :report report)))
                           (case mode
                             (:artifact-storage
                              (test-call-with-function-replacements
                               (list (list 'task--write-result-artifact
                                           (lambda (job result)
                                             (declare (ignore job result))
                                             (error 'task-error :tool-name "job.get"
                                                    :message               "Injected artifact storage failure."))))
                               #'publish))
                             (:terminal-storage
                              (let ((write (symbol-function 'task-continuity--write)))
                                (test-call-with-function-replacements
                                 (list (list 'task-continuity--write
                                             (lambda (path record &key require-absent)
                                               (if (equal "terminal.sexp" (file-namestring path))
                                                   (error 'task-error :tool-name "job.get"
                                                          :message               "Injected terminal storage failure.")
                                                   (funcall write path record :require-absent require-absent)))))
                                 #'publish)))
                             (otherwise
                              (publish)))))
                       :initargs
                       (list :orchestrator orchestrator :execution-identifier (make-user-identifier)
                             :definition        (task-job-definition template)
                             :item              (list :agent "task" :task "Inspect checkpoint publication" :async t)
                             :parent-agent parent :mission-context origin :detached-p t
                             :completion-owner-conversation
                             (conversation-identifier (agent-conversation parent))
                             :completion-policy ':continue)))))))
       (unwind-protect
            (progn
              (task-continuity-record-job job parent)
              (bt:signal-semaphore start)
              (assert (bt:wait-on-semaphore captured :timeout 10))
              (case mode
                (:cancel-before
                 (test-assert (job-cancel job :reason ':requested)
                              "Accepted cancellation before publication wins")
                 (bt:signal-semaphore release))
                (:cancel-after
                 (assert (bt:wait-on-semaphore claimed :timeout 10))
                 (test-assert (not (job-cancel job :reason ':requested))
                              "The library refuses cancellation after its publication claim")
                 (bt:signal-semaphore release)))
              (multiple-value-bind (snapshot terminal-p) (job-await job :timeout-seconds 10)
                (assert terminal-p)
                (test-assert (= 1 bodies cleanups publications)
                             "The actual body, unwind cleanup and terminal callback run once")
                (funcall inspect parent orchestrator job snapshot)))
         (bt:signal-semaphore start)
         (bt:signal-semaphore release)
         (task-completion-release parent))))))

(-> test-task-checkpoint-publication () null)
(defun test-task-checkpoint-publication ()
  "Preserve trusted suspension through artifacts, live projection and fresh artifact-only recovery."
  (task-checkpoint-tests--publication
   ':trusted
   (lambda (parent orchestrator job snapshot)
     (let* ((result    (getf snapshot :result))
            (entry     (task-continuity--find parent (session-job-execution-identifier job)))
            (directory (uiop:pathname-directory-pathname (pathname (getf entry :path))))
            (record    (getf entry :record))
            (artifact  (task--read-result-artifact (merge-pathnames "result.sexp" directory)))
            (live      (task-completion--snapshot
                        job snapshot :root (uiop:pathname-parent-directory-pathname directory)))
            (notice    (task-completion--notice
                        (list :payload (cl-jobpond:completion-event live)))))
       (test-assert (and (eq ':aborted (getf snapshot :state))
                         (null (getf snapshot :cancellation-reason))
                         (task-review-required-result-p result :record record))
                    "Trusted administrative publication is physically aborted without cancellation")
       (test-assert (task-review-required-result-p artifact :record record)
                    "The full artifact retains the validated checkpoint")
       (test-assert (eq ':review-required (getf notice :outcome))
                    "The host live boundary overrides the library's physical cancellation outcome")
       (test-assert (eq ':review-required
                         (getf (task-continuity-classify entry parent orchestrator) :classification))
                    "Live continuity classifies suspension before generic completion")
       (delete-file (merge-pathnames "terminal.sexp" directory))
       (let* ((registry (task-augment-tool-registry
                         (make-default-tool-registry :configuration (agent-configuration parent))))
              (restored (agent-create :configuration (agent-configuration parent)
                                      :provider      (agent-provider parent)
                                      :conversation  (agent-conversation parent)
                                      :tool-registry registry :worker nil))
              (fresh    (task-run-tool-orchestrator (tool-registry-find registry "task" "run"))))
         (unwind-protect
              (progn
                (task-completion-restore restored)
                (let ((pending (task-completion-pending restored)))
                  (test-assert (and (= 1 (length pending))
                                    (eq ':review-required (getf (first pending) :outcome))
                                    (eq ':aborted (getf (first pending) :execution-state)))
                               "A fresh runtime restores artifact-only suspension for notification"))
                (test-assert (null (task-orchestrator-list-jobs fresh))
                             "Artifact-only restoration dispatches no job")
                (test-assert
                 (task-continuity-tests--refused-p
                  (lambda ()
                    (task-continuity-revive
                     (make-instance 'tool-context :configuration (agent-configuration restored)
                                    :agent restored :registry registry)
                     fresh (session-job-execution-identifier job) :authorized-p t)))
                 "Even explicit revive authorization cannot replay suspended authority"))
           (task-completion-release restored)
           (task-orchestrator-close fresh))))))
  nil)

(-> test-task-checkpoint-publication-refusals () null)
(defun test-task-checkpoint-publication-refusals ()
  "Reject forged or stale authority and preserve the library's cancellation linearization."
  (dolist (mode '(:forged :owners :generation :mission-id :execution-id :run-token :attempt :stale
                  :cancel-before :cancel-after :checkpoint-storage))
    (task-checkpoint-tests--publication
     mode
     (lambda (parent orchestrator job snapshot)
       (declare (ignore parent orchestrator job))
       (let ((result (getf snapshot :result)))
         (if (member mode '(:cancel-after :checkpoint-storage))
             (progn
               (test-assert (task-review-required-result-p result)
                            "Valid retained authority preserves review-required")
               (when (eq mode ':checkpoint-storage)
                 (test-assert (not (getf result :durable-checkpoint-p))
                              "An unwritten checkpoint makes no durability promise")))
             (test-assert (not (task-review-required-result-p result))
                          "Raw or mismatched evidence cannot mint suspension"))
         (when (eq mode ':cancel-before)
           (test-assert (and (eq ':aborted (getf snapshot :state))
                             (eq ':requested (getf snapshot :cancellation-reason))
                             (eq ':aborted (getf result :status)))
                        "Accepted user cancellation is not mislabelled as review-required"))))))
  nil)

(-> test-task-checkpoint-publication-storage () null)
(defun test-task-checkpoint-publication-storage ()
  "Keep the primary suspension and consistent durability across storage failures."
  (dolist (mode '(:artifact-storage :terminal-storage))
    (task-checkpoint-tests--publication
     mode
     (lambda (parent orchestrator job snapshot)
       (declare (ignore orchestrator))
       (let* ((result        (getf snapshot :result))
              (entry         (task-continuity--find parent (session-job-execution-identifier job)))
              (directory     (uiop:pathname-directory-pathname (pathname (getf entry :path))))
              (artifact-path (merge-pathnames "result.sexp" directory))
              (terminal-path (merge-pathnames "terminal.sexp" directory))
              (disk          (if (probe-file artifact-path)
                                 (task--read-result-artifact artifact-path)
                                 (getf (task-continuity--read terminal-path) :result))))
         (test-assert (and (task-review-required-result-p result)
                           (eq ':aborted (getf snapshot :state))
                           (getf result :persistence-failed))
                      "Secondary storage failure does not replace the primary administrative outcome")
         (test-assert (equal (getf result :administrative-checkpoint)
                             (getf disk :administrative-checkpoint))
                      "Surviving disk evidence and live publication agree on checkpoint durability")
         (test-assert (task-review-required-result-p disk :record (getf entry :record))
                      "Surviving fallback storage retains exact durable ownership")
         (test-assert (eq (getf result :persistence-failure-phase)
                          (if (eq mode ':artifact-storage) ':artifact ':terminal-metadata))
                      "The secondary failure identifies its exact storage phase")))))
  nil)

(defmethod task-checkpoint-lineage-current-p ((context task-checkpoint-test-context) job publication-p)
  "S2 publication tests provide an explicit host authority protocol without S3 mission scopes."
  (declare (ignore context job publication-p))
  t)
