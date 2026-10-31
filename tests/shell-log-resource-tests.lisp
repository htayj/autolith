(in-package #:autolith)

;;;; -- Shell Capture Resource Fixtures --

(-> shell-log-tests--context (configuration string) tool-context)
(defun shell-log-tests--context (configuration identifier)
  "Create a standalone requesting conversation without a runtime."
  (make-instance 'tool-context :configuration configuration :worker nil
                 :conversation (conversation-create configuration :identifier identifier)))

(-> shell-log-tests--capture (shell-log-artifact vector &optional keyword) t)
(defun shell-log-tests--capture (artifact bytes &optional (stream ':output))
  "Write exact fixture octets and return closed generic capture evidence."
  (let* ((path    (merge-pathnames (if (eq stream ':output) "output.bin" "error.bin")
                                   (shell-log-capture-directory artifact)))
         (capture (make-instance 'cl-exec-sandbox:sandbox-capture :path path)))
    (with-open-file (output path :direction ':output :if-exists ':supersede
                                :element-type '(unsigned-byte 8))
      (write-sequence bytes output))
    (setf (cl-exec-sandbox:sandbox-capture-byte-count capture) (length bytes)
          (cl-exec-sandbox:sandbox-capture-observed-byte-count capture) (length bytes)
          (cl-exec-sandbox:sandbox-capture-complete-p capture) t
          (cl-exec-sandbox:sandbox-capture-status capture) ':complete)
    capture))

(-> shell-log-tests--finish (shell-log-artifact vector &key (:error-bytes (option vector))) list)
(defun shell-log-tests--finish (artifact bytes &key error-bytes)
  "Publish merged or separated fixture captures through the generic result boundary."
  (shell-log-record-capture
   artifact
   (make-instance 'cl-exec-sandbox:sandbox-result
                  :output-capture (shell-log-tests--capture artifact bytes)
                  :error-capture  (and error-bytes (shell-log-tests--capture artifact error-bytes ':error))
                  :exit-code 7 :timed-out-p nil :cancelled-p nil :status ':exited)))

(-> shell-log-tests--resolve (shell-log-artifact tool-context &optional keyword) shell-log-resource)
(defun shell-log-tests--resolve (artifact context &optional (stream ':output))
  "Reopen a reference through the authorized resolver rather than retained objects."
  (resource-resolver-resolve (make-instance 'shell-log-resolver :scheme "shell-log")
                             (subseq (shell-log-reference artifact stream) 10) context))

(-> shell-log-tests--read (shell-log-resource tool-context hash-table) tool-result)
(defun shell-log-tests--read (resource context arguments)
  "Invoke the model-facing bounded resource adapter."
  (resource-tool-read
   resource (make-instance 'resource-read-tool
                           :namespace "resource" :name "read" :description "Read shell capture."
                           :parameters        (tool-object-schema (json-object) '())
                           :resource-registry (make-resource-registry))
   context arguments))

(-> shell-log-tests--public-inventory () null)
(defun shell-log-tests--public-inventory ()
  "Exercise bounded job metadata, public projection and missing-manifest recovery."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (context                        (shell-log-tests--context configuration "shell-inventory"))
           (orchestrator                   (task-orchestrator-create))
           (job                            (task-tests--make-job orchestrator :identifier "inventory-job"
                                                                 :root-identifier         "shell-inventory"))
           (latest                         nil)
           (base-time                      (get-universal-time)))
      (unwind-protect
           (progn
             (loop for index from 1 to 17
                   for artifact = (shell-log-allocate context :job job)
                   do (shell-log-tests--finish artifact (utf8-string-to-octets "x"))
                      (setf (getf (shell-log-artifact-metadata artifact) :created-at)
                            (+ base-time index)
                            latest artifact)
                      (shell-log--write-manifest artifact))
             (let* ((metadata (shell-log-job-metadata configuration job))
                    (logs     (getf metadata :shell-logs)))
               (test-assert (and (= 16 (length logs)) (= 1 (getf metadata :shell-logs-omitted)))
                            "Job inspection exposes bounded newest logs and explicit omissions")
               (test-assert (equal (getf (first logs) :artifact-id)
                                   (getf (shell-log-artifact-metadata latest) :artifact-id))
                            "Newest public inventory follows durable creation order")
               (test-assert
                (every (lambda (log)
                         (and (not (getf log :owner-execution-ids))
                              (not (getf log :completion-owner-conversation))
                              (every (lambda (capture) (not (getf capture :file)))
                                     (getf log :captures)))) logs)
                "Public inventory omits private filenames and ancestor ownership")
               (test-assert (< (length (utf8-string-to-octets (task--write-readable-sexp metadata)))
                               (* 64 1024))
                            "Public shell metadata fits below the tool metadata bound"))
             (let* ((missing-job (task-tests--make-job orchestrator :identifier "missing-job"
                                                       :root-identifier         "shell-inventory"))
                    (artifact    (shell-log-allocate context :job missing-job)))
               (shell-log-tests--finish artifact (utf8-string-to-octets "x"))
               (platform-delete-file *platform* (shell-log--manifest-path
                                                 (shell-log-capture-directory artifact)))
               (test-assert
                (eq ':unavailable (getf (first (getf (shell-log-job-metadata configuration missing-job)
                                                    :shell-logs)) :state))
                "Missing manifests are bounded diagnostics rather than terminal inspection failures")))
        (task-orchestrator-close orchestrator))))
  nil)

(-> shell-log-tests--descendant-authority () null)
(defun shell-log-tests--descendant-authority ()
  "Check current child ownership through the resolver, including sibling refusal."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (context                        (shell-log-tests--context configuration "shell-descendants"))
           (orchestrator                   (task-orchestrator-create))
           (job                            (task-tests--make-job orchestrator :identifier "current-child"
                                                                 :root-identifier         "shell-descendants"))
           (agent                          (task-tests--child-viewer configuration job))
           (child-context                  (make-instance 'tool-context :configuration configuration :worker nil
                                                          :conversation (agent-conversation agent) :agent agent))
           (artifact                       (shell-log-allocate context)))
      (unwind-protect
           (progn
             (shell-log-tests--finish artifact (utf8-string-to-octets "owned descendant"))
             (setf (getf (shell-log-artifact-metadata artifact) :owner-execution-ids)
                   (list (session-job-execution-identifier job)))
             (shell-log--write-manifest artifact)
             (test-assert
              (tool-result-success-p (shell-log-tests--read
                                      (shell-log-tests--resolve artifact child-context)
                                      child-context (json-object)))
              "A current child may inspect captures owned by its descendants")
             (setf (getf (shell-log-artifact-metadata artifact) :owner-execution-ids)
                   (list "different-sibling-execution"))
             (shell-log--write-manifest artifact)
             (test-assert
              (handler-case (progn (shell-log-tests--resolve artifact child-context) nil)
                (resource-access-denied () t))
              "A sibling execution is refused even within the same root session"))
        (task-orchestrator-close orchestrator))))
  nil)

(-> shell-log-tests--delivery-and-accounting () null)
(defun shell-log-tests--delivery-and-accounting ()
  "Protect undelivered sync logs, verify ordinary receipts, and measure actual disk use."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*               (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*   32)
           (*shell-log-retention-byte-limit* 96)
           (context                          (shell-log-tests--context configuration "shell-sync-proof"))
           (orchestrator                     (task-orchestrator-create))
           (job                              (task-tests--make-job orchestrator :identifier "sync-proof-job"
                                                                   :root-identifier "shell-sync-proof" :detached-p nil)))
      (unwind-protect
           (progn
             (setf (slot-value job 'completion-owner-conversation) "shell-sync-proof"
                   (slot-value job 'parent-call-id) "expected-shell-call")
             (let* ((artifact (shell-log-allocate context :job job))
                    (metadata (shell-log-tests--finish artifact (utf8-string-to-octets "x"))))
              (test-assert
               (= 1 (getf (shell-log--read-manifest
                           configuration (shell-log-capture-directory artifact))
                          :accounted-bytes))
               "Closed captures account for retained bytes, not their launch reservation")
              (test-assert (not (shell-log--prunable-p configuration
                                                      (shell-log-capture-directory artifact) metadata))
                           "Closing a synchronous job does not prove delivery")
               ;; Simulate an oversized regular file independently of the executor.
              (shell-log-tests--capture artifact
                                        (make-array 100 :element-type    '(unsigned-byte 8)
                                                        :initial-element 120))
              (let* ((measured (shell-log--read-manifest configuration
                                                        (shell-log-capture-directory artifact)))
                     (capture  (first (getf measured :captures))))
                (test-assert (and (= 100 (getf measured :accounted-bytes))
                                  (= 100 (getf capture :byte-count))
                                  (getf capture :oversized-p) (not (getf capture :complete-p)))
                             "Accounting includes actual oversized safe regular files")
                (test-assert (handler-case (progn (shell-log-allocate context) nil)
                               (shell-log-error () t))
                             "Actual stored bytes cannot evade aggregate reservation limits"))
              (labels ((append-receipt (call-id)
                         "Persist an ordinary owner result containing this execution identity."
                         (conversation-append-record
                          (tool-context-conversation context)
                          (list :tool-result :call-id call-id :tool "shell.run"
                                :status ':ok :category ':success :output "finished"
                                :details (list :shell-logs (list metadata))))))
                (append-receipt "unrelated-shell-call")
                (test-assert (not (shell-log--sync-delivered-p configuration metadata))
                             "A different provider call cannot acknowledge this execution")
                (append-receipt "expected-shell-call")
                (test-assert (shell-log--sync-delivered-p configuration metadata)
                             "Exact-owner durable terminal tool metadata proves sync delivery")
                (setf (getf metadata :detached-p) t)
                (test-assert (not (shell-log--sync-delivered-p configuration metadata))
                             "Detached jobs require a completion receipt, not an ordinary tool result")
                (setf (getf metadata :detached-p) nil))
              (let ((replacement (shell-log-allocate context)))
                (test-assert (eq ':pruned (getf (shell-log--read-manifest
                                                 configuration (shell-log-capture-directory artifact)) :state))
                             "Delivered sync logs are eligible for safe quota pruning")
                (shell-log-release-artifact replacement))))
        (task-orchestrator-close orchestrator))))
  nil)

(-> test-shell-log-child-delivery () null)
(defun test-shell-log-child-delivery ()
  "Prune child captures only after delivery in the child's private transcript."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*                (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*    32)
           (*shell-log-retention-byte-limit*  32)
           (*shell-log-retention-count-limit* 1)
           (root                              "shell-child-delivery")
           (orchestrator                      (task-orchestrator-create))
           (child-job                         (task-tests--make-job orchestrator :identifier "child-owner"
                                                                     :root-identifier        root))
           (owner                             (task-job-execution-identifier child-job))
           (child                             (conversation-create configuration :identifier owner
                                                                   :storage-root             (task--artifact-root configuration child-job)))
           (unrelated                         (conversation-create configuration :identifier owner))
           (context                           (make-instance 'tool-context :configuration configuration
                                                                          :conversation child :worker nil))
           (job                               (task-tests--make-job orchestrator :identifier "child-command"
                                                                     :root-identifier root :detached-p nil)))
      (unwind-protect
           (progn
             (conversation-append-user-message child "Run one command.")
             (setf (slot-value job 'completion-owner-conversation) owner
                   (slot-value job 'parent-call-id) "child-shell-call")
             (let* ((artifact  (shell-log-allocate context :job job))
                    (metadata  (shell-log-tests--finish artifact (utf8-string-to-octets "child-output")))
                    (directory (shell-log-capture-directory artifact))
                    (capture   (merge-pathnames "output.bin" directory))
                    (receipt   (list :tool-result :call-id "child-shell-call" :tool "shell.run"
                                     :status ':ok :category ':success :output "finished"
                                     :details (list :shell-logs
                                                    (list (shell-log--public-metadata metadata))))))
               (conversation-append-record unrelated receipt)
               (let ((replacement (handler-case (shell-log-allocate context)
                                    (shell-log-error () nil))))
                 (when replacement (shell-log-release-artifact replacement))
                 (test-assert (null replacement)
                              "A same-ID primary transcript cannot acknowledge a child capture"))
               (test-assert (probe-file capture)
                            "Undelivered child output remains available under quota pressure")
               (conversation-append-record child receipt)
               (let ((replacement (shell-log-allocate context)))
                 (unwind-protect
                      (test-assert
                       (and (eq ':pruned (getf (shell-log--read-manifest configuration directory) :state))
                            (not (probe-file capture)))
                       "The exact child transcript permits quota pruning of delivered output")
                   (shell-log-release-artifact replacement)))))
        (task-orchestrator-close orchestrator))))
  nil)

(-> shell-log-tests--pre-executor-unwind () null)
(defun shell-log-tests--pre-executor-unwind ()
  "Release real pre-executor failures even when a synchronous job has no receipt."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*                (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*    32)
           (*shell-log-retention-byte-limit*  32)
           (*shell-log-retention-count-limit* 1)
           (context                           (shell-log-tests--context configuration "shell-preflight"))
           (orchestrator                      (task-orchestrator-create))
           (*tool-execution-current-job*
             (task-tests--make-job orchestrator :identifier "preflight-job"
                                  :root-identifier "shell-preflight" :detached-p nil)))
      (unwind-protect
           (loop repeat 2
                 do (test-assert
                     (handler-case
                         (progn
                           (workspace-tool-run-shell-command
                            (test-fixture-shell-command *platform* "exit 0" "exit 0")
                            (config :working-directory configuration)
                            :policy       ':invalid-policy
                            :timeout      1
                            :output-limit 32
                            :context      context)
                           nil)
                       (error () t))
                     "Policy validation fails before the executor can launch")
                    (test-assert (zerop (hash-table-count *shell-log-active*))
                                 "A live worker releases the abandoned artifact pin")
                    (let* ((metadata
                             (loop for directory in (shell-log--directories configuration)
                                   for item = (shell-log--read-manifest configuration directory)
                                   when (eq (getf item :state) ':closed) return item)))
                      (test-assert (and metadata (eq ':interrupted (getf metadata :status))
                                        (zerop (getf metadata :reserved-bytes))
                                        (null (getf metadata :captures)))
                                   "Pre-executor failures free quota without inventing captures")))
        (task-orchestrator-close orchestrator))))
  nil)


;;;; -- Durable Storage and Inspection --

(-> test-shell-log-private-allocation () null)
(defun test-shell-log-private-allocation ()
  "Reserve before launch; generate distinct execution identities without a runtime."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*               (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*   32)
           (*shell-log-retention-byte-limit* 96)
           (context                          (shell-log-tests--context configuration "shell-owner"))
           (first                            (shell-log-allocate context))
           (second                           (shell-log-allocate context :merge-output-p nil)))
      (test-assert (probe-file (shell-log--manifest-path (shell-log-capture-directory first)))
                   "Ownership exists before any capture launch")
      (test-assert (not (equal (getf (shell-log-artifact-metadata first) :execution-id)
                               (getf (shell-log-artifact-metadata second) :execution-id)))
                   "Standalone invocations have stable independent execution identities")
      (test-assert (= 64 (getf (shell-log-artifact-metadata second) :reserved-bytes))
                   "Separated streams reserve both finite allowances")
      (test-assert (handler-case (progn (shell-log-allocate context) nil)
                     (shell-log-error () t))
                   "Active captures cannot be pruned to evade aggregate reservation")
      (with-test-fixture (':file-modes "private shell artifact permissions")
        (test-assert (= 0 (logand #o077 (platform-file-permissions
                                         *platform* (shell-log-capture-directory first))))
                     "Private capture directories exclude group and other access"))))
  (shell-log-tests--public-inventory)
  (shell-log-tests--delivery-and-accounting)
  (shell-log-tests--pre-executor-unwind)
  nil)

(-> test-shell-log-byte-windows-and-search () null)
(defun test-shell-log-byte-windows-and-search ()
  "Read exact raw ranges and bounded search pages, including the shared fifty-result cap."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*              (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*  256)
           (*shell-log-resource-scan-bytes* 16)
           (context                         (shell-log-tests--context configuration "shell-windows"))
           (artifact                        (shell-log-allocate context))
           (bytes                           (make-array 96 :element-type '(unsigned-byte 8) :initial-element 120)))
      (replace bytes (utf8-string-to-octets "needle") :start1 14)
      (replace bytes (utf8-string-to-octets "needle") :start1 50)
      (setf (aref bytes 30) 255 (aref bytes 31) 195 (aref bytes 32) 169)
      (shell-log-tests--finish artifact bytes)
      (let* ((resource (shell-log-tests--resolve artifact context))
             (path     (merge-pathnames "output.bin" (shell-log-capture-directory artifact))))
        (multiple-value-bind (range size next)
            (shell-log-resource--read-bytes configuration path :offset 29 :count 4)
          (test-assert (equalp range (subseq bytes 29 33))
                       "Binary ranges preserve invalid bytes and UTF-8 boundaries")
          (test-assert (and (= size 96) (= next 33)) "Byte ranges report truthful positions"))
        (let ((result (shell-log-tests--read resource context
                                             (json-object "byte-offset" 30 "byte-count" 3))))
          (test-assert (tool-result-success-p result) "Invalid UTF-8 renders without losing retained raw bytes")
          (test-assert (< (length (tool-result-content result)) 7000) "Resource text is bounded inline"))
        (let ((offset 0) (found nil))
          (loop
            for page = (shell-log-resource--search configuration path :query "needle"
                                                   :offset offset :maximum 1)
            do (setf found (append found (mapcar (lambda (match) (getf match :byte-offset))
                                                 (getf page :matches))))
               (test-assert (<= (getf page :scanned-byte-count) 21) "Search scans finite byte budgets")
            while (getf page :next-byte-offset)
            do (test-assert (> (getf page :next-byte-offset) offset) "Search pagination always progresses")
               (setf offset (getf page :next-byte-offset)))
          (test-assert (equal found '(14 50)) "Search recovers middle and boundary-spanning matches exactly"))
        (let ((*shell-log-resource-scan-bytes* 1024))
          (dolist (maximum '(20 30 50))
            (test-assert
             (tool-result-success-p
              (shell-log-tests--read resource context (json-object "query" "x" "max-results" maximum)))
             "Resource reads accept explicit search limits through fifty"))
          (let* ((page  (shell-log-resource--search configuration path :query "x" :maximum 50))
                 (next  (shell-log-resource--search configuration path :query "x" :maximum 50
                                                   :offset (getf page :next-byte-offset)))
                 (found (mapcar (lambda (match) (getf match :byte-offset))
                                (append (getf page :matches) (getf next :matches)))))
            (test-assert (= 50 (length (getf page :matches))) "Explicit fifty-result pages honor their bound")
            (test-assert (equal found (loop for byte across bytes for index from 0
                                           when (= byte 120) collect index))
                         "Larger pages resume without dropping or repeating matches")
            (test-assert (null (getf next :next-byte-offset)) "The final search page reaches the end")))
        (test-assert
         (handler-case (progn (shell-log-tests--read resource context
                                                     (json-object "query" "x" "max-results" 51)) nil)
           (tool-error () t))
         "Search result limits above fifty are rejected")
        (test-assert
         (handler-case (progn (shell-log-tests--read resource context
                                                     (json-object "byte-count" 4097)) nil)
           (tool-error () t))
         "Oversized resource windows are rejected"))))
  nil)

(-> test-shell-log-resource-authority () null)
(defun test-shell-log-resource-authority ()
  "Reject cross-session references, traversal and symbolic-link capture substitution."
  (with-test-configuration (configuration root)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 256)
           (owner                          (shell-log-tests--context configuration "shell-authority"))
           (foreign                        (shell-log-tests--context configuration "shell-foreign"))
           (artifact                       (shell-log-allocate owner)))
      (shell-log-tests--finish artifact (utf8-string-to-octets "private diagnostics"))
      (test-assert
       (handler-case (progn (shell-log-tests--resolve artifact foreign) nil)
         (resource-access-denied () t))
       "A different root session cannot inspect private captures")
      (dolist (identifier '("shell-authority/../x/output" "shell-authority/x/y/../../output"
                            "shell-authority/x/y/%2e%2e"))
        (test-assert
         (handler-case
             (progn (resource-resolver-resolve (make-instance 'shell-log-resolver :scheme "shell-log")
                                               identifier owner) nil)
           (resource-access-denied () t))
         "URI identifiers cannot introduce traversal"))
      (with-test-fixture (':symbolic-links "shell capture substitution")
        (let* ((path   (merge-pathnames "output.bin" (shell-log-capture-directory artifact)))
               (secret (merge-pathnames "secret.bin" root)))
          (with-open-file (stream secret :direction ':output :if-exists ':supersede)
            (write-string "foreign" stream))
          (platform-delete-file *platform* path)
          (test-fixture-make-symbolic-link *platform* (namestring secret) (namestring path))
          (test-assert
           (handler-case
               (progn (shell-log-tests--read (shell-log-tests--resolve artifact owner)
                                             owner (json-object)) nil)
             (shell-log-error () t))
           "Capture symlinks are refused before opening foreign bytes")))))
  (shell-log-tests--descendant-authority)
  nil)

(-> test-shell-log-reopen-and-pruning () null)
(defun test-shell-log-reopen-and-pruning ()
  "Reopen closed streams without replay and preserve missing/pruned diagnostics."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*               (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*   32)
           (*shell-log-retention-byte-limit* 64)
           (context                          (shell-log-tests--context configuration "shell-restart"))
           (artifact                         (shell-log-allocate context :merge-output-p nil))
           (reference                        (shell-log-reference artifact)))
      (shell-log-tests--finish artifact (utf8-string-to-octets "stdout")
                              :error-bytes (utf8-string-to-octets "stderr failure"))
      (let ((*shell-log-active* (make-hash-table :test #'equal)))
        (let ((reopened (shell-log-tests--resolve artifact context ':error)))
          (test-assert
           (search "stderr failure" (tool-result-content
                                      (shell-log-tests--read reopened context (json-object))))
           "Restart inspection recovers the separated error stream from its manifest")))
      (let* ((*shell-log-retention-byte-limit* 48)
             (replacement                      (shell-log-allocate context)))
        (test-assert (string= reference (shell-log-reference artifact)) "Pruning does not change stable references")
        (test-assert (eq ':pruned (getf (shell-log--read-manifest configuration
                                                               (shell-log-capture-directory artifact)) :state))
                     "Quota pressure prunes eligible closed captures")
        (test-assert (tool-result-success-p
                      (shell-log-tests--read (shell-log-tests--resolve artifact context) context (json-object)))
                     "Tombstones produce a diagnostic, not a failed session")
        (shell-log-release-artifact replacement))))
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*                (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit*    32)
           (*shell-log-retention-byte-limit*  32)
           (*shell-log-retention-count-limit* 1)
           (*shell-log-tombstone-count-limit* 1)
           (context                           (shell-log-tests--context configuration "shell-tombstones")))
      (dotimes (index 3)
        (shell-log-tests--finish (shell-log-allocate context)
                                (utf8-string-to-octets (princ-to-string index))))
      (let ((states (loop for directory in (shell-log--directories configuration)
                          collect (getf (shell-log--read-manifest configuration directory) :state))))
        (test-assert (and (= 1 (count ':closed states)) (= 1 (count ':pruned states)))
                     "One allocation enforces both live-artifact and tombstone count bounds"))))
  nil)

(-> test-shell-log-capture-failure-metadata () null)
(defun test-shell-log-capture-failure-metadata ()
  "Persist actual retained/observed bytes and incomplete execution outcomes."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (context                        (shell-log-tests--context configuration "shell-failures")))
      (dolist (status '(:timeout :cancelled :launch-failed :interrupted))
        (let* ((artifact (shell-log-allocate context))
               (capture  (shell-log-tests--capture artifact (utf8-string-to-octets "partial"))))
          (setf (cl-exec-sandbox:sandbox-capture-observed-byte-count capture) 100
                (cl-exec-sandbox:sandbox-capture-complete-p capture) nil
                (cl-exec-sandbox:sandbox-capture-truncated-p capture) t
                (cl-exec-sandbox:sandbox-capture-status capture) ':limit)
          (let* ((metadata (shell-log-record-capture
                            artifact (make-instance 'cl-exec-sandbox:sandbox-result
                                                     :output-capture capture :exit-code nil
                                                     :status status :timed-out-p (eq status ':timeout)
                                                     :cancelled-p (eq status ':cancelled))))
                 (record   (first (getf metadata :captures))))
            (test-assert (and (eq status (getf metadata :status))
                             (= 7 (getf record :byte-count))
                             (= 100 (getf record :observed-byte-count))
                             (not (getf record :complete-p)) (getf record :truncated-p))
                         "Portable manifests preserve storage loss separately from execution failure"))))))
  nil)

(-> test-shell-log-cross-process-lifecycle () null)
(defun test-shell-log-cross-process-lifecycle ()
  "Keep live foreign captures readable and reserved, then reclaim after owner exit."
  (with-test-configuration (configuration root)
    (let* ((*shell-log-capture-byte-limit*    32)
           (*shell-log-retention-byte-limit*  32)
           (*shell-log-retention-count-limit* 1)
           (context                           (shell-log-tests--context configuration "shell-process"))
           (ready                             (merge-pathnames "capture.sexp" root))
           (worker                            nil)
           (replacement                       nil))
      (unwind-protect
           (progn
             (multiple-value-bind (rendered child)
                 (test-conversation--worker-eval
                  configuration
                  (format nil
                          "(let* ((configuration ~A)
         (*shell-log-capture-byte-limit* 32)
         (*shell-log-retention-byte-limit* 32)
         (*shell-log-retention-count-limit* 1)
         (context (make-instance 'tool-context :configuration configuration :worker nil
                    :conversation (conversation-create configuration :identifier \"shell-process\")))
         (artifact (shell-log-allocate context))
         (path (merge-pathnames \"output.bin\" (shell-log-capture-directory artifact)))
         (capture (make-instance 'cl-exec-sandbox:sandbox-capture :path path)))
    (with-open-file (output path :direction ':output :element-type '(unsigned-byte 8))
      (write-sequence (utf8-string-to-octets \"live bytes\") output))
    (shell-log-record-capture artifact
      (make-instance 'cl-exec-sandbox:sandbox-result :output-capture capture
                     :exit-code nil :timed-out-p nil :cancelled-p nil :status ':running)
      :closed-p nil)
    (task-continuity--write ~S
      (list :reference (shell-log-reference artifact) :directory (namestring (shell-log-capture-directory artifact)))))"
                          (test-conversation--child-configuration-form configuration) ready))
               (declare (ignore rendered))
               (setf worker child))
             (let* ((published         (task-continuity--read ready))
                    (reference         (getf published :reference))
                    (capture-directory (getf published :directory)))
               (flet ((resolve ()
                        (resource-resolver-resolve (make-instance 'shell-log-resolver :scheme "shell-log")
                                                   (subseq reference 10) context)))
                 (let ((resource (resolve)))
                   (test-assert (eq ':active (getf (shell-log-resource-metadata resource) :state))
                                "Another process's live capture is not interrupted")
                   (test-assert
                    (search "live bytes" (tool-result-content
                                              (shell-log-tests--read resource context (json-object))))
                    "A live foreign capture can be read through its authorized reference"))
                 (test-assert (handler-case
                                 (let ((unexpected (shell-log-allocate context)))
                                   (shell-log-release-artifact unexpected)
                                   nil)
                                (shell-log-error () t))
                              "A foreign owner's reservation prevents quota overcommit and pruning")
                 (test-assert (probe-file (merge-pathnames "output.bin" capture-directory))
                              "Quota pressure preserves the live capture file")
                 (lisp-worker-stop worker)
                 (setf worker nil)
                 (test-assert (eq ':interrupted (getf (shell-log-resource-metadata (resolve)) :state))
                              "Owner exit releases the kernel lease and exposes interruption")
                 (setf replacement (shell-log-allocate context))
                 (test-assert (eq ':pruned (getf (shell-log-resource-metadata (resolve)) :state))
                              "Quota pressure reclaims the dead owner's capture with a truthful tombstone")
                 (shell-log-release-artifact replacement)
                 (setf replacement nil)
                 (let ((*shell-log-tombstone-count-limit* 0))
                   (setf replacement (shell-log-allocate context)))
                 (test-assert (not (probe-file capture-directory))
                              "Tombstone expiry removes lease files along with expired metadata")))
             (shell-log-release-artifact replacement)
             (setf replacement nil)
             (multiple-value-bind (rendered child)
                 (test-conversation--worker-eval
                  configuration
                  (format nil
                          "(let* ((configuration ~A)
         (*shell-log-capture-byte-limit* 32)
         (*shell-log-retention-byte-limit* 32)
         (*shell-log-retention-count-limit* 1)
         (context (make-instance 'tool-context :configuration configuration :worker nil
                    :conversation (conversation-create configuration :identifier \"shell-orphan\")))
         (artifact (shell-log-allocate context)))
    (platform-delete-file *platform* (shell-log--manifest-path (shell-log-capture-directory artifact)))
    (task-continuity--write ~S (list :directory (namestring (shell-log-capture-directory artifact)))))"
                          (test-conversation--child-configuration-form configuration) ready))
               (declare (ignore rendered))
               (setf worker child))
             (let ((capture-directory (getf (task-continuity--read ready) :directory)))
               (lisp-worker-stop worker)
               (setf worker nil
                     replacement (shell-log-allocate context))
               (test-assert (not (probe-file capture-directory))
                            "A dead unpublished allocation does not block future shell commands")))
        (when worker (lisp-worker-stop worker))
        (when replacement (shell-log-release-artifact replacement)))))
  (with-test-configuration (configuration)
    (let* ((context   (shell-log-tests--context configuration "shell-staging"))
           (directory (merge-pathnames "tasks/shell-staging/interrupted/shell-log/unpublished/"
                                       (config :data-root configuration))))
      (ensure-directories-exist directory)
      (dolist (name '("owner.lock" ".sexp-store-A1.staging"))
        (with-open-file (stream (merge-pathnames name directory) :direction :output)
          (write-string "interrupted publication" stream)))
      (let ((artifact (shell-log-allocate context)))
        (unwind-protect
             (test-assert (not (probe-file directory))
                          "Interrupted manifest staging is reclaimed before the next allocation")
          (shell-log-release-artifact artifact)))))
  (with-test-configuration (configuration)
    (let* ((*shell-log-capture-byte-limit*    32)
           (*shell-log-retention-byte-limit*  32)
           (*shell-log-retention-count-limit* 1)
           (*shell-log-tombstone-count-limit* 0)
           (context                           (shell-log-tests--context configuration "shell-update-staging"))
           (original                          (shell-log-allocate context))
           (directory                         (shell-log-capture-directory original)))
      (shell-log-tests--finish original (utf8-string-to-octets "published"))
      (with-open-file (stream (merge-pathnames ".sexp-store-A1.staging" directory) :direction ':output)
        (write-string "interrupted later update" stream))
      (let ((replacement (shell-log-allocate context)))
        (unwind-protect
             (test-assert (not (probe-file directory))
                          "A published manifest's abandoned staging does not block tombstone expiry")
          (shell-log-release-artifact replacement)))))
  (with-test-configuration (configuration root)
    (let* ((*shell-log-capture-byte-limit*    32)
           (*shell-log-retention-byte-limit*  32)
           (*shell-log-retention-count-limit* 1)
           (*shell-log-tombstone-count-limit* 0)
           (context                           (shell-log-tests--context configuration "shell-staging-refusal"))
           (original                          (shell-log-allocate context))
           (directory                         (shell-log-capture-directory original))
           (staging                           (merge-pathnames ".sexp-store-A1.staging" directory)))
      (shell-log-tests--finish original (utf8-string-to-octets "published"))
      (dolist (name '("foreign.bin" ".sexp-store-a1.staging" ".sexp-store-A1.staging.extra"))
        (let ((path (merge-pathnames name directory)))
          (with-open-file (stream path :direction ':output)
            (write-string "unexpected file" stream))
          (test-assert
           (handler-case
               (let ((artifact (shell-log-allocate context)))
                 (shell-log-release-artifact artifact)
                 nil)
             (shell-log-error () t))
           "Tombstone expiry refuses unexpected names instead of deleting them")
          (test-assert (probe-file path) "Refused files are preserved")
          (platform-delete-file *platform* path)))
      (with-test-fixture (':symbolic-links "shell manifest staging substitution")
        (let ((target (merge-pathnames "staging-target.bin" root)))
          (with-open-file (stream target :direction ':output)
            (write-string "foreign contents" stream))
          (test-fixture-make-symbolic-link *platform* (namestring target) (namestring staging))
          (test-assert
           (handler-case
               (let ((artifact (shell-log-allocate context)))
                 (shell-log-release-artifact artifact)
                 nil)
             (shell-log-error () t))
           "A staging-shaped symbolic link cannot authorize tombstone deletion")
          (test-assert (and (probe-file target)
                            (platform-path-status *platform* staging :follow-links-p nil))
                       "Refused staging links and their targets are preserved")
          (platform-delete-file *platform* staging)))))
  nil)

(-> test-shell-log-concurrent-allocation () null)
(defun test-shell-log-concurrent-allocation ()
  "Pause one process after its quota scan and contend from another process."
  (with-test-configuration (configuration)
    (let ((first-worker  nil)
          (second-worker nil))
      (flet ((evaluate (worker form)
               (let ((response (lisp-worker-request worker ':eval (list :forms (list form)))))
                 (test-assert (eq (getf (rest response) :status) ':ok)
                              (format nil "Contention worker completed: ~S" response)))))
        (unwind-protect
             (progn
               (multiple-value-bind (rendered worker)
                   (test-conversation--worker-eval
                    configuration
                    (format nil
                            "(let* ((configuration ~A)
          (scanned (sb-thread:make-semaphore))
          (resume (sb-thread:make-semaphore))
          (done (sb-thread:make-semaphore))
          (hold (sb-thread:make-semaphore))
          (state (list :resume resume :done done :hold hold :result nil))
          (prune (symbol-function 'shell-log--prune)))
     (defparameter *shell-log-contention-fixture* state)
     (setf (symbol-function 'shell-log--prune)
           (lambda (configuration &key (reservation 0))
             (funcall prune configuration :reservation reservation)
             (sb-thread:signal-semaphore scanned)
             (unless (sb-thread:wait-on-semaphore resume :timeout 30)
               (error \"Allocation was never resumed\"))))
     (sb-thread:make-thread
      (lambda ()
        (let ((*shell-log-capture-byte-limit* 32)
              (*shell-log-retention-byte-limit* 32)
              (*shell-log-retention-count-limit* 1))
          (unwind-protect
               (handler-case
                   (setf (getf state :result)
                         (shell-log-allocate
                          (make-instance 'tool-context :configuration configuration :worker nil
                           :conversation (conversation-create configuration :identifier \"shell-race-first\"))))
                 (error (condition) (setf (getf state :result) condition)))
            (setf (symbol-function 'shell-log--prune) prune)
            (sb-thread:signal-semaphore done))
          (sb-thread:wait-on-semaphore hold :timeout 30))))
     (unless (sb-thread:wait-on-semaphore scanned :timeout 30)
       (error \"First allocation never completed its scan\")))"
                            (test-conversation--child-configuration-form configuration)))
                 (declare (ignore rendered))
                 (setf first-worker worker))
               (multiple-value-bind (rendered worker)
                   (test-conversation--worker-eval
                    configuration
                    (format nil
                            "(let* ((configuration ~A)
          (entered (sb-thread:make-semaphore))
          (done (sb-thread:make-semaphore))
          (state (list :done done :result nil)))
     (defparameter *shell-log-contention-fixture* state)
     (handler-case
         (let ((lease (lease-acquire (merge-pathnames \"tasks/shell-log.lock\" (config :data-root configuration)))))
           (lease-release lease)
           (error \"Quota scan did not hold the cross-process allocation lock\"))
       (file-lock-busy () t))
     (sb-thread:make-thread
      (lambda ()
        (let ((*shell-log-capture-byte-limit* 32)
              (*shell-log-retention-byte-limit* 32)
              (*shell-log-retention-count-limit* 1))
          (sb-thread:signal-semaphore entered)
          (unwind-protect
               (setf (getf state :result)
                     (handler-case
                         (shell-log-allocate
                          (make-instance 'tool-context :configuration configuration :worker nil
                           :conversation (conversation-create configuration :identifier \"shell-race-second\")))
                       (shell-log-error () :quota-refused)))
            (sb-thread:signal-semaphore done)))))
     (unless (sb-thread:wait-on-semaphore entered :timeout 30)
       (error \"Second allocator never started\"))
     (when (sb-thread:wait-on-semaphore done :timeout 0.1)
       (error \"Second allocation bypassed the paused quota scan\")))"
                            (test-conversation--child-configuration-form configuration)))
                 (declare (ignore rendered))
                 (setf second-worker worker))
               (evaluate first-worker
                         "(progn
    (sb-thread:signal-semaphore (getf *shell-log-contention-fixture* :resume))
    (unless (sb-thread:wait-on-semaphore (getf *shell-log-contention-fixture* :done) :timeout 30)
      (error \"First allocation did not publish\"))
    (unless (typep (getf *shell-log-contention-fixture* :result) 'shell-log-artifact)
      (error \"First allocation failed: ~S\" (getf *shell-log-contention-fixture* :result))))")
               (evaluate second-worker
                         "(progn
    (unless (sb-thread:wait-on-semaphore (getf *shell-log-contention-fixture* :done) :timeout 30)
      (error \"Second allocation did not finish\"))
    (unless (eq :quota-refused (getf *shell-log-contention-fixture* :result))
      (error \"Concurrent allocators overcommitted the single reservation\")))"))
          (when second-worker (lisp-worker-stop second-worker))
          (when first-worker (lisp-worker-stop first-worker))))))
  nil)


;;;; -- Durable Completion Receipt Regressions --

(-> shell-log-tests--receipt-append (conversation string) t)
(defun shell-log-tests--receipt-append (conversation identifier)
  "Persist a completion identifier through the production message boundary."
  (conversation-append-user-message conversation "Completed."
                                    :pending-input-identifier identifier :automatic-p t))

(-> shell-log-tests--receipt-capture (configuration task-orchestrator conversation &key (:root string)) shell-log-artifact)
(defun shell-log-tests--receipt-capture (configuration orchestrator conversation &key root)
  "Publish one detached closed capture owned by CONVERSATION."
  (let* ((job     (task-tests--make-job orchestrator :identifier "receipt-capture"
                                                  :root-identifier root :detached-p t))
         (context (make-instance 'tool-context :configuration configuration :worker nil
                                             :conversation conversation)))
    (setf (slot-value job 'completion-owner-conversation) (conversation-identifier conversation))
    (let ((artifact (shell-log-allocate context :job job)))
      (shell-log-tests--finish artifact #(65))
      artifact)))

(-> shell-log-tests--receipt-prunable (configuration shell-log-artifact) boolean)
(defun shell-log-tests--receipt-prunable (configuration artifact)
  "Query the production delivery disjunction for one closed fixture capture."
  (shell-log--prunable-p configuration (shell-log-capture-directory artifact)
                        (shell-log-artifact-metadata artifact)))

(-> test-shell-log-receipt-owners () null)
(defun test-shell-log-receipt-owners ()
  "Recognize late root, child and nested-child receipts, never foreign authority."
  (with-test-configuration (configuration)
    (let* ((*shell-log-active*             (make-hash-table :test #'equal))
           (*shell-log-capture-byte-limit* 32)
           (runtime                        (task-orchestrator-create))
           (root                           (conversation-create configuration :identifier "receipt-root"))
           (root-agent                     (make-instance 'agent :configuration configuration :conversation root))
           (foreign                        (conversation-create configuration :identifier "receipt-foreign"))
           (parent                         root-agent))
      (unwind-protect
           (dotimes (depth 3)
             (let* ((job          (and (plusp depth)
                                       (task-tests--make-job runtime :identifier "receipt-child"
                                         :root-identifier "receipt-root" :parent-agent parent)))
                    (owner        (if job (task-job-execution-identifier job) "receipt-root"))
                    (conversation (if job
                                      (conversation-create configuration :identifier owner
                                        :storage-root                                (task--artifact-root configuration job)) root))
                    (artifact     (shell-log-tests--receipt-capture configuration runtime conversation :root "receipt-root"))
                    (receipt      (task-completion--receipt (getf (shell-log-artifact-metadata artifact) :execution-id))))
               (test-assert (not (shell-log-tests--receipt-prunable configuration artifact))
                            "unknown completion stays protected")
               (shell-log-tests--receipt-append foreign receipt)
               (when job
                 (shell-log-tests--receipt-append (conversation-create configuration :identifier owner) receipt))
               (test-assert (not (shell-log-tests--receipt-prunable configuration artifact))
                            "foreign transcript including same-ID primary is not authority")
               (shell-log-tests--receipt-append conversation "completion:wrong-execution")
               (test-assert (not (shell-log-tests--receipt-prunable configuration artifact))
                            "wrong execution receipt is not delivery")
               (shell-log-tests--receipt-append conversation receipt)
               (test-assert (shell-log-tests--receipt-prunable configuration artifact)
                            "exact late receipt permits closed capture pruning at every depth")
               (setf (getf (shell-log-artifact-metadata artifact) :state) :active)
               (test-assert (not (shell-log-tests--receipt-prunable configuration artifact))
                            "delivery never overrides active state")
               (when job
                 (setf parent (make-instance 'task-child-agent :configuration configuration
                                            :conversation conversation :job job :depth depth)))))
        (task-orchestrator-close runtime))))
  nil)

(-> shell-log-tests--newest-receipts (pathname) list)
(defun shell-log-tests--newest-receipts (identity)
  "Independently verify the supported newest-segment persistence invariant."
  (let* ((active   (conversation-storage-active-pathname identity))
         (header   (conversation-peek-header active))
         (receipts (copy-list (getf (rest header) :pending-input-identifiers))))
    (conversation--from-header identity active header)
    (conversation--map-segment-records
     identity active
     (lambda (record)
       (when (eq (first record) :message)
         (let ((receipt (getf (rest record) :pending-input-identifier)))
           (when receipt (pushnew receipt receipts :test #'equal))))))
    receipts))

(-> test-shell-log-receipt-rotation () null)
(defun test-shell-log-receipt-rotation ()
  "Execute two real rotations with reloads for modern and genuine v1 storage."
  (with-test-configuration (configuration)
    (dolist (legacy-p '(nil t))
      (let* ((conversation (conversation-create configuration :identifier (if legacy-p "legacy" "modern")))
             (identity     (conversation-pathname conversation))
             (receipts     '("completion:before" "completion:middle" "completion:after")))
        (shell-log-tests--receipt-append conversation (first receipts))
        (when legacy-p
          (let ((forms (copy-tree (conversation--read-records (conversation-log-pathname conversation)))))
            (setf (getf (rest (first forms)) :version) 1)
            (remf (rest (first forms)) :pending-input-identifiers)
            (conversation-identifier-migration--write-forms identity forms)
            (platform-delete-directory-tree *platform* (conversation-storage-directory-pathname identity)
                                            :validate t :if-does-not-exist :ignore))
          (setf conversation (conversation-load identity))
          (test-assert (member (first receipts) (conversation-pending-input-identifiers conversation) :test #'equal)
                       "v1 reload accumulates receipts from messages")
          (test-assert (member (first receipts) (shell-log-tests--newest-receipts identity) :test #'equal)
                       "v1 identity segment contains accumulated message receipt"))
        (loop for receipt in (rest receipts)
              do (let ((old (conversation-storage-active-pathname identity)))
                   (conversation-append-summary conversation "Real checkpoint")
                   (test-assert (not (equal old (conversation-storage-active-pathname identity)))
                                "production checkpoint rotates the physical segment")
                   (test-assert
                    (every (lambda (id) (member id (getf (rest (conversation-peek-header identity))
                                                        :pending-input-identifiers) :test #'equal))
                           (conversation-pending-input-identifiers conversation))
                    "new checkpoint header restates every accumulated receipt")
                   (setf conversation (conversation-load identity))
                   (shell-log-tests--receipt-append conversation receipt)))
        (test-assert (not (member (third receipts) (getf (rest (conversation-peek-header identity))
                                                       :pending-input-identifiers) :test #'equal))
                     "late receipt is deliberately absent from the header")
        (test-assert (equal (sort (copy-list (conversation-pending-input-identifiers (conversation-load identity))) #'string<)
                            (sort (shell-log-tests--newest-receipts identity) #'string<))
                     "validated newest segment and full replay agree after two rotations"))))
  nil)

(-> test-shell-log-receipt-validation () null)
(defun test-shell-log-receipt-validation ()
  "Validate the whole segment before publishing any receipt, including its tail."
  (with-test-configuration (configuration)
    (let ((*shell-log-capture-byte-limit* 32)
          (runtime                        (task-orchestrator-create)))
      (unwind-protect
           (dolist (kind '(:valid :header :v1 :torn :malformed :gap :checkpoint :identity :version :missing :owner :traversal))
             (let* ((conversation (conversation-create configuration :identifier (string-downcase (symbol-name kind))))
                    (artifact     (shell-log-tests--receipt-capture configuration runtime conversation
                                   :root                                                                (conversation-identifier conversation)))
                    (receipt      (task-completion--receipt (getf (shell-log-artifact-metadata artifact) :execution-id))))
               (shell-log-tests--receipt-append conversation receipt)
               (when (eq kind :header) (conversation-append-summary conversation "Header receipt"))
               (let* ((path  (conversation-storage-active-pathname (conversation-pathname conversation)))
                      (forms (copy-tree (conversation--read-records path))))
                 (case kind
                   (:v1
                    (setf (getf (rest (first forms)) :version) 1)
                    (remf (rest (first forms)) :pending-input-identifiers)
                    (conversation-identifier-migration--write-forms (conversation-pathname conversation) forms)
                    (platform-delete-directory-tree *platform*
                      (conversation-storage-directory-pathname (conversation-pathname conversation))
                      :validate t :if-does-not-exist ':ignore))
                   (:owner
                    (setf (getf (shell-log-artifact-metadata artifact) :completion-owner-conversation) nil))
                   (:traversal
                    (setf (getf (shell-log-artifact-metadata artifact) :completion-owner-conversation) "../foreign"))
                   (:torn
                    (with-open-file (s path :direction :output :if-exists :append)
                      (write-string "(:message :seq " s)))
                   (:malformed
                    (with-open-file (s path :direction :output :if-exists :append)
                      (write '(:message :seq 2 :role) :stream s) (terpri s)))
                   (:gap
                    (incf (getf (rest (second forms)) :seq))
                    (conversation-identifier-migration--write-forms path forms))
                   (:checkpoint
                    (conversation-append-summary conversation "Checkpoint")
                    (let* ((active  (conversation-storage-active-pathname (conversation-pathname conversation)))
                           (rotated (conversation--read-records active)))
                      (conversation-identifier-migration--write-forms active (list (first rotated)))))
                   (:identity
                    (setf (getf (rest (first forms)) :id) "another")
                    (conversation-identifier-migration--write-forms path forms))
                   (:version
                    (setf (getf (rest (first forms)) :version) 999)
                    (conversation-identifier-migration--write-forms path forms))
                   (:missing
                    (platform-delete-file *platform* path)))
                 (test-assert (eq (not (null (shell-log-tests--receipt-prunable configuration artifact)))
                                  (not (null (member kind '(:valid :header :v1 :torn)))))
                              (format nil "whole-segment receipt validation: ~A" kind)))))
        (task-orchestrator-close runtime))))
  nil)

(-> test-shell-log-receipt-pressure () null)
(defun test-shell-log-receipt-pressure ()
  "Prune verified root/child receipts under pressure, retaining an unknown capture."
  (with-test-configuration (configuration)
    (let* ((*shell-log-capture-byte-limit*    32)
           (*shell-log-retention-count-limit* 3)
           (runtime                           (task-orchestrator-create))
           (root                              (conversation-create configuration :identifier "pressure-root"))
           (child-job                         (task-tests--make-job runtime :identifier "pressure-child" :root-identifier "pressure-root"))
           (child                             (conversation-create configuration :identifier (task-job-execution-identifier child-job)
                                                                   :storage-root             (task--artifact-root configuration child-job))))
      (unwind-protect
           (let* ((unknown (shell-log-tests--receipt-capture configuration runtime root :root "pressure-root"))
                  (first   (shell-log-tests--receipt-capture configuration runtime root :root "pressure-root"))
                  (second  (shell-log-tests--receipt-capture configuration runtime child :root "pressure-root")))
             (dolist (pair (list (cons first root) (cons second child)))
               (shell-log-tests--receipt-append (rest pair)
                 (task-completion--receipt (getf (shell-log-artifact-metadata (first pair)) :execution-id))))
             (test-assert
              (handler-case (progn (shell-log-tests--receipt-capture configuration runtime root :root "pressure-root") t)
                (shell-log-error () nil))
              "allocation under count pressure reclaims receipt-backed closed captures")
             (test-assert (eq :closed (getf (shell-log--read-manifest configuration
                                             (shell-log-capture-directory unknown)) :state))
                          "pressure preserves genuinely unknown evidence"))
        (task-orchestrator-close runtime))))
  nil)

(-> test-shell-log-receipt-migration () null)
(defun test-shell-log-receipt-migration ()
  "Resolve a real migrated root alias through its authoritative canonical identity."
  (with-test-configuration (configuration)
    (let* ((*shell-log-capture-byte-limit* 32)
           (runtime                        (task-orchestrator-create))
           (old                            "cb472f21-969d-48f5-9c1e-e793d19054b9")
           (conversation                   (conversation-create configuration :identifier old)))
      (unwind-protect
           (let* ((artifact (shell-log-tests--receipt-capture configuration runtime conversation :root old))
                  (receipt  (task-completion--receipt (getf (shell-log-artifact-metadata artifact) :execution-id))))
             (shell-log-tests--receipt-append conversation receipt)
             (let* ((entries   (conversation-identifier-migrate configuration))
                    (entry     (conversation-identifier-migration--entry-for-old old entries))
                    (canonical (getf entry :new)))
               (test-assert (and canonical (not (equal old canonical))) "real migration publishes a new root identifier")
               (test-assert (equal canonical (pathname-name (conversation-pathname-for-id configuration old)))
                            "authoritative root lookup resolves the old alias")
               (test-assert (shell-log-tests--receipt-prunable configuration artifact)
                            "old capture metadata recognizes receipt in migrated root")))
        (task-orchestrator-close runtime))))
  nil)

(-> test-shell-log-receipt-cache () null)
(defun test-shell-log-receipt-cache ()
  "A new prune sees appended receipts after an earlier conservative cache miss."
  (with-test-configuration (configuration)
    (let* ((*shell-log-capture-byte-limit*    32)
           (*shell-log-retention-count-limit* 1)
           (runtime                           (task-orchestrator-create))
           (conversation                      (conversation-create configuration :identifier "cached-root")))
      (unwind-protect
           (let* ((artifact                  (shell-log-tests--receipt-capture configuration runtime conversation :root "cached-root"))
                  (receipt                   (task-completion--receipt (getf (shell-log-artifact-metadata artifact) :execution-id)))
                  (*shell-log-receipt-cache* (make-hash-table :test #'equal)))
             (declare (special *shell-log-receipt-cache*))
             (test-assert (not (shell-log-tests--receipt-prunable configuration artifact))
                          "first lookup has no verified evidence")
             (shell-log-tests--receipt-append conversation receipt)
             (test-assert (not (shell-log-tests--receipt-prunable configuration artifact))
                          "one prune-local snapshot may conservatively retain its negative result")
             (let ((*shell-log-receipt-cache* nil))
               (declare (special *shell-log-receipt-cache*))
               (test-assert (shell-log-tests--receipt-prunable configuration artifact)
                            "standalone lookup reads fresh delivery evidence"))
             (test-assert (handler-case (progn (shell-log-tests--receipt-capture configuration runtime conversation :root "cached-root") t)
                            (shell-log-error () nil))
                          "new prune overrides a stale caller cache and admits replacement allocation"))
        (task-orchestrator-close runtime))))
  nil)

(-> test-shell-log-receipt-linked-segment () null)
(defun test-shell-log-receipt-linked-segment ()
  "Reject linked transcript segments even if the target contains exact receipts."
  (with-test-configuration (configuration)
    (with-test-fixture (':symbolic-links "completion receipt segment substitution")
      (let* ((*shell-log-capture-byte-limit* 32)
             (runtime                        (task-orchestrator-create))
             (conversation                   (conversation-create configuration :identifier "linked-root")))
        (unwind-protect
             (let* ((artifact (shell-log-tests--receipt-capture configuration runtime conversation :root "linked-root"))
                    (receipt  (task-completion--receipt (getf (shell-log-artifact-metadata artifact) :execution-id))))
               (shell-log-tests--receipt-append conversation receipt)
               (conversation-append-summary conversation "Header witness")
               (let* ((active (conversation-storage-active-pathname (conversation-pathname conversation)))
                      (target (merge-pathnames "foreign-segment.sexp" (config :data-root configuration))))
                 (uiop:copy-file active target)
                 (platform-delete-file *platform* active)
                 (test-fixture-make-symbolic-link *platform* (namestring target) (namestring active))
                 (test-assert (not (shell-log-tests--receipt-prunable configuration artifact))
                              "linked receipt segment cannot authorize pruning")))
          (task-orchestrator-close runtime)))))
  nil)

(-> test-shell-log-receipt-large-history () null)
(defun test-shell-log-receipt-large-history ()
  "Resolve header and late receipts amid a large validated owner history."
  (with-test-configuration (configuration)
    (let* ((*shell-log-capture-byte-limit* 32)
           (runtime                        (task-orchestrator-create))
           (conversation                   (conversation-create configuration :identifier "large-receipts")))
      (unwind-protect
           (let* ((early   (shell-log-tests--receipt-capture configuration runtime conversation :root "large-receipts"))
                  (late    (shell-log-tests--receipt-capture configuration runtime conversation :root "large-receipts"))
                  (unknown (shell-log-tests--receipt-capture configuration runtime conversation :root "large-receipts"))
                  (receipt (task-completion--receipt (getf (shell-log-artifact-metadata early) :execution-id))))
             (shell-log-tests--receipt-append conversation receipt)
             (let* ((path   (conversation-log-pathname conversation))
                    (forms  (conversation--read-records path))
                    (header (first forms))
                    (seed   (second forms)))
               (setf (getf (rest header) :pending-input-identifiers)
                     (cons receipt (loop for index below 15000 collect (format nil "completion:header-~D" index))))
               (setf forms
                     (cons header
                           (loop for sequence from 1 to 15000
                                 for record = (copy-tree seed)
                                 do (setf (getf (rest record) :seq) sequence
                                          (getf (rest record) :pending-input-identifier)
                                          (if (= sequence 15000)
                                              (task-completion--receipt (getf (shell-log-artifact-metadata late) :execution-id))
                                              (format nil "completion:message-~D" sequence)))
                                 collect record)))
               (conversation-identifier-migration--write-forms path forms)
               (let ((*shell-log-receipt-cache* (make-hash-table :test #'equal)))
                 (test-assert (shell-log-tests--receipt-prunable configuration early)
                              "A large checkpoint header proves exact earlier delivery")
                 (test-assert (shell-log-tests--receipt-prunable configuration late)
                              "A late receipt after thousands of messages proves delivery")
                 (test-assert (not (shell-log-tests--receipt-prunable configuration unknown))
                              "Large receipt sets do not acknowledge unknown executions"))
               (push receipt (getf (rest header) :pending-input-identifiers))
               (conversation-identifier-migration--write-forms path forms)
               (test-assert (not (shell-log-tests--receipt-prunable configuration early))
                            "Duplicate checkpoint identifiers invalidate all receipt evidence")))
        (task-orchestrator-close runtime))))
  nil)
