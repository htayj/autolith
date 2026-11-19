(in-package #:autolith)

;;;; -- Configured Candidate Reviewer Contract --

(defparameter *mission-draft-review-output-schema*
  '(:type :object
    :properties
    (("verdict" (:type :string :enum ("pass" "fail")))
     ("summary" (:type :string))
     ("coverage" (:type :array :max-items 64
                     :items (:type                  :object
                             :properties
                             (("gate" (:type :string))
                              ("verdict" (:type :string :enum ("pass" "fail")))
                              ("reason" (:type :string)))
                             :required              ("gate" "verdict" "reason")
                             :additional-properties nil))))
    :required ("verdict" "summary" "coverage") :additional-properties nil)
  "Explicit specification review data, never an executed acceptance gate proof.")

(-> mission-draft-review-definition (configuration) (values task-agent-definition list))
(defun mission-draft-review-definition (configuration)
  "Narrow the effective configured reviewer without changing its models or effort.
Return a private definition and its persistable identity snapshot. Discovery and
its precedence diagnostics are identical to ordinary task role discovery. An
unusable model list is refused before the native scheduler can inherit the
primary model. This preparation grants no candidate admission or work authority."
  (multiple-value-bind (definitions diagnostics) (task-discover-agents configuration)
    (let* ((diagnostic (task-find-agent-diagnostic diagnostics "reviewer"))
           (configured (task-find-agent-definition definitions "reviewer")))
      (when diagnostic (error diagnostic))
      (unless configured
        (mission--reject ':review-configuration "No configured reviewer role is available."))
      (let* ((models (task-agent-definition-models configured))
             (model  (loop for alias in models
                           thereis (task--model-alias alias nil))))
        (unless (and model
                     (notany (lambda (alias)
                               (member alias '("@parent" "@auto" "@task") :test #'string-equal))
                             models))
          (mission--reject ':review-configuration
                           "Configured review requires an independently resolved model; primary-model fallback is unavailable."))
        (let* ((definition
                 (task-agent-definition-create
                  :name             (task-agent-definition-name configured)
                  :description      (task-agent-definition-description configured)
                  :instructions
                  (format nil "~A~%Review the supplied candidate specification. Return an explicit pass or fail and cover every supplied gate exactly once. Coverage assesses the specification and never proves execution. Submit the required native output through yield.submit."
                          (task-agent-definition-instructions configured))
                  :source           (task-agent-definition-source configured)
                  :pathname         (task-agent-definition-pathname configured)
                  :models           (copy-list models)
                  :reasoning-effort (task-agent-definition-reasoning-effort configured)
                  :tools nil :spawns nil :blocking-p t
                  :output           *mission-draft-review-output-schema*))
               (child (task-configuration-for-definition configuration definition)))
          (values definition
                  (list :name                        (task-agent-definition-name configured)
                        :source                      (task-agent-definition-source configured)
                        :pathname                    (task-agent-definition-pathname configured)
                        :models                      (copy-list models)
                        :configured-reasoning-effort (task-agent-definition-reasoning-effort configured)
                        :model                       (config :model child)
                        :reasoning-effort            (config :reasoning-effort child)
                        :instructions-digest
                        (mission-draft--digest (list (task-agent-definition-instructions configured)))
                        :tools nil :spawns nil)))))))

(-> mission-draft-review-verdict (list list) list)
(defun mission-draft-review-verdict (result gate-ids)
  "Validate a complete native artifact and exact per-gate specification coverage.
Return the full explicit decision and coverage as data. This function does not
publish PASS, mutate a candidate, approve a start, or execute acceptance gates."
  (unless (and (proper-list-p gate-ids) (<= (length gate-ids) 64)
               (every #'non-empty-string-p gate-ids)
               (= (length gate-ids) (length (remove-duplicates gate-ids :test #'equal))))
    (mission--reject ':review-coverage "Expected review gate identities must be unique bounded strings."))
  (unless (and (eq (getf result :status) ':success)
               (getf result :structured-output-present-p))
    (mission--reject ':review-failed "Review requires a successful complete native output artifact."))
  (let* ((data   (task-sexp->json (getf result :structured-output)))
         (schema (task-output-schema-normalize *mission-draft-review-output-schema*)))
    (unless (task-output-schema-valid-p data schema)
      (mission--reject ':review-contract "Review requires an explicit verdict, summary and per-gate coverage."))
    (let* ((verdict  (gethash "verdict" data))
           (coverage (coerce (gethash "coverage" data) 'list))
           (names    (mapcar (lambda (entry) (gethash "gate" entry)) coverage)))
      (unless (and (non-empty-string-p (gethash "summary" data))
                   (= (length names) (length gate-ids))
                   (= (length names) (length (remove-duplicates names :test #'equal)))
                   (subsetp gate-ids names :test #'equal)
                   (every (lambda (entry) (non-empty-string-p (gethash "reason" entry))) coverage)
                   (or (equal verdict "fail")
                       (every (lambda (entry) (equal "pass" (gethash "verdict" entry))) coverage)))
        (mission--reject ':review-coverage "Review coverage is incomplete, duplicated, foreign or inconsistent with PASS."))
      (list :verdict verdict :summary (gethash "summary" data)
            :coverage (mapcar (lambda (entry)
                                (list :gate    (gethash "gate" entry)
                                      :verdict (gethash "verdict" entry)
                                      :reason  (gethash "reason" entry)))
                              coverage)
            :result   (copy-tree result)))))


;;;; -- Durable Host Claims and Native Admission --
