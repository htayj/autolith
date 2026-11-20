(in-package #:autolith)

;;;; -- Task Conditions --

(define-condition task-error
    (tool-error)
  ((task-id
    :initarg       :task-id
    :initform      nil
    :reader        task-error-task-id
    :type          (option string)
    :documentation "The child or job identifier involved in the failure, when known."))
  (:documentation
   "A task request, child run, or task job violated its contract."))

(define-condition task-agent-definition-error
    (task-error)
  ((pathname
    :initarg       :pathname
    :initform      nil
    :reader        task-agent-definition-error-pathname
    :type          (option pathname)
    :documentation "The external role file containing the invalid definition, when any.")
   (source
    :initarg       :source
    :reader        task-agent-definition-error-source
    :type          keyword
    :documentation "The project, user, site, bundled, or programmatic definition origin.")
   (line
    :initarg       :line
    :initform      nil
    :reader        task-agent-definition-error-line
    :type          (option (integer 1))
    :documentation "The source line nearest the invalid value, when known.")
   (field
    :initarg       :field
    :initform      nil
    :reader        task-agent-definition-error-field
    :type          (option keyword)
    :documentation "The native plist field whose contract was violated, when known.")
   (cause
    :initarg       :cause
    :reader        task-agent-definition-error-cause
    :type          t
    :documentation "The original condition or concise structural failure.")
   (definition-name
    :initarg       :definition-name
    :initform      nil
    :reader        task-agent-definition-error-definition-name
    :type          (option string)
    :documentation "The normalized role basename reserved by this diagnostic."))
  (:documentation "A native child-role definition is unsafe or malformed.")
  (:report
   (lambda (condition stream)
     (format stream "Invalid ~A task agent~@[ ~S~]~@[ in ~A~]~@[ at line ~D~]~@[, field ~S~]: ~A"
             (string-downcase
              (symbol-name (task-agent-definition-error-source condition)))
             (task-agent-definition-error-definition-name condition)
             (task-agent-definition-error-pathname condition)
             (task-agent-definition-error-line condition)
             (task-agent-definition-error-field condition)
             (task-agent-definition-error-cause condition)))))


;;;; -- Native Definition Bounds --

(defparameter *task-agent-file-maximum-bytes* 131072
  "The largest external child-role file accepted by the native reader.")

(defparameter *task-agent-form-maximum-nodes* 8192
  "The largest readable object tree accepted from one role file.")

(defparameter *task-agent-form-maximum-depth* 128
  "The deepest list nesting accepted in a native child-role file.")

(defparameter *task-agent-string-maximum-characters* 32768
  "The largest individual string accepted in a role form.")

(defparameter *task-agent-name-maximum-characters* 64
  "The maximum normalized child-role name length.")

(defparameter *task-agent-description-maximum-characters* 512
  "The maximum child-role description length.")

(defparameter *task-agent-instructions-maximum-characters* 32768
  "The maximum child-role instruction body length.")

(defparameter *task-agent-definition-fields*
  '(:name :description :instructions :tools :spawns :models
    :reasoning-effort)
  "The complete native child-role plist vocabulary.")

(defparameter *task-forbidden-child-tool-namespaces*
  '("self" "child")
  "Tool namespaces structurally unavailable to ordinary child-role grants.")

(defparameter *task-model-aliases*
  '("@task" "@parent" "@auto" "@smol" "@slow" "@designer")
  "The model aliases accepted by child-role definitions.")

(defparameter *task-agent-native-keyword-names*
  '("name" "description" "instructions" "tools" "spawns" "models"
    "reasoning-effort" "type" "enum" "properties"
    "required" "additional-properties" "items" "min-items" "max-items"
    "all" "auto" "object" "array" "string" "number" "integer" "boolean"
    "null")
  "Native child-role keywords other than live reasoning-effort names.")

(-> task-agent-native-keyword-name-p (string) boolean)
(defun task-agent-native-keyword-name-p (name)
  "Return true when NAME is accepted by the native child-role reader."
  (not
   (null
    (or (member name *task-agent-native-keyword-names* :test #'string-equal)
        (member name *supported-reasoning-efforts* :test #'string-equal)))))


;;;; -- Contract Diagnostics --

(-> task-agent-definition--error
    (&key (:pathname (option pathname))
          (:source keyword)
          (:line (option (integer 1)))
          (:field (option keyword))
          (:cause t)
          (:definition-name (option string)))
    null)
(defun task-agent-definition--error
    (&key pathname source line field cause definition-name)
  "Signal a structured child-role diagnostic with source context."
  (let ((message
          (format nil "Invalid ~A task agent~@[ ~S~]~@[ in ~A~]~@[ at line ~D~]~@[, field ~S~]: ~A"
                  (string-downcase (symbol-name source))
                  definition-name pathname line field cause)))
    (error 'task-agent-definition-error
           :message         message
           :tool-name       "child.spawn"
           :pathname        pathname
           :source          source
           :line            line
           :field           field
           :cause           cause
           :definition-name definition-name)))

(-> task--trim (string) string)
(defun task--trim (text)
  "Return TEXT without surrounding horizontal or line whitespace."
  (string-trim '(#\Space #\Tab #\Newline #\Return) text))

(-> task--split-lines (string) list)
(defun task--split-lines (text)
  "Return TEXT as a list of lines without newline characters."
  (loop with start = 0
        for end = (position #\Newline text :start start)
        collect (string-right-trim
                 '(#\Return)
                 (subseq text start (or end (length text))))
        while end
        do (setf start (1+ end))))

(-> task--plist-key-present-p (list keyword) boolean)
(defun task--plist-key-present-p (plist key)
  "Return true when proper PLIST contains KEY in a key position."
  (loop for tail on plist by (partial #'nthcdr 2)
        thereis (eq (first tail) key)))

(-> task--plist-alist
    (t list &key (:pathname (option pathname))
                 (:source keyword)
                 (:line (option (integer 1)))
                 (:definition-name (option string)))
    list)
(defun task--plist-alist
    (value allowed-fields &key pathname source line definition-name)
  "Validate native plist VALUE and return its ordered key-value pairs."
  (multiple-value-bind (problem key)
      (plist-schema-problem value :allowed-keys allowed-fields)
    (when problem
      (task-agent-definition--error
       :pathname pathname :source source :line line
       :field           (and (member problem '(:unknown :duplicate)) key)
       :cause           (case problem
                          (:improper
                           "The value must be a proper list.")
                          (:odd
                           "The property list has a key without a value.")
                          (:non-keyword
                           (format nil "Property key ~S is not a keyword." key))
                          (:unknown
                           "The property is not part of the native role contract.")
                          (:duplicate
                           "The property occurs more than once."))
       :definition-name definition-name)))
  (loop for (key child) on value by (partial #'nthcdr 2) collect (cons key child)))

(-> task--alist-value (keyword list) (values t boolean))
(defun task--alist-value (key pairs)
  "Return KEY's value and presence flag from ordered PAIRS."
  (let ((pair (assoc key pairs :test #'eq)))
    (values (and pair (rest pair)) (and pair t))))

(-> task--unique-list-p (list &key (:test function)) boolean)
(defun task--unique-list-p (values &key (test #'equal))
  "Return true when VALUES has no duplicate elements under TEST."
  (loop for tail on values
        always (not (member (first tail) (rest tail) :test test))))


;;;; -- Structured Output Adapters --

(-> task-output--json-number-p (t) boolean)
(defun task-output--json-number-p (value)
  "Check numeric values at native role and task-result boundaries."
  (cl-llm-provider-api:output-json-number-p value))

(-> task-output-schema-normalize
    (t &key (:pathname (option pathname)) (:source keyword)
       (:definition-name (option string))) list)
(defun task-output-schema-normalize
    (schema &key pathname (source ':programmatic) definition-name)
  "Validate native output SCHEMA with child-role provenance."
  (handler-case
      (cl-llm-provider-api:output-schema-normalize
       schema
       :maximum-nodes       *task-agent-form-maximum-nodes*
       :maximum-depth       *task-agent-form-maximum-depth*
       :property-name-limit *task-agent-string-maximum-characters*)
    (cl-llm-provider-api:output-contract-error (condition)
      (task-agent-definition--error
       :pathname pathname :source source :definition-name definition-name
       :field (cl-llm-provider-api:output-contract-error-field condition)
       :cause (cl-llm-provider-api:provider-api-error-message condition)))))

(-> task-output-schema->json (list) json-object)
(defun task-output-schema->json (schema)
  "Project a validated task output contract into provider JSON Schema."
  (cl-llm-provider-api:output-schema->json schema))

(-> task-json-decode (string &key (:tool-name string)) json-value)
(defun task-json-decode (source &key (tool-name "task"))
  "Decode exact JSON arguments and attach canonical tool failure metadata."
  (handler-case
      (cl-llm-provider-api:output-json-decode source)
    (cl-llm-provider-api:output-value-error (condition)
      (error 'task-error :tool-name tool-name
             :message               (cl-llm-provider-api:provider-api-error-message condition)))))

(-> task-json-normalize (t &key (:tool-name string)) json-value)
(defun task-json-normalize (arguments &key (tool-name "task"))
  "Detach native arguments using the task decoder's output marker policy."
  (handler-case
      (json-normalize arguments :input-false-value (json-false)
                                :false-value 'yason:false :null-value ':null)
    (error ()
      (error 'task-error :tool-name tool-name
                        :message    "Could not decode the JSON value."))))

(-> task--character-strings (t) t)
(defun task--character-strings (value)
  "Copy string leaves in a bounded acyclic cons tree into fresh character strings.

Fresh strings avoid implementation array syntax and shared-string reader labels.
Repeated subtrees are copied independently; cyclic conses signal TASK-ERROR."
  (handler-case
      (portable-value-map
       value
       (lambda (item child)
         (declare (ignore child))
         (if (stringp item)
             (make-array (length item) :element-type 'character :initial-contents item)
             item)))
    (portable-value-error ()
      (error 'task-error :tool-name "child.spawn"
             :message               "Task result data is cyclic or exceeds its structural bounds."))))

(-> task--write-readable-sexp (t &key (:pretty-p boolean)) string)
(defun task--write-readable-sexp (value &key pretty-p)
  "Return VALUE as one portable readable s-expression."
  (with-standard-io-syntax
    (write-to-string (task--character-strings value)
                     :readably t
                     :escape   t
                     :circle   nil
                     :pretty   pretty-p)))


;;;; -- Readable Character Measurement --

(defclass task-character-count-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((count
    :initform      0
    :accessor      task-character-count-stream-count
    :documentation "The number of characters written, without retained text.")
   (column
    :initform      0
    :accessor      task-character-count-stream-column
    :documentation "Characters since the last newline, as in a string output stream."))
  (:documentation "A synchronous counting sink for non-pretty task measurements."))

(defclass task-readable-string-proxy ()
  ((value
    :initarg       :value
    :reader        task-readable-string-proxy-value
    :documentation "The original string leaf, borrowed only during measurement."))
  (:documentation "A temporary printer view of a normalized CHARACTER string."))

(defmethod trivial-gray-streams:stream-write-char
    ((stream task-character-count-stream) character)
  "Count CHARACTER and update the string-stream column."
  (incf (task-character-count-stream-count stream))
  (setf (task-character-count-stream-column stream)
        (if (char= character #\Newline)
            0
            (1+ (task-character-count-stream-column stream))))
  character)

(defmethod trivial-gray-streams:stream-write-string
    ((stream task-character-count-stream) string &optional (start 0) end)
  "Count the selected range without copying STRING, including empty ranges."
  (let* ((end     (or end (length string)))
         (newline (position #\Newline string :start start :end end :from-end t)))
    (incf (task-character-count-stream-count stream) (- end start))
    (setf (task-character-count-stream-column stream)
          (if newline
              (- end newline 1)
              (+ (task-character-count-stream-column stream) (- end start)))))
  string)

(defmethod trivial-gray-streams:stream-line-column ((stream task-character-count-stream))
  "Return the logical column used by line-sensitive atom printers."
  (task-character-count-stream-column stream))

(defmethod print-object ((proxy task-readable-string-proxy) stream)
  "Emit normalized quoted string syntax without a CHARACTER-array copy."
  (write-char #\" stream)
  (loop for character across (task-readable-string-proxy-value proxy)
        do (when (or (char= character #\") (char= character #\\))
             (write-char #\\ stream))
           (write-char character stream))
  (write-char #\" stream)
  proxy)

(-> task--readable-sexp-character-count (t) (integer 0))
(defun task--readable-sexp-character-count (value)
  "Count normalized non-pretty readable VALUE without retaining its text.

Direct string visits borrow leaves through private proxies. Other atoms use
their ordinary printers, whose internal allocation is not bounded here."
  (with-standard-io-syntax
    (let ((mapped
            (handler-case
                (portable-value-map
                 value
                 (lambda (item child)
                   (declare (ignore child))
                   (if (stringp item)
                       (make-instance 'task-readable-string-proxy :value item)
                       item)))
              (portable-value-error ()
                (error 'task-error :tool-name "child.spawn"
                       :message               "Task result data is cyclic or exceeds its structural bounds.")))))
      (with-open-stream (stream (make-instance 'task-character-count-stream))
        (write mapped :stream stream :readably t :escape t :circle nil :pretty nil)
        (task-character-count-stream-count stream)))))
