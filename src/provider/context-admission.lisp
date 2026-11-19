(in-package #:autolith)

;;;; -- Request Context Admission --


(-> provider-effective-context-window (model-provider) (integer 1))
(defun provider-effective-context-window (provider)
  "Return the window PROVIDER's requests are checked against.

That is the configured window. A compaction request is checked against the
window the model takes when that one is larger: compacting is how a
conversation gets back under the configured window."
  (let ((configured (config :context-window (provider-configuration provider)))
        (model      (and (eq provider *compaction-provider*)
                         (provider-compaction-recovery-window provider))))
    (if (integerp model)
        (max configured model)
        configured)))

(defparameter *provider-context-admission-state* nil
  "Request-local admission state shared with mission accounting.

FIRST is :PENDING until a provider admits the request, then :ENTERED.
REST holds the condition from a local refusal. Function entry and condition
class do not establish zero usage.")

(defparameter *provider-image-token-estimate* nil
  "Optional portable admission allowance per image, independent of paid usage.

NIL follows the current local image-preparation patch cap dynamically. This
fallback reflects locally prepared images, not measured costs for arbitrary
remote or MCP images or every model's image-token multiplier. Bind an explicit
allowance or specialize provider accounting for those policies. Base64 bytes
and URL length are never charged as text tokens.")

(defgeneric provider-request-input-token-estimate (provider request)
  (:documentation
   "Return REQUEST's estimated input tokens and whether non-text estimates were needed.

REQUEST is the final encoded request, including selected tools and temporary
context. This local admission estimate is independent of paid usage. Methods
may use provider token counts when available; the default is not a tokenizer.
Optional third/fourth values are the estimate mode and failed receipt reason;
an omitted mode denotes an unsupported extension, not a character estimate."))

(defmethod provider-request-input-token-estimate ((provider model-provider) (request hash-table))
  "Use matched ordinary usage, otherwise estimate the final encoded prompt."
  (multiple-value-bind (tokens matched-p uncertain-p)
      (provider-context--matched-input provider request)
    (if matched-p
        (values tokens uncertain-p :retained-usage)
        (multiple-value-bind (estimated uncertain-p)
            (provider-context--input-tokens provider nil (provider-request-body provider request))
          (values estimated uncertain-p :character/image)))))

(-> provider-context--soft-header (model-provider vector) (option list))
(defun provider-context--soft-header (provider tools)
  "Fingerprint prompt policy and tools when the provider owns a configuration."
  (let ((configuration (and (compute-applicable-methods #'provider-configuration (list provider))
                            (provider-configuration provider))))
    (when configuration
      (provider-cache-prefix--entry
       (vector (symbol-name (class-name (class-of provider)))
               (config :model configuration) (config :model-provider configuration)
                (config :reasoning-effort configuration) (config :provider-endpoint configuration)
               (system-prompt configuration) tools)))))

(-> provider-context--header-components (model-provider vector) list)
(defun provider-context--header-components (provider tools)
  "Separate stable accounting policy from replaceable prompt and tool text."
  (let ((configuration (and (compute-applicable-methods #'provider-configuration (list provider))
                            (provider-configuration provider))))
    (when configuration
      (let ((prompt (system-prompt configuration)))
        (list :policy      (provider-cache-prefix--entry
                            (vector (symbol-name (class-name (class-of provider)))
                                    (config :model configuration) (config :model-provider configuration)
                                    (config :reasoning-effort configuration) (config :provider-endpoint configuration)))
              :prompt      (provider-cache-prefix--entry prompt)
              :tool-header (provider-cache-prefix--entry tools)
              :header-text (list prompt (json-encode tools)))))))

(-> provider-context--header-charge (list list) (integer 0))
(defun provider-context--header-charge (sample components)
  "Charge one token per inserted or replacement UTF-8 byte of changed headers."
  (loop for key in '(:prompt :tool-header)
        for previous in (or (getf sample :header-text) '("" ""))
        for current in (getf components :header-text)
        unless (equal (getf sample key) (getf components key))
          sum (cl-llm-provider-api:context-inserted-utf8-bytes previous current)))

(-> provider-context--history (model-provider conversation) list)
(defun provider-context--history (provider conversation)
  "Fingerprint compatible retained history without storing prompt bodies."
  (mapcar #'provider-cache-prefix--entry
          (provider-history-items provider conversation :include-ephemeral-p nil)))

(-> provider-context--extends-p (list list) boolean)
(defun provider-context--extends-p (previous current)
  "Return whether CURRENT preserves every entry of PREVIOUS, including empty items."
  (and (<= (length previous) (length current))
       (equal previous (subseq current 0 (length previous))) t))

(-> provider-context-request-create (model-provider conversation vector) list)
(defun provider-context-request-create (provider conversation tools)
  "Start ordinary request-local evidence; ephemera require the unmeasured fallback."
  (let ((header (provider-context--soft-header provider tools)))
    ;; Preallocate mutable fields to preserve the shared evidence plist head.
    (let ((context
            (append (and header (provider-context--header-components provider tools))
                    (list :provider provider :conversation conversation
                          :generation (conversation-input-measurement-generation conversation)
                          :history (and header (provider-context--history provider conversation))
                          :soft-header header
                          :eligible-p (and header (null (conversation-ephemeral-input-entries conversation)) t)
                          :encoder nil :projection nil :tools nil :wire-provider nil :wire nil
                          :wire-characters 0 :route nil))))
      (with-recursive-lock-held ((conversation-append-lock conversation))
        (setf (conversation-context-usage-request conversation) context))
      context)))


(-> provider-context--wire-plan (model-provider hash-table) list)
(defun provider-context--wire-plan (provider request)
  "Fingerprint the encoder's measured-input projection."
  (provider-cache-wire-plan provider (provider-request-context-body provider request)))

(-> provider-context--prefix-matches-p (list (integer 0) list) boolean)
(defun provider-context--prefix-matches-p (fingerprint count entries)
  "Verify COUNT retained ENTRIES against a portable prefix FINGERPRINT."
  (and (<= count (length entries))
       (equal fingerprint
              (provider-cache-prefix--entry (coerce (subseq entries 0 count) 'vector)))
       t))

(-> provider-context--sample-matches-p (list (option list) list &key (:components list)) boolean)
(defun provider-context--sample-matches-p (sample header history &key components)
  "Verify measured history under exact headers or a format-two stable policy."
  (and (conversation-context-usage-sample-p sample)
       header
       (or (equal (getf sample :soft-header) header)
           (and (= 2 (getf sample :format)) components
                (equal (getf sample :policy) (getf components :policy))))
       (provider-context--prefix-matches-p
        (getf sample :history-digest) (getf sample :history-count) history)
       t))

(-> provider-context--switch-source-model (codex-subscription-provider list) (option string))
(defun provider-context--switch-source-model (provider sample)
  "Identify the measured model under the same Codex endpoint and provider."
  (let* ((configuration (provider-configuration provider))
         (registration  (model-provider-registration provider))
         (name          (config :model-provider configuration))
         (model-digest  (rest (assoc "model" (getf sample :encoder-fields) :test #'equal))))
    (when (and registration model-digest)
      (loop for candidate in (provider-registration-models registration)
            for model = (provider-model-name candidate)
            when (and (equal model-digest (provider-cache-prefix--entry model))
                      (loop for effort in (configuration--reasoning-efforts-for model name)
                            thereis (equal (getf sample :policy)
                                           (provider-cache-prefix--entry
                                            (vector (symbol-name (class-name (class-of provider)))
                                                    model name effort
                                                    (config :provider-endpoint configuration))))))
              return model))))

(-> provider-context--switch-history (model-provider conversation &key (:sample list) (:current list))
    (values list boolean))
(defun provider-context--switch-history (provider conversation &key sample current)
  "Check which CURRENT items are covered by a compatible Codex measurement.

Verify the original measured prefix. Only sealed reasoning may disappear from
that prefix. Count every new item and take no credit for removed reasoning."
  (block nil
    (unless (and (typep provider 'codex-subscription-provider)
                 (conversation-context-usage-sample-p sample)
                 (= 2 (getf sample :format)))
      (return (values nil nil)))
    (let* ((model (provider-context--switch-source-model provider sample))
           (count (getf sample :history-count))
           (old   (and model
                       (conversation-input-items-for-family
                        conversation (provider-family provider) :include-ephemeral-p nil :model model))))
      (unless (and model (plusp count) (<= count (getf sample :wire-count))
                   (provider-context--prefix-matches-p
                    (getf sample :history-digest) count
                    (mapcar #'provider-cache-prefix--entry old)))
        (return (values nil nil)))
      (let* ((model-changed-p (not (equal model (config :model (provider-configuration provider)))))
             (prefix          (subseq old 0 count))
             (covered         (if model-changed-p
                                  (remove-if #'encrypted-reasoning-item-p prefix)
                                  prefix))
             (added           nil))
        (dolist (item current)
          (cond
            ((and covered (eq item (first covered)))
             (pop covered))
            ((or (null covered) (and model-changed-p (encrypted-reasoning-item-p item)))
             (push item added))
            (t
             (return (values nil nil)))))
        (if covered
            (values nil nil)
            (values (nreverse added) t))))))

(-> provider-context--request-items (model-provider hash-table) (option vector))
(defun provider-context--request-items (provider request)
  "Return the encoder's declared conversation sequence."
  (values (provider-request-history provider request)))

(-> provider-context--input-tokens ((option model-provider) list t &key (:replaced-characters (integer 0))) (values (integer 0) boolean))
(defun provider-context--input-tokens (provider sample additions &key (replaced-characters 0))
  "Retain measured occupancy, charge additions and replace measured temporary context."
  (multiple-value-bind (characters uncertain-p)
      (if additions (provider-context--request-characters provider additions) (values 0 nil))
    (values (max 0 (+ (getf sample :tokens 0)
                      (ceiling (- characters
                                  (if (and sample (vectorp additions)
                                           (not (stringp additions))) 2 0)
                                  replaced-characters)
                               4)))
            uncertain-p)))

(-> provider-context--current-wire-p (list hash-table list) boolean)
(defun provider-context--current-wire-p (context request sample)
  "Reproduce current encoding while retaining measured transport-only fields."
  (let ((adapter    (getf context :encoder))
        (projection (getf context :projection))
        (tools      (getf context :tools)))
    (when (and adapter projection tools)
      (let* ((*provider-context-request*       nil)
             (*provider-cache-prefix-function* nil)
             (encoded                          (provider-context--wire-plan
                                                adapter (provider-request-object adapter projection tools)))
             (current                          (provider-context--wire-plan (getf context :provider) request))
             (transport-fields
               (remove-if (lambda (field)
                            (equal field (assoc (first field) (getf sample :encoder-fields) :test #'equal)))
                          (getf sample :wire-fields)))
             (expected-fields
               (append transport-fields
                       (remove-if (lambda (field)
                                    (assoc (first field) transport-fields :test #'equal))
                                  (getf encoded :fields)))))
        (and (equal (getf encoded :items) (getf current :items))
             (= (length expected-fields) (length (getf current :fields)))
             (every (lambda (field)
                      (equal field (assoc (first field) (getf current :fields) :test #'equal)))
                    expected-fields)
             t)))))

(-> provider-context--switched-field-charge (model-provider hash-table list) (integer 0))
(defun provider-context--switched-field-charge (provider request sample)
  "Charge changed encoded fields that can contribute new context."
  (let ((body (provider-request-context-body provider request)))
    (multiple-value-bind (history history-field) (provider-request-history provider body)
      (declare (ignore history))
      (loop for name being the hash-keys of (provider-request-body provider body) using (hash-value value)
            unless (or (equal name history-field)
                       (member name '("model" "reasoning") :test #'equal)
                       (equal (provider-cache-prefix--entry value)
                              (rest (assoc name (getf sample :wire-fields) :test #'equal))))
              sum (let ((old-prompt (first (getf sample :header-text))))
                    (if (and (equal name "instructions") (stringp value) old-prompt
                             (equal (provider-cache-prefix--entry old-prompt)
                                    (rest (assoc name (getf sample :wire-fields) :test #'equal))))
                        (provider-context--inserted-bytes old-prompt value)
                        (utf8-string-length (json-encode value))))))))

(-> provider-context--switched-input (model-provider hash-table &key (:context list) (:sample list))
    (values (integer 0) boolean boolean))
(defun provider-context--switched-input (provider request &key context sample)
  "Use measured occupancy only if the final request contains the verified history."
  (block nil
    (unless (and (typep provider 'codex-subscription-provider)
                 (conversation-context-usage-sample-p sample)
                 (= 2 (getf sample :format))
                 (not (equal (getf sample :policy) (getf context :policy)))
                 (or (getf sample :wire-history-p)
                     (and (= (getf sample :wire-count) (getf sample :history-count))
                          (equal (getf sample :wire-digest) (getf sample :history-digest)))))
      (return (values 0 nil nil)))
    (let* ((conversation (getf context :conversation))
           (history      (provider-history-items provider conversation :include-ephemeral-p nil))
           (items        (provider-context--request-items provider request)))
      (multiple-value-bind (added matched-p)
          (provider-context--switch-history provider conversation :sample sample :current history)
        (unless (and matched-p items
                     (provider-context--extends-p (mapcar #'provider-cache-prefix--entry history)
                                                  (map 'list #'provider-cache-prefix--entry items))
                     (provider-context--current-wire-p context request sample))
          (return (values 0 nil nil)))
        (values (+ (max (provider-context--header-charge sample context)
                        (provider-context--switched-field-charge provider request sample))
                   (provider-context--input-tokens
                    provider sample
                    (concatenate 'vector added (subseq items (length history)))))
                t t)))))

(-> provider-context--matched-input (model-provider hash-table)
    (values (integer 0) boolean boolean))
(defun provider-context--matched-input (provider request)
  "Revalidate history, encoding and policy before retaining measured occupancy."
  (let* ((context          *provider-context-request*)
         (conversation     (getf context :conversation))
         (sample           (and conversation (conversation-context-usage-sample conversation)))
         (header-changed-p (not (equal (getf sample :soft-header) (getf context :soft-header)))))
    (when (and (eq provider (getf context :provider)) (getf context :eligible-p)
               (eq context (conversation-context-usage-request conversation))
               (= (getf context :generation)
                  (conversation-input-measurement-generation conversation))
               (or (not (typep provider 'codex-subscription-provider))
                   (let ((route (provider-input-measurement--route
                                 provider *provider-input-measurement-credentials*)))
                     (and route (equal route *provider-input-measurement-route*)
                          (equal route (getf sample :route)))))

               (provider-context--sample-matches-p
                sample (getf context :soft-header) (provider-context--history provider conversation)
                :components context))
      (let* ((current    (provider-context--wire-plan provider request))
             (items      (provider-context--request-items provider request))
             (fields     (getf current :fields))
             (old-fields (getf sample :wire-fields)))
        (when (and items
                   (if header-changed-p
                       (provider-context--current-wire-p context request sample)
                       (equal old-fields fields))
                   (provider-context--prefix-matches-p
                    (getf sample :wire-digest) (getf sample :wire-count) (getf current :items)))
          (let ((count (getf sample :wire-count)))
            (multiple-value-bind (tokens uncertain-p)
                (provider-context--input-tokens provider sample (unless (= count (length items)) (subseq items count)) :replaced-characters (getf sample :temporary-characters 0))              (return-from provider-context--matched-input
                                                                                                                                                                                                 (values (+ tokens (if header-changed-p
                                                                                                                                                                                                                       (provider-context--header-charge sample context) 0))
                                                                                                                                                                                                         t (or header-changed-p uncertain-p))))))))
    (if (and (eq provider (getf context :provider)) (getf context :eligible-p))
        (provider-context--switched-input provider request :context context :sample sample)
        (values 0 nil nil))))

(-> provider-context--response-entries-p (list list) boolean)
(defun provider-context--response-entries-p (entries output)
  "Require every added entry to be a distinct ordered response item, excluding concurrent input."
  (loop with remaining = output
        for entry in entries
        always (let ((tail (member entry remaining :test #'equal)))
                 (when tail
                   (setf remaining (rest tail))
                   t))))

(-> provider-context--wire-fields-compatible-p (list list) boolean)
(defun provider-context--wire-fields-compatible-p (encoded observed)
  "Check that ENCODED introduces no field the transport did not observe.

Providers may add transport-only fields after encoding.  The usage sample
must describe the transported request, so those fields come from OBSERVED;
the encoder's own fields must still be present there."
  (and observed
       (loop for field in encoded
             always (assoc (first field) observed :test #'equal))))

(-> provider-context-note-response (list provider-result) null)
(defun provider-context-note-response (context result)
  "Anchor ordinary usage after persistence, before locally generated tool evidence.

Re-encode the measured projection through its original encoder, with its
persisted response and no temporary suffix. Retain header text once per epoch;
messages that later change fail closed."
  (let* ((conversation (getf context :conversation))
         (provider     (getf context :provider))
         (adapter      (getf context :encoder))
         (projection   (getf context :projection))
         (usage        (provider-usage-normalize (provider-result-usage result)))
         (input        (and (json-object-p usage) (json-get usage "input_tokens")))
         (output       (and (json-object-p usage) (json-get usage "output_tokens")))
         (total        (and (json-object-p usage) (json-get usage "total_tokens"))))
    (when conversation
      (with-recursive-lock-held ((conversation-append-lock conversation))
       (when (and (eq context (conversation-context-usage-request conversation))
                  (= (getf context :generation)
                     (conversation-input-measurement-generation conversation))
                  (eq provider (getf context :wire-provider)))
      (when (and (getf context :eligible-p) adapter projection
                 (or (not (typep provider 'codex-subscription-provider))
                     (getf context :route))
                 (getf context :wire)
                 (typep input '(integer 1)) (typep output '(integer 0))
                 (or (plusp output) (null (provider-result-output-items result)))
                 (let ((cached (json-get usage "cached_input_tokens")))
                   (or (null cached) (and (typep cached '(integer 0)) (<= cached input))))
                 (or (null total) (and (typep total '(integer 0)) (>= total (+ input output)))))
        (let* ((history (provider-history-items provider conversation :include-ephemeral-p nil))
               (entries (mapcar #'provider-cache-prefix--entry history))
               (before  (getf context :history)))
          (when (and (provider-context--extends-p before entries)
                     (provider-context--response-entries-p
                      (nthcdr (length before) entries)
                      (mapcar #'provider-cache-prefix--entry (provider-result-output-items result))))
            (let* ((options                          (copy-list (cl-llm-provider-api:wire-request-options projection)))
                   (*provider-context-request*       nil)
                   (*provider-cache-prefix-function* nil)
                   (base
                     (make-instance 'cl-llm-provider-api:wire-request
                                    :model   (cl-llm-provider-api:wire-request-model projection)
                                    :prefix  (cl-llm-provider-api:wire-request-prefix projection)
                                    :items   (cl-llm-provider-api:wire-request-items projection)
                                    :options options))
                   (base-characters
                     (provider-context--request-characters adapter (provider-context--request-items adapter (provider-request-object adapter base (getf context :tools)))))
                   (covered
                      (make-instance 'cl-llm-provider-api:wire-request
                                     :model   (cl-llm-provider-api:wire-request-model projection)
                                     :prefix  (cl-llm-provider-api:wire-request-prefix projection)
                                     :items   (append (cl-llm-provider-api:wire-request-items projection)
                                                      (nthcdr (length before) history))
                                     :options options))
                   (wire                             (provider-context--wire-plan adapter (provider-request-object adapter covered (getf context :tools)))))
              (when (provider-context--wire-fields-compatible-p (getf wire :fields) (getf (getf context :wire) :fields))
                (let ((sample
                        (list :route (getf context :route) :format 2 :soft-header (getf context :soft-header)
                              :policy         (getf context :policy)
                              :prompt         (getf context :prompt)
                              :tool-header    (getf context :tool-header)
                              :history-count  (length entries)
                              :history-digest (provider-cache-prefix--entry (coerce entries 'vector))
                              :wire-fields    (getf (getf context :wire) :fields)
                              :encoder-fields (getf wire :fields)
                              :wire-count     (length (getf wire :items))
                              :wire-digest    (provider-cache-prefix--entry
                                               (coerce (getf wire :items) 'vector))
                              :tokens         (max (+ input output) (or total 0))
                              :temporary-characters
                              (max 0 (- (getf context :wire-characters) base-characters)))))
                  (when (and (typep provider 'codex-subscription-provider)
                             (provider-context--extends-p before (getf (getf context :wire) :items))
                             (provider-context--extends-p entries (getf wire :items)))
                    (setf (getf sample :wire-history-p) t))
                 (let ((previous (conversation-context-usage-sample conversation)))
                   (unless (and (conversation-context-usage-sample-p previous)
                                (getf previous :header-text)
                                (every (lambda (key)
                                         (equal (getf previous key) (getf sample key)))
                                       '(:prompt :tool-header)))
                     (setf (getf sample :header-text) (getf context :header-text)))
                   (conversation-append-record conversation (list :context-usage :sample sample))
                   (setf (conversation-context-usage-sample conversation)
                         (conversation-context-usage-resolve sample previous)))))))))))
))
  nil)

(defmethod cl-llm-provider-api::request-analysis--image-location-p :around
    ((provider codex-subscription-provider) path)
  "Include Codex's structured tool-result image parts in the encoder allowance.

The pinned provider analyzer covers message content, but Codex also accepts
multimodal function outputs. Match only direct output-array parts in history;
image-shaped JSON nested inside a tool payload is ordinary encoded text."
  (or (call-next-method)
      (and (integerp (first path)) (equal (second path) "output")
           (integerp (third path))
           (or (null (cdddr path))
               (equal (cdddr path) '("input"))))))
(-> provider-context--request-characters ((option model-provider) t) (values (integer 0) boolean))
(defun provider-context--request-characters (provider value)
  "Apply Autolith's image allowance to encoder-owned character analysis."
  (provider-request-characters (or provider (make-instance 'model-provider)) value
                               :image-characters (* 4 (or *provider-image-token-estimate* *image-input-maximum-patches*))))

(-> provider-request-output-ceilings (model-provider hash-table) list)
(defun provider-request-output-ceilings (provider request)
  "Combine positive wire limits with the caller's Autolith output allowance."
  (append (when (and (integerp *provider-maximum-output-tokens*)
                     (plusp *provider-maximum-output-tokens*))
            (list *provider-maximum-output-tokens*))
          (provider-request-output-limits provider request)))

(-> provider-request-output-reserve (model-provider hash-table) (integer 1))
(defun provider-request-output-reserve (provider request)
  "Reserve the larger of configured headroom and the request's explicit output cap."
  (reduce #'max (provider-request-output-ceilings provider request)          :initial-value (config :context-output-reserve (provider-configuration provider))))

(defparameter *provider-context-observation* nil
  "Ordinary invocation's owner, attempt, boundary counter and completion mapping.")

(declaim (special *provider-bounded-assessment-p*))

(-> provider-context--observe
    (model-provider &key (:stage keyword) (:estimate integer) (:non-text-p boolean)
     (:mode (option keyword)) (:failure t) (:window integer) (:reserve integer)
     (:decision keyword)) null)
(defun provider-context--observe
    (provider &key stage estimate non-text-p mode failure window reserve decision)
  "Append one immutable boundary snapshot when this ordinary owner is supported."
  (let ((context *provider-context-observation*))
    (when (and context (eq stage :hard)) (setf (getf context :hard) nil))
    (when (and context (getf context :active-p)
               (not *provider-bounded-assessment-p*)
               (eq provider (getf context :provider)))
      (when (member mode '(:receipt-history :receipt-prefix :retained-usage :character/image))
        (let ((candidate (incf (getf context :candidate))))
          (conversation-append-context-decision
           (getf context :owner)
           (list :version 1 :attempt (getf context :attempt) :candidate candidate
                 :stage stage :estimate estimate :mode mode
                 :receipt-failure (and failure
                                       (if (member failure '(:no-owner :no-anchor :unsupported-layout
                                                             :identity-changed :prefix-changed
                                                             :unattested-provenance :history-changed))
                                           failure :unknown))
                 :non-text-p non-text-p :window window :reserve reserve
                 :soft-threshold (getf context :threshold) :decision decision))
          (when (and (eq stage :hard) (eq decision :admit))
            (setf (getf context :hard) candidate))))))
  nil)

(-> provider-check-request-context (model-provider hash-table) null)
(defun provider-check-request-context (provider request)
  "Reject an oversized encoded request before network I/O."
  (multiple-value-bind (input non-text-p mode failure)
      (provider-request-input-token-estimate provider request)
    (let ((reserve (provider-request-output-reserve provider request))
          (window  (provider-effective-context-window provider)))
      (provider-context--observe
       provider :stage ':hard :estimate input :non-text-p non-text-p
       :mode mode :failure failure :window window :reserve reserve
       :decision (if (> (+ input reserve) window) :refuse :admit))
      (when (> (+ input reserve) window)
        (error 'provider-context-budget-error
               :message                (format nil
                                               "The next request needs approximately ~D input tokens plus ~D output tokens; the model context window is ~D. Compact the conversation or reduce request context before retrying."
                                               input reserve window)
               :estimated-input-tokens input
               :output-reserve         reserve
               :context-window         window
               :non-text-estimate-p    non-text-p))))
  nil)

(defmethod provider-open-response-stream :before
    ((provider model-provider) (request hash-table) &key credentials conversation)
  "Admit the fully encoded streaming request after provider-specific modifications."
  (provider-check-assessment-request provider request)
  (provider-context--admit-request provider request)
  (when (and conversation *provider-context-request*
             (eq conversation (getf *provider-context-request* :conversation)))
    (setf (getf *provider-context-request* :route)
          (provider-input-measurement--route provider credentials)
          (getf *provider-context-request* :wire-provider) provider
          (getf *provider-context-request* :wire) (provider-context--wire-plan provider request)          (getf *provider-context-request* :wire-characters)
          (provider-context--request-characters provider (provider-context--request-items provider request))))
  (provider-input-measurement--attest provider request conversation)

  (when *provider-context-admission-state*
    (setf (first *provider-context-admission-state*) :entered
          (rest *provider-context-admission-state*) nil)))

(define-condition provider-context-budget-error (autolith-error)
  ((estimated-input-tokens
    :initarg       :estimated-input-tokens
    :reader        provider-context-budget-error-estimated-input-tokens
    :type          (integer 0)
    :documentation "Estimated input size of the actual encoded request.")
   (output-reserve
    :initarg       :output-reserve
    :reader        provider-context-budget-error-output-reserve
    :type          (integer 1)
    :documentation "Tokens reserved for output.")
   (context-window
    :initarg       :context-window
    :reader        provider-context-budget-error-context-window
    :type          (integer 1)
    :documentation "The configured shared input/output context window.")
   (non-text-estimate-p
    :initarg       :non-text-estimate-p
    :reader        provider-context-budget-error-non-text-estimate-p
    :type          boolean
    :documentation "Whether image or opaque payload estimates affected admission."))
  (:documentation "Local context admission refused a request before transport."))

(-> provider-context--admit-request (model-provider hash-table) null)
(defun provider-context--admit-request (provider request)
  "Check the request budget and retain any context-budget error."
  (handler-bind
      ((provider-context-budget-error
         (lambda (condition)
           (when (and *provider-context-admission-state*
                      (eq (first *provider-context-admission-state*) :pending))
             (setf (rest *provider-context-admission-state*) condition)))))
    (provider-check-request-context provider request)))

(defgeneric provider-final-request-participation-p (provider)
  (:documentation "Return true when PROVIDER participates in final-request soft admission.

Participants must call PROVIDER-CALL-PREPARED-REQUEST-DECISION once for each final
ordinary candidate before transport, using the prepared request that hard admission
will check. They receive no raw-estimate soft preflight. The default preserves the
legacy proactive preflight for direct-return PROVIDER-STREAM-TURN extensions.
Specialize this generic to opt in, or return NIL when overriding a participating
provider with a direct-return implementation."))

(defmethod provider-final-request-participation-p ((provider model-provider))
  "Preserve proactive compaction for legacy direct-return providers."
  (declare (ignore provider))
  nil)

(defmethod provider-final-request-participation-p ((provider session-preserving-provider-mixin))
  "Opt Autolith's production final-preparation adapters into anchored decisions."
  (declare (ignore provider))
  t)

(defparameter *provider-prepared-request-decision* nil
  "An ordinary owner's input-only proactive decision, scoped to one candidate.")

(define-condition provider-proactive-compaction-required (provider-context-budget-error)
  ()
  (:documentation "The ordinary owner requested recovery before final hard admission."))

(-> provider-call-prepared-request-decision (model-provider hash-table) null)
(defun provider-call-prepared-request-decision (provider request)
  "Ask the ordinary owner about this final candidate without granting transport permission."
  (when *provider-prepared-request-decision*
    (multiple-value-bind (input non-text-p mode failure)
        (provider-request-input-token-estimate provider request)
      (let ((compact-p (funcall *provider-prepared-request-decision* input))
            (reserve   (provider-request-output-reserve provider request))
            (window    (provider-effective-context-window provider)))
        (provider-context--observe
         provider :stage ':soft :estimate input :non-text-p non-text-p
         :mode mode :failure failure :window window :reserve reserve
         :decision (if compact-p :compact :continue))
        (when compact-p
          (let ((condition (make-condition
                            'provider-proactive-compaction-required
                            :message "The prepared ordinary input reached its soft compaction threshold."
                            :estimated-input-tokens input :output-reserve reserve
                            :context-window window :non-text-estimate-p t)))
            (when (and *provider-context-admission-state*
                       (eq (first *provider-context-admission-state*) :pending))
              (setf (rest *provider-context-admission-state*) condition))
            (error condition))))))
  nil)

(-> provider-call-with-mission-output-allowance (model-provider hash-table function) t)
(defun provider-call-with-mission-output-allowance (provider request continuation)
  "Limit mission output tokens to the available context before calling CONTINUATION.

Check caller limits before reducing the mission allowance, then recheck the
encoded request."
  (if (null *provider-mission-output-allowance*)
      (funcall continuation request)
      (let ((ceilings (provider-request-output-ceilings provider request)))
        (let ((*provider-maximum-output-tokens*
                (when ceilings (reduce #'max ceilings))))
          (provider-context--admit-request provider request))
        (let* ((window    (provider-effective-context-window provider))
               (ceiling
                 (reduce #'min ceilings
                         :initial-value
                         (min *provider-mission-output-allowance*
                              (- window (provider-request-input-token-estimate provider request)))))
               (candidate (provider-request-with-output-ceiling provider request ceiling))
               (remaining (- window (provider-request-input-token-estimate provider candidate))))
          (if (plusp remaining)
              (let* ((effective                        (min ceiling remaining))
                     (final-request
                       (if (= effective ceiling)
                           candidate
                           (provider-request-with-output-ceiling provider request effective)))
                     (*provider-maximum-output-tokens* effective))
                (provider-context--admit-request provider final-request)
                (funcall continuation final-request))
              ;; No positive output envelope exists for this encoded candidate.
              (let ((*provider-maximum-output-tokens* ceiling))
                (provider-context--admit-request provider candidate)))))))
;;;; -- Measured Ordinary Input --

(defparameter *provider-input-measurement-context* nil
  "An ordinary request's exact owner, issuance and final transport proof, or NIL.")

(-> provider-input-measurement--digest (t) string)
(defun provider-input-measurement--digest (value)
  "Hash portable proof data without retaining its JSON representation."
  (first (provider-cache-prefix--entry value)))

(-> provider-input-measurement--root (list integer) string)
(defun provider-input-measurement--root (entries count)
  "Fold exactly COUNT ordered item fingerprints into one domain-separated root."
  (provider-input-measurement--digest
   (vector "autolith-input-prefix-v1" (coerce (subseq entries 0 count) 'vector))))

(-> provider-input-measurement--route (model-provider t) (option string))
(defun provider-input-measurement--route (provider credentials)
  "Attest the Codex account header inside credential ownership, not bearer identity.

Unknown or malformed routing has no proof. Only a bounded digest may escape the
credential scope; access-token refresh does not change a known account route."
  (when (and (typep provider 'codex-subscription-provider)
             (typep credentials 'oauth-credentials))
    (let ((account (oauth-credentials-account-id credentials)))
      (when (and (stringp account) (<= 1 (length account) 256)
                 (every (lambda (character)
                          (or (and (char<= #\a character #\z))
                              (and (char<= #\A character #\Z))
                              (and (char<= #\0 character #\9))
                              (find character "-_.")))
                        account))
        (provider-input-measurement--digest
         (vector "codex-account-route-v1" account))))))

(-> provider-input-measurement--begin-route (model-provider t) (option string))
(defun provider-input-measurement--begin-route (provider credentials)
  "Start an actual credential-owned attempt without inheriting an earlier proof."
  (when *provider-input-measurement-context*
    (setf (getf *provider-input-measurement-context* :proof) nil))
  (provider-input-measurement--route provider credentials))

(-> provider-input-measurement--end-route (model-provider t (option string)) null)
(defun provider-input-measurement--end-route (provider credentials route)
  "Revoke proof if routing changed before ordinary parsing and cleanup returned."
  (unless (equal route (provider-input-measurement--route provider credentials))
    (when *provider-context-request*
      (setf (getf *provider-context-request* :eligible-p) nil))
    (when *provider-input-measurement-context*
      (setf (getf *provider-input-measurement-context* :proof) nil)))
  nil)

(-> provider-input-measurement--identity (model-provider hash-table) (option string))
(defun provider-input-measurement--identity (provider request)
  "Identify the supported full-input interpretation and effective transport route."
  (when (and (typep provider 'codex-subscription-provider)
             *provider-input-measurement-route*
             (equal *provider-input-measurement-route*
                    (provider-input-measurement--route
                     provider *provider-input-measurement-credentials*))
             (vectorp (json-get request "input"))
             (stringp (json-get request "model"))
             (null (json-get request "request")))
    (provider-input-measurement--digest
     (vector "codex-responses-full-input-v2"
             *provider-input-measurement-route*
             (symbol-name (class-name (class-of provider)))
             (symbol-name (provider-family provider))
             (config :provider-endpoint (provider-configuration provider))
             (config :model (provider-configuration provider))
             (json-get request "model")
             (json-get request "reasoning")))))

(defparameter *provider-input-measurement-encoding* nil
  "Actual item encoder calls for one Responses projection, never a replay encoding.")

(-> provider-input-measurement--project-history
    (model-provider conversation &key (:history list) (:items list)) null)
(defun provider-input-measurement--project-history (provider conversation &key history items)
  "Record origins at the trusted history/context assembly boundary by object identity.

Require the complete raw sequence to survive family projection unchanged. Unsupported
family transformations have no provenance authority. New assembly objects are injected
context, including developer messages; durable developer messages retain history origin."
  (let ((context *provider-input-measurement-context*))
    (when (and context (eq provider (getf context :adapter))
               (eq conversation (getf context :owner)))
      (setf (getf context :provenance) nil)
      (let ((raw     (conversation-input-items conversation))
            (durable (conversation-input-items-for-request conversation :include-ephemeral-p nil)))
        (when (and (= (length raw) (length history)) (every #'eq raw history)
                   (= (length raw) (length (remove-duplicates raw :test #'eq))))
          (let ((remaining history)
                (origins   nil))
            (dolist (item items)
              (cond
                ((and remaining (eq item (first remaining)))
                 (push (position item durable :test #'eq) origins)
                 (pop remaining))
                ((member item history :test #'eq)
                 (return-from provider-input-measurement--project-history nil))
                (t
                 (push nil origins))))
            (unless remaining
              (setf (getf context :provenance)
                    (list :items (copy-list items) :origins (nreverse origins) :durable durable
                          :history-root   (provider-input-measurement--items-root durable (length durable))
                          :generation     (conversation-input-measurement-generation conversation)
                          :log-generation (conversation-log-generation conversation))))))))
   nil))

(defmethod provider-wire-input-item :around ((provider codex-subscription-provider) item)
  "Observe the actual encoder result paired with its source object, without re-encoding."
  (let ((result (call-next-method)))
    (when *provider-input-measurement-encoding*
      (push (list item result (provider-cache-prefix--entry result))
            (rest *provider-input-measurement-encoding*)))
    result))

(-> provider-input-measurement--encode
    (model-provider cl-llm-provider-api:wire-request function) (values hash-table t))
(defun provider-input-measurement--encode (provider projection continuation)
  "Attest the pinned Responses encoder's complete sequence, including prefix and suffix.

Every history/context item must have exactly one actual encoder call in order. Prefix
and suffix spans originate in the supplied instruction lists, with the pinned encoder's
known placement. Any extra, dropped, merged or substituted item fails closed."
  (let* ((context                               *provider-input-measurement-context*)
         (plan                                  (getf context :provenance))
         (*provider-input-measurement-encoding* (and plan (list t))))
    (multiple-value-bind (request volatile) (funcall continuation)
      (when plan
        (let* ((calls     (nreverse (rest *provider-input-measurement-encoding*)))
               (items     (cl-llm-provider-api:wire-request-items projection))
               (placement (cl-llm-provider-api:provider-responses-instructions-placement provider))
               (prefix    (when (eq placement :input)
                            (mapcar #'responses-developer-message
                                    (remove-if-not #'non-empty-string-p
                                                   (cl-llm-provider-api:wire-request-prefix projection)))))
               (suffix    (mapcar #'responses-developer-message
                                  (remove-if-not #'non-empty-string-p
                                                 (cl-llm-provider-api:wire-request-suffix projection))))
               (wire      (json-get request "input"))
               (start     (length prefix)))
          (setf (getf context :provenance) nil)
          (when (and (member placement '(:input :top-level))
                     (vectorp wire) (not (stringp wire))
                     (= (length items) (length (getf plan :items)) (length calls))
                     (every #'eq items (getf plan :items))
                     (every #'eq (getf plan :items) (mapcar #'first calls))
                     (= (length wire) (+ start (length calls) (length suffix)))
                     (equal (mapcar #'provider-cache-prefix--entry prefix)
                            (map 'list #'provider-cache-prefix--entry (subseq wire 0 start)))
                     (equal (mapcar #'provider-cache-prefix--entry suffix)
                            (map 'list #'provider-cache-prefix--entry
                                 (subseq wire (+ start (length calls)))))
                     (loop for call in calls for index from start
                           always (and (eq (second call) (aref wire index))
                                       (equal (third call) (provider-cache-prefix--entry (aref wire index))))))
            (setf (getf plan :wire-items) (copy-seq wire)
                  (getf plan :wire-entries) (map 'list #'provider-cache-prefix--entry wire)
                  (getf plan :wire-origins) (append (make-list start) (getf plan :origins)
                                                  (make-list (length suffix)))
                  (getf context :provenance) plan))))
      (values request volatile))))

(-> provider-input-measurement--final-provenance (hash-table) list)
(defun provider-input-measurement--final-provenance (request)
  "Validate complete origin mapping against the actual final-prepared request and owner."
  (let* ((context  *provider-input-measurement-context*)
         (owner    (getf context :owner))
         (plan     (getf context :provenance))
         (wire     (json-get request "input"))
         (expected (getf plan :wire-items)))
    (when (and owner plan expected (vectorp wire) (not (stringp wire))
               (= (getf context :issued) (conversation-input-measurement-issuance owner))
               (= (getf plan :generation) (conversation-input-measurement-generation owner))
               (= (getf plan :log-generation) (conversation-log-generation owner))
               (= (length wire) (length expected)
                  (length (getf plan :wire-origins)) (length (getf plan :wire-entries)))
               ;; Every claimed durable item must occur once, in order, on this wire.
               ;; NIL origins belong to injected spans and carry no durable authority.
               (equal (remove nil (getf plan :wire-origins))
                      (loop for index below (length (getf plan :durable)) collect index))
               (every #'eq wire expected)
               (equal (getf plan :wire-entries) (map 'list #'provider-cache-prefix--entry wire))
               (equal (getf plan :history-root)
                      (let ((durable (conversation-input-items-for-request owner :include-ephemeral-p nil)))
                        (and (= (length durable) (length (getf plan :durable)))
                             (provider-input-measurement--items-root durable (length durable))))))
      plan)))

(-> provider-input-measurement--durable-root (list integer) string)
(defun provider-input-measurement--durable-root (plan count)
  "Commit complete ordered source semantics together with their actual encoded results."
  (provider-input-measurement--digest
   (vector "codex-responses-durable-v3"
           (coerce
            (loop for item in (getf plan :durable) for index below count
                  collect (list (provider-cache-prefix--entry item)
                                (nth (position index (getf plan :wire-origins))
                                     (getf plan :wire-entries))))
            'vector))))

(-> provider-input-measurement--provenance-estimate
    (model-provider hash-table list) (values (option integer) keyword))
(defun provider-input-measurement--provenance-estimate (provider request receipt)
  "Credit only the complete old durable commitment; charge every other current span."
  (let* ((plan    (provider-input-measurement--final-provenance request))
         (durable (getf plan :durable))
         (count   (getf receipt :history-count)))
    (cond
      ((null plan)
       (values nil :unattested-provenance))
      ((or (>= (getf receipt :issued) (getf *provider-input-measurement-context* :issued))
           (< (length durable) count)
           (not (equal (getf receipt :history-root)
                       (provider-input-measurement--durable-root plan count))))
       (values nil :history-changed))
      (t
       (let ((unaccounted (json-object-copy request)))
         (setf (gethash "input" unaccounted)
               (coerce (loop for item across (json-get request "input")
                             for origin in (getf plan :wire-origins)
                             unless (and (integerp origin) (< origin count)) collect item)
                       'vector))
         (values (+ (getf receipt :input)
                     (ceiling (provider-context--request-characters provider unaccounted) 4))
                 :measured-history))))))

(-> provider-input-measurement--provenance-receipt (model-provider hash-table list) list)
(defun provider-input-measurement--provenance-receipt (provider request legacy)
  "Earn v3 authority only at a qualifying ordinary success's final transport boundary.

Retain the independent strict wire-prefix receipt when provenance is unsupported.
Neither an old receipt nor current history alone can authorize a v3 upgrade."
  (declare (ignore provider))
  (let ((plan (provider-input-measurement--final-provenance request)))
    (if (and plan (plusp (length (getf plan :durable))))
        (append (list :version 3) (rest (rest legacy))
                (list :history-count (length (getf plan :durable))
                      :history-root  (provider-input-measurement--durable-root plan (length (getf plan :durable)))
                      :attestation   (provider-input-measurement--digest
                                      (vector "codex-responses-origins-v3"
                                              (provider-input-measurement--durable-root plan (length (getf plan :durable)))
                                              (coerce (getf plan :wire-origins) 'vector)
                                              (coerce (getf plan :wire-entries) 'vector)))
                      :provenance    :codex-responses-origins-v3))
        legacy)))

(-> provider-input-measurement--estimate (model-provider hash-table) (values (option integer) keyword))
(defun provider-input-measurement--estimate (provider request)
  "Credit only a measured exact ordered prefix, charging all current overhead.

This is a measured/heuristic estimate, not a tokenizer upper bound. The prior
whole input already includes its old overhead; charging current overhead again
intentionally avoids subtracting any unproven old estimate."
  (let* ((context  *provider-input-measurement-context*)
         (owner    (getf context :owner))
         (receipt  (and owner (conversation-input-measurement owner)))
         (identity (provider-input-measurement--identity provider request)))
    (cond
      ((not (and owner (eq provider (getf context :adapter))))
       (values nil :no-owner))
      ((not (conversation-input-measurement-valid-p receipt))
       (values nil :no-anchor))
      ((null identity)
       (values nil :unsupported-layout))
      ((not (equal identity (getf receipt :identity)))
       (values nil :identity-changed))
      ((eql (getf receipt :version) 3)
       (provider-input-measurement--provenance-estimate provider request receipt))
      (t
       (let* ((items   (json-get request "input"))
              (count   (getf receipt :count))
              (entries (getf (provider-cache-wire-plan provider request) :items)))
         (if (or (< (length entries) count)
                 (not (equal (provider-input-measurement--root entries count)
                             (getf receipt :root))))
             (values nil :prefix-changed)
             (let ((unaccounted (json-object-copy request)))
               (setf (gethash "input" unaccounted) (subseq items count))
               (values (+ (getf receipt :input)
                          (ceiling (provider-context--request-characters provider unaccounted) 4))
                       :measured-prefix))))))))

(defmethod provider-request-input-token-estimate :around
    ((provider model-provider) (request hash-table))
  "Use an independently proven ordinary measurement, otherwise the existing estimate."
  (multiple-value-bind (estimate reason)
      (provider-input-measurement--estimate provider request)
    (if estimate
        (values estimate t (ecase reason
                             (:measured-history
                              :receipt-history)
                             (:measured-prefix
                              :receipt-prefix)) nil)
        (multiple-value-bind (tokens non-text-p mode)
            (call-next-method)
          (values tokens non-text-p mode reason)))))

(defmethod provider-request-object :before
    ((provider model-provider) (conversation conversation)
     (tool-namespaces vector) &key goal-context compaction-p)
  "Authorize measurement only for this ordinary attempt's exact conversation object."
  (declare (ignore tool-namespaces goal-context))
  (when *provider-context-observation*
    (setf (getf *provider-context-observation* :hard) nil
          (getf *provider-context-observation* :active-p)
          (and (not compaction-p) (not *provider-bounded-assessment-p*)
               (eq conversation (getf *provider-context-observation* :owner))))
    ;; Every ordinary dispatch, including a different fallback adapter, owns a new attempt.
    (when (getf *provider-context-observation* :active-p)
      (setf (getf *provider-context-observation* :provider) provider
            (getf *provider-context-observation* :attempt) (make-identifier)
            (getf *provider-context-observation* :candidate) 0)))
  (when (and *provider-context-request*
             (or compaction-p
                 (not (eq conversation (getf *provider-context-request* :conversation)))
                 (not (eq provider (getf *provider-context-request* :provider)))))
    (setf (getf *provider-context-request* :eligible-p) nil))
  (when *provider-input-measurement-context*
    (setf (getf *provider-input-measurement-context* :adapter)
          (and (not compaction-p)
               (eq conversation (getf *provider-input-measurement-context* :owner))
               provider))))

(-> provider-input-measurement--attest (model-provider hash-table (option conversation)) null)
(defun provider-input-measurement--attest (provider request conversation)
  "Attest the actual admitted final request, after routing and output headroom."
  (let* ((context  *provider-input-measurement-context*)
         (identity (provider-input-measurement--identity provider request)))
    (when (and context identity
               (eq conversation (getf context :owner))
               (eq provider (getf context :adapter)))
      (let* ((plan  (provider-cache-wire-plan
                     provider request :volatile-items (getf *provider-cache-request-prefix* :volatile-items 0)))
             (count (getf plan :prefix-items)))
        (setf (getf *provider-input-measurement-context* :proof)
              (and (plusp count)
                   (let ((legacy
                           (list :version 1 :input 1 :count count
                                 :root (provider-input-measurement--root (getf plan :items) count)
                                 :identity identity :issued (getf context :issued) :layout :responses)))
                     (provider-input-measurement--provenance-receipt provider request legacy)))))))
  nil)

(-> provider-input-measurement--usage (provider-result) (option integer))
(defun provider-input-measurement--usage (result)
  "Accept complete positive full input, with any cache counter a valid subset."
  (let ((usage (provider-usage-normalize (provider-result-usage result))))
    ;; Custom providers may return portable lists or no usage; neither attests input.
    (when (hash-table-p usage)
      (let ((input  (json-get usage "input_tokens"))
            (cached (json-get usage "cached_input_tokens")))
        (and (typep input '(integer 1))
             (or (null cached) (and (typep cached '(integer 0)) (<= cached input)))
             input)))))

(defmethod provider-open-response-stream :around
    ((provider codex-subscription-provider) (request hash-table) &key credentials conversation)
  "Bind the actual transport credentials before either final admission check."
  (declare (ignore request))
  (let* ((route                                    (provider-input-measurement--route provider credentials))
         (*provider-input-measurement-credentials* credentials)
         (*provider-input-measurement-route*       route)
         (*provider-context-request*
           (and (eq conversation (getf *provider-context-request* :conversation))
                *provider-context-request*))
         (*provider-input-measurement-context*
           (and (eq conversation (getf *provider-input-measurement-context* :owner))
                *provider-input-measurement-context*)))
    (unwind-protect (call-next-method)
      ;; A mutable credential object must not attest a route changed during transport.
      (unless (equal route (provider-input-measurement--route provider credentials))
        (when *provider-context-request*
          (setf (getf *provider-context-request* :eligible-p) nil))
        (when *provider-input-measurement-context*
          (setf (getf *provider-input-measurement-context* :proof) nil))))))

(-> provider-input-measurement--items-root (list integer) string)
(defun provider-input-measurement--items-root (items count)
  "Hash each semantic item separately, discarding payload serialization immediately."
  (provider-input-measurement--root
   (loop for item in items repeat count
         collect (provider-cache-prefix--entry item))
   count))

(-> provider-call-with-input-measurement (conversation function) (values provider-result list))
(defun provider-call-with-input-measurement (conversation continuation)
  "Return an ordinary result and an earned receipt only after successful cleanup.

The context is object-local and never inferred from goal context, shared IDs or
paths. A concurrent replacement or completion cannot publish this stale attempt."
  (let* ((issued                          (with-recursive-lock-held ((conversation-append-lock conversation))
                                            (incf (conversation-input-measurement-issuance conversation))))
         (generation                      (conversation-input-measurement-generation conversation))
         (log-generation                  (conversation-log-generation conversation))
         (items                           (conversation-input-items conversation))
         (count                           (length items))
         (root                            (provider-input-measurement--items-root items count))
         ;; Always capture a fresh request-local boundary, even without an observer.
         (*provider-cache-request-prefix* nil)
         (*provider-cache-prefix-function*
           (or *provider-cache-prefix-function* (constantly nil)))
         (*provider-input-measurement-context*
           (list :owner conversation :issued issued :adapter nil :proof nil :provenance nil))
         (result                          (funcall continuation))
         (input                           (provider-input-measurement--usage result))
         (proof                           (getf *provider-input-measurement-context* :proof)))
    (with-recursive-lock-held ((conversation-append-lock conversation))
      (let ((current (conversation-input-items conversation)))
        (values result
                (when (and input proof
                           (= generation (conversation-input-measurement-generation conversation))
                           (= log-generation (conversation-log-generation conversation))
                           (= issued (conversation-input-measurement-issuance conversation))
                           (<= count (length current))
                           (equal root (provider-input-measurement--items-root current count)))
                  (setf (getf proof :input) input)
                  (list :owner conversation :receipt proof :generation generation
                        :log-generation log-generation :count count :root root)))))))

(-> provider-input-measurement-ticket-receipt (conversation list) list)
(defun provider-input-measurement-ticket-receipt (conversation ticket)
  "Revalidate transient issuance and lineage while holding the owner's append lock."
  (let ((receipt (getf ticket :receipt))
        (items   (conversation-input-items conversation)))
    (when (and (eq conversation (getf ticket :owner))
               (conversation-input-measurement-valid-p receipt)
               (= (getf receipt :issued) (conversation-input-measurement-issuance conversation))
               (eql (getf ticket :generation) (conversation-input-measurement-generation conversation))
               (eql (getf ticket :log-generation) (conversation-log-generation conversation))
               (typep (getf ticket :count) '(integer 0))
               (<= (getf ticket :count) (length items))
               (equal (getf ticket :root)
                      (provider-input-measurement--items-root items (getf ticket :count))))
      receipt)))

(-> provider-input-measurement-close-stream (t) null)
(defun provider-input-measurement-close-stream (stream)
  "Keep cleanup best effort, but revoke ordinary attestation when cleanup fails."
  (handler-case
      (when (open-stream-p stream)
        (close stream :abort t))
    ((or error sb-sys:deadline-timeout) ()
      (when *provider-input-measurement-context*
        (setf (getf *provider-input-measurement-context* :proof) nil))))
  nil)


;;;; -- Bounded Auxiliary Assessment --

(defparameter *provider-bounded-assessment-p* nil
  "Require an enforceable 512-token wire ceiling and 8192 estimated input tokens.")

(defgeneric provider-assessment-output-path (provider)
  (:documentation "Return a supported final wire ceiling path, or NIL for an unknown transport.

Participants must use the product final-admission boundary on every dispatch,
including retries. A direct-return extension must decline participation."))

(defmethod provider-assessment-output-path ((provider model-provider))
  "Refuse routes without an admitted protocol and final transport boundary."
  (declare (ignore provider))
  nil)

(defmethod provider-assessment-output-path ((provider openai-api-provider))
  "Use the public Responses output ceiling."
  (declare (ignore provider))
  '("max_output_tokens"))

(defmethod provider-assessment-output-path ((provider openai-compatible-provider))
  "Use the actual Chat adapter's declared remote field."
  (list (openai-compatible-provider-output-ceiling-field provider)))

(defmethod provider-assessment-output-path ((provider anthropic-product-provider-mixin))
  "Use the Messages ceiling, including the Vertex Messages adapter."
  (declare (ignore provider))
  '("max_tokens"))

(defmethod provider-assessment-output-path ((provider gemini-product-provider-mixin))
  "Use the GenerateContent ceiling, including the Vertex adapter."
  (declare (ignore provider))
  '("generationConfig" "maxOutputTokens"))

(defmethod provider-assessment-output-path ((provider gemini-code-assist-provider))
  "Validate the real inner GenerateContent ceiling in the routing envelope."
  (cons "request" (call-next-method)))

(-> provider-bounded-assessment-supported-p (model-provider) boolean)
(defun provider-bounded-assessment-supported-p (provider)
  "Require both remote ceiling capability and a participating final wire adapter."
  (not (null (and (provider-output-ceiling-p provider)
                  (provider-final-request-participation-p provider)
                  (provider-assessment-output-path provider)))))

(define-condition provider-assessment-bound-error (autolith-error)
  ()
  (:documentation "An auxiliary assessment cannot establish its finite request bounds."))

(-> provider-check-assessment-route (model-provider) null)
(defun provider-check-assessment-route (provider)
  "Refuse an unsupported actual route before its authentication or request lookup."
  (when (and *provider-bounded-assessment-p*
             (not (provider-bounded-assessment-supported-p provider)))
    (error 'provider-assessment-bound-error
           :message "This route cannot enforce bounded goal assessment."))
  nil)

(defmethod provider-attempt-turn :before
    ((provider model-provider) (conversation conversation)
     &key tool-namespaces event-callback force-refresh goal-context compaction-p)
  "Recheck each selected fallback or retry route before any auxiliary attempt."
  (declare (ignore conversation tool-namespaces event-callback force-refresh goal-context compaction-p))
  (provider-check-assessment-route provider))

(-> provider-check-assessment-request (model-provider hash-table) null)
(defun provider-check-assessment-request (provider request)
  "Check only actual encoded remote fields, never the dynamically requested ceiling."
  (when *provider-bounded-assessment-p*
    (provider-check-assessment-route provider)
    (let ((ceiling (reduce (lambda (object key)
                             (and (json-object-p object) (json-get object key)))
                           (provider-assessment-output-path provider) :initial-value request)))
      (unless (eql ceiling 512)
        (error 'provider-assessment-bound-error
               :message "The final goal assessment request lacks its 512-token remote ceiling.")))
    (when (> (provider-request-input-token-estimate provider request) 8192)
      (error 'provider-assessment-bound-error
             :message "The final goal assessment request exceeds 8192 estimated input tokens.")))
  nil)
