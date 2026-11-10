(in-package #:autolith)

;;;; -- Request Context Admission --


(-> provider-effective-context-window (model-provider) (integer 1))
(defun provider-effective-context-window (provider)
  "Return the larger of the configured window and any eligible recovery window."
  (let ((configured (config :context-window (provider-configuration provider)))
        (permit     *provider-compaction-recovery-permit*))
    (if (and permit (eq provider (first permit))
             (member (third permit) '(:ready :portable-ready)))
        (max configured (second permit))
        configured)))

(-> provider-consume-compaction-recovery-permit (model-provider &key (:native-p boolean)) null)
(defun provider-consume-compaction-recovery-permit (provider &key native-p)
  "Consume the allowance, allowing portable fallback if native compaction is unsupported."
  (when (and *provider-compaction-recovery-permit*
             (eq provider (first *provider-compaction-recovery-permit*))
             (member (third *provider-compaction-recovery-permit*)
                     (if native-p '(:ready) '(:ready :portable-ready))))
    (setf (third *provider-compaction-recovery-permit*) (if native-p :native-spent :spent)))
  nil)

(-> provider-handoff-unsupported-native-permit (model-provider) null)
(defun provider-handoff-unsupported-native-permit (provider)
  "Transfer only a just-consumed native allowance to one portable summary."
  (let ((permit *provider-compaction-recovery-permit*))
    (when (and permit (eq provider (first permit)) (eq :native-spent (third permit)))
      (setf (third permit) :portable-ready)))
  nil)

(defparameter *provider-context-admission-state* nil
  "Request-local admission state shared with mission accounting.

FIRST is :PENDING until a provider admits the request, then :ENTERED.
REST holds the condition from a local refusal. Function entry and condition
class do not establish zero usage.")

(defparameter *provider-image-token-estimate* 16384
  "Token allowance per image when provider token counts are unavailable.")

(defgeneric provider-request-input-token-estimate (provider request)
  (:documentation
   "Return REQUEST's estimated input tokens and whether non-text estimates were needed.

REQUEST is the final encoded request, including selected tools and temporary
context. This local admission estimate is independent of paid usage. Methods
may use provider token counts when available; the default is not a tokenizer."))

(defmethod provider-request-input-token-estimate ((provider model-provider) (request hash-table))
  "Use matched ordinary usage, otherwise estimate the final encoded prompt."
  (multiple-value-bind (tokens matched-p uncertain-p)
      (provider-context--matched-input provider request)
    (if matched-p
        (values tokens uncertain-p)
        (multiple-value-bind (characters non-text-p)
            (provider-context--request-characters request)
          (values (ceiling characters 4) non-text-p)))))

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

(-> provider-context--history (model-provider conversation) list)
(defun provider-context--history (provider conversation)
  "Fingerprint compatible retained history without storing prompt bodies."
  (mapcar #'provider-cache-prefix--entry
          (conversation-input-items-for-family conversation (provider-family provider)
                                               :include-ephemeral-p nil)))

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
            (list :provider provider :conversation conversation
                  :generation (conversation-input-measurement-generation conversation)
             :history         (and header (provider-context--history provider conversation))
             :soft-header     header
             :eligible-p      (and header (null (conversation-ephemeral-input-entries conversation)) t)
             :encoder nil :projection nil :tools nil :wire-provider nil :wire nil :route nil)))
      (with-recursive-lock-held ((conversation-append-lock conversation))
        (setf (conversation-context-usage-request conversation) context))
      context)))

(-> provider-context--wire-body (hash-table) hash-table)
(defun provider-context--wire-body (request)
  "Return the token-bearing request inside an optional routing envelope."
  (let ((inner (json-get request "request")))
    (if (json-object-p inner) inner request)))

(-> provider-context--wire-plan (hash-table) list)
(defun provider-context--wire-plan (request)
  "Reuse wire fingerprints, ignoring cache annotations and output-only controls."
  (labels ((copy-value (value)
             (typecase value
               (hash-table
                (let ((copy (json-object)))
                  (maphash (lambda (key item)
                             (unless (equal key "cache_control")
                               (setf (gethash key copy) (copy-value item))))
                           value)
                  copy))
               (string
                value)
               (vector
                (map 'vector #'copy-value value))
               (t
                value))))
    (let ((body (copy-value (provider-context--wire-body request))))
      (dolist (field '("stream" "stream_options" "store" "max_output_tokens"
                       "max_completion_tokens" "max_tokens" "prompt_cache_key"
                       "prompt_cache_retention"))
        (remhash field body))
      (provider-cache-wire-plan body))))

(-> provider-context--prefix-matches-p (list (integer 0) list) boolean)
(defun provider-context--prefix-matches-p (fingerprint count entries)
  "Verify COUNT retained ENTRIES against a portable prefix FINGERPRINT."
  (and (<= count (length entries))
       (equal fingerprint
              (provider-cache-prefix--entry (coerce (subseq entries 0 count) 'vector)))
       t))

(-> provider-context--sample-matches-p (list (option list) list) boolean)
(defun provider-context--sample-matches-p (sample header history)
  "Verify a portable measured SAMPLE against current policy and retained HISTORY."
  (and (conversation-context-usage-sample-p sample)
       header
       (equal (getf sample :soft-header) header)
       (provider-context--prefix-matches-p
        (getf sample :history-digest) (getf sample :history-count) history)
       t))

(-> provider-context--matched-input (model-provider hash-table)
    (values (integer 0) boolean boolean))
(defun provider-context--matched-input (provider request)
  "Return measured input plus new encoded items after revalidating the covered prefix."
  (let* ((context      *provider-context-request*)
         (conversation (getf context :conversation))
         (sample       (and conversation (conversation-context-usage-sample conversation))))
    (when (and (getf context :eligible-p)
               (eq provider (getf context :provider))
               (eq context (conversation-context-usage-request conversation))
               (= (getf context :generation)
                  (conversation-input-measurement-generation conversation))
               (or (not (typep provider 'codex-subscription-provider))
                   (let ((route (provider-input-measurement--route
                                 provider *provider-input-measurement-credentials*)))
                     (and route (equal route *provider-input-measurement-route*)
                          (equal route (getf sample :route)))))
               (provider-context--sample-matches-p
                sample (getf context :soft-header) (getf context :history)))
      (let ((current (provider-context--wire-plan request)))
        (when (and (equal (getf sample :wire-fields) (getf current :fields))
                   (provider-context--prefix-matches-p
                    (getf sample :wire-digest) (getf sample :wire-count)
                    (getf current :items)))
          (let* ((body  (provider-context--wire-body request))
                 (field (find-if (lambda (name)
                                   (provider-cache-wire--array-p (json-get body name)))
                                 *provider-cache-wire-sequence-fields*))
                 (items (and field (json-get body field)))
                 (count (getf sample :wire-count)))
            (when items
              (multiple-value-bind (characters uncertain-p)
                  (if (= count (length items))
                      (values 0 nil)
                      (provider-context--request-characters (subseq items count)))
                (return-from provider-context--matched-input
                  (values (+ (getf sample :tokens) (ceiling characters 4)) t uncertain-p))))))))
    (values 0 nil nil)))

(-> provider-context--response-entries-p (list list) boolean)
(defun provider-context--response-entries-p (entries output)
  "Require every added entry to be a distinct ordered response item, excluding concurrent input."
  (loop with remaining = output
        for entry in entries
        always (let ((tail (member entry remaining :test #'equal)))
                 (when tail
                   (setf remaining (rest tail))
                   t))))

(-> provider-context-note-response (list provider-result) null)
(defun provider-context-note-response (context result)
  "Anchor ordinary usage after persistence, before locally generated tool evidence.

Re-encode the measured projection through its original encoder, with its
persisted response and no temporary suffix. Retain fingerprints only. Merged
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
        (when (and
               (eq context (conversation-context-usage-request conversation))
               (= (getf context :generation)
                  (conversation-input-measurement-generation conversation))
               (eq provider (getf context :wire-provider)))
         (setf (conversation-context-usage-sample conversation) nil)
         (when (and (getf context :eligible-p) adapter projection
                    (or (not (typep provider 'codex-subscription-provider))
                        (getf context :route))
                    (getf context :wire)
                    (typep input '(integer 1)) (typep output '(integer 0))
                    (or (plusp output) (null (provider-result-output-items result)))
                    (let ((cached (json-get usage "cached_input_tokens")))
                      (or (null cached) (and (typep cached '(integer 0)) (<= cached input))))
                    (or (null total) (and (typep total '(integer 0)) (>= total (+ input output)))))
           (let* ((history (conversation-input-items-for-family
                            conversation (provider-family provider) :include-ephemeral-p nil))
                  (entries (mapcar #'provider-cache-prefix--entry history))
                  (before  (getf context :history)))
             (when (and (provider-context--extends-p before entries)
                        (provider-context--response-entries-p
                         (nthcdr (length before) entries)
                         (mapcar #'provider-cache-prefix--entry (provider-result-output-items result))))
               (let* ((options                          (copy-list (cl-llm-provider-api:wire-request-options projection)))
                      (*provider-context-request*       nil)
                      (*provider-cache-prefix-function* nil)
                      (covered
                        (make-instance 'cl-llm-provider-api:wire-request
                                       :model   (cl-llm-provider-api:wire-request-model projection)
                                       :prefix  (cl-llm-provider-api:wire-request-prefix projection)
                                       :items   (append (cl-llm-provider-api:wire-request-items projection)
                                                        (nthcdr (length before) history))
                                       :options options))
                      (wire                             (provider-context--wire-plan
                                                         (provider-request-object adapter covered (getf context :tools)))))
                 (when (equal (getf wire :fields) (getf (getf context :wire) :fields))
                   (let ((sample
                           (append (when (getf context :route)
                                     (list :route (getf context :route)))
                                   (list :format 1 :soft-header (getf context :soft-header)
                                    :history-count  (length entries)
                                    :history-digest (provider-cache-prefix--entry (coerce entries 'vector))
                                    :wire-fields    (getf wire :fields)
                                    :wire-count     (length (getf wire :items))
                                    :wire-digest    (provider-cache-prefix--entry
                                                     (coerce (getf wire :items) 'vector))
                                    :tokens         (max (+ input output) (or total 0))))))
                     (conversation-append-record conversation (list :context-usage :sample sample))
                     (setf (conversation-context-usage-sample conversation) sample)))))))))))
  nil)

(-> provider-context--request-characters (t) (values (integer 0) boolean))
(defun provider-context--request-characters (value)
  "Count encoded characters, using image allowances, and flag non-text estimates."
  (typecase value
    (hash-table
     (if (provider-context--image-p value)
         (values (* 4 *provider-image-token-estimate*) t)
         (let ((characters 2)
               (non-text-p nil))
           (maphash
            (lambda (key item)
              (multiple-value-bind (count uncertain-p)
                  (provider-context--request-characters item)
                (incf characters (+ 4 (length (princ-to-string key)) count))
                (when (or uncertain-p (equal key "encrypted_content"))
                  (setf non-text-p t))))
            value)
           (values characters non-text-p))))
    (string
     (values (+ 2 (length value)) nil))
    (vector
     (let ((characters 2)
           (non-text-p nil))
       (loop for item across value
             do (multiple-value-bind (count uncertain-p)
                    (provider-context--request-characters item)
                  (incf characters (1+ count))
                  (when uncertain-p
                    (setf non-text-p t))))
       (values characters non-text-p)))
    (t
     (values (length (princ-to-string value)) nil))))

(-> provider-context--image-p (hash-table) boolean)
(defun provider-context--image-p (value)
  "Recognize image blocks in Responses, Chat, Messages and GenerateContent requests."
  (let ((type (json-get value "type"))
        (mime (or (json-get value "mimeType") (json-get value "mime_type"))))
    (not (null (or (member type '("image" "input_image" "image_url") :test #'equal)
                   (and (stringp mime) (uiop:string-prefix-p "image/" mime)))))))

(-> provider-request-output-ceilings (hash-table &key (:enclosed-p boolean)) list)
(defun provider-request-output-ceilings (request &key enclosed-p)
  "Return positive caller and request output limits, including envelopes if requested."
  (let ((generation (json-get request "generationConfig"))
        (inner      (json-get request "request")))
    (append
     (remove-if-not
      (lambda (value) (and (integerp value) (plusp value)))
      (list *provider-maximum-output-tokens*
            (json-get request "max_output_tokens")
            (json-get request "max_completion_tokens")
            (json-get request "max_tokens")
            (and (json-object-p generation)
                 (json-get generation "maxOutputTokens"))))
     ;; GenerateContent policy is nested in the Code Assist routing envelope.
     (when (and enclosed-p (json-object-p inner))
       (provider-request-output-ceilings inner :enclosed-p t)))))

(-> provider-request-output-reserve (model-provider hash-table) (integer 1))
(defun provider-request-output-reserve (provider request)
  "Reserve the larger of configured headroom and the request's explicit output cap."
  (reduce #'max (provider-request-output-ceilings request)
          :initial-value (config :context-output-reserve (provider-configuration provider))))

(-> provider-check-request-context (model-provider hash-table) null)
(defun provider-check-request-context (provider request)
  "Reject an oversized encoded request before network I/O."
  (multiple-value-bind (input non-text-p)
      (provider-request-input-token-estimate provider request)
    (let ((reserve (provider-request-output-reserve provider request))
          (window  (provider-effective-context-window provider)))
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
  (provider-context--admit-request provider request)
  (when (and conversation *provider-context-request*
             (eq conversation (getf *provider-context-request* :conversation)))
    (setf (getf *provider-context-request* :route)
          (provider-input-measurement--route provider credentials)
          (getf *provider-context-request* :wire-provider) provider
          (getf *provider-context-request* :wire) (provider-context--wire-plan request)))
  (provider-consume-compaction-recovery-permit provider)
  (when *provider-context-admission-state*
    (setf (first *provider-context-admission-state*) :entered
          (rest *provider-context-admission-state*) nil))
  (provider-input-measurement--attest provider request conversation))

(defmethod provider-open-native-compaction :before
    ((provider model-provider) (request hash-table) &key credentials conversation)
  "Check native compaction requests against the context budget."
  (declare (ignore credentials conversation))
  ;; Portable fallback allowance cannot be reused for native compaction.
  (let ((*provider-compaction-recovery-permit*
          (unless (eq :portable-ready (third *provider-compaction-recovery-permit*))
            *provider-compaction-recovery-permit*)))
    (provider-context--admit-request provider request)
    (provider-consume-compaction-recovery-permit provider :native-p t))
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

(-> provider-call-with-mission-output-allowance (model-provider hash-table function) t)
(defun provider-call-with-mission-output-allowance (provider request continuation)
  "Limit mission output tokens to the available context before calling CONTINUATION.

Check caller limits before reducing the mission allowance, then recheck the
encoded request."
  (if (null *provider-mission-output-allowance*)
      (funcall continuation request)
      (let ((ceilings (provider-request-output-ceilings request :enclosed-p t)))
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

(defmethod provider-open-native-compaction :around
    ((provider model-provider) (request hash-table) &key credentials conversation)
  "Finalize mission output headroom before native compaction admission."
  (provider-call-with-mission-output-allowance
   provider request
   (lambda (final-request)
     (call-next-method provider final-request :credentials credentials
                                            :conversation  conversation))))


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
      (t
       (let* ((items   (json-get request "input"))
              (count   (getf receipt :count))
              (entries (getf (provider-cache-wire-plan request) :items)))
         (if (or (< (length entries) count)
                 (not (equal (provider-input-measurement--root entries count)
                             (getf receipt :root))))
             (values nil :prefix-changed)
             (let ((unaccounted (json-object-copy request)))
               (setf (gethash "input" unaccounted) (subseq items count))
               (values (+ (getf receipt :input)
                          (ceiling (provider-context--request-characters unaccounted) 4))
                       :measured-prefix))))))))

(defmethod provider-request-input-token-estimate :around
    ((provider model-provider) (request hash-table))
  "Use an independently proven ordinary measurement, otherwise the existing estimate."
  (multiple-value-bind (estimate reason)
      (provider-input-measurement--estimate provider request)
    (declare (ignore reason))
    (if estimate
        (values estimate t)
        (call-next-method))))

(defmethod provider-request-object :before
    ((provider model-provider) (conversation conversation)
     (tool-namespaces vector) &key goal-context compaction-p)
  "Authorize measurement only for this ordinary attempt's exact conversation object."
  (declare (ignore tool-namespaces goal-context))
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
                     request :volatile-items (getf *provider-cache-request-prefix* :volatile-items 0)))
             (count (getf plan :prefix-items)))
        (setf (getf *provider-input-measurement-context* :proof)
              (and (plusp count)
                   (list :version 1 :input 1 :count count
                         :root (provider-input-measurement--root (getf plan :items) count)
                         :identity identity :issued (getf context :issued) :layout :responses))))))
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

(defmethod provider-open-native-compaction :around
    ((provider codex-subscription-provider) (request hash-table) &key credentials conversation)
  "Native compaction neither consumes nor attests an ordinary measured prefix."
  (declare (ignore credentials request conversation))
  (let ((*provider-input-measurement-context* nil))
    (call-next-method)))

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
           (list :owner conversation :issued issued :adapter nil :proof nil))
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
