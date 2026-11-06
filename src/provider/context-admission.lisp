(in-package #:autolith)

;;;; -- Request Context Admission --


(-> provider-effective-context-window (model-provider) (integer 1))
(defun provider-effective-context-window (provider)
  "Return the configured window or this request's unspent recovery allowance."
  (let ((configured (config :context-window (provider-configuration provider)))
        (permit     *provider-compaction-recovery-permit*))
    (if (and permit (eq provider (first permit))
             (member (third permit) '(:ready :portable-ready)))
        (max configured (second permit))
        configured)))

(-> provider-consume-compaction-recovery-permit (model-provider &key (:native-p boolean)) null)
(defun provider-consume-compaction-recovery-permit (provider &key native-p)
  "Consume an admitted allowance, retaining native provenance for an unsupported handoff."
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
  "Request-local proof shared with mission accounting during supervised execution.

When bound, FIRST is :PENDING until a provider boundary admits a request, then
:ENTERED permanently. REST holds the exact condition from a local admission
refusal. Neither function entry nor a condition's class proves zero usage.")

(defparameter *provider-image-token-estimate* 16384
  "Portable admission allowance per image when a provider supplies no token count.

This deliberately avoids counting base64 bytes or URL length as image tokens;
provider-specific token accounting may refine the estimate.")

(defgeneric provider-request-input-token-estimate (provider request)
  (:documentation
   "Return REQUEST's estimated input tokens and whether non-text estimates were needed.

REQUEST is the final encoded request, including selected tools and temporary
context. This local admission estimate is independent of paid usage. Methods
may use provider token counts when available; the default is not a tokenizer."))

(defmethod provider-request-input-token-estimate ((provider model-provider) (request hash-table))
  "Estimate the actual encoded prompt at four characters per text token."
  (declare (ignore provider))
  (multiple-value-bind (characters non-text-p)
      (provider-context--request-characters request)
    (values (ceiling characters 4) non-text-p)))

(-> provider-context--request-characters (t) (values (integer 0) boolean))
(defun provider-context--request-characters (value)
  "Count encoded VALUE with bounded image allowances and flag opaque/media estimates."
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
  "Return positive caller and wire ceilings, optionally inside routing envelopes."
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
     ;; Code Assist carries the GenerateContent policy inside its routing envelope.
     (when (and enclosed-p (json-object-p inner))
       (provider-request-output-ceilings inner :enclosed-p t)))))

(-> provider-request-output-reserve (model-provider hash-table) (integer 1))
(defun provider-request-output-reserve (provider request)
  "Reserve the larger of configured headroom and the request's explicit output cap."
  (reduce #'max (provider-request-output-ceilings request)
          :initial-value (config :context-output-reserve (provider-configuration provider))))

(-> provider-check-request-context (model-provider hash-table) null)
(defun provider-check-request-context (provider request)
  "Reject an oversized final projection before network I/O, without consuming context."
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
  (declare (ignore credentials))
  (provider-context--admit-request provider request)
  (provider-consume-compaction-recovery-permit provider)
  (when *provider-context-admission-state*
    (setf (first *provider-context-admission-state*) :entered
          (rest *provider-context-admission-state*) nil))
  (provider-input-measurement--attest provider request conversation))

(defmethod provider-open-native-compaction :before
    ((provider model-provider) (request hash-table) &key credentials conversation)
  "Apply context admission to native compaction requests as well as ordinary turns."
  (declare (ignore credentials conversation))
  ;; The unsupported handoff belongs to the summary, never another native attempt.
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
    :documentation "Output allowance reserved beside the input.")
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
  "Check a real provider boundary and retain its exact local refusal condition."
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
  (when (and *provider-prepared-request-decision*
             (funcall *provider-prepared-request-decision*
                      (provider-request-input-token-estimate provider request)))
    (let ((condition (make-condition
                      'provider-proactive-compaction-required
                      :message                "The prepared ordinary input reached its soft compaction threshold."
                      :estimated-input-tokens (provider-request-input-token-estimate provider request)
                      :output-reserve         (provider-request-output-reserve provider request)
                      :context-window         (provider-effective-context-window provider)
                      :non-text-estimate-p    t)))
      (when (and *provider-context-admission-state*
                 (eq (first *provider-context-admission-state*) :pending))
        (setf (rest *provider-context-admission-state*) condition))
      (error condition)))
  nil)

(-> provider-call-with-mission-output-allowance (model-provider hash-table function) t)
(defun provider-call-with-mission-output-allowance (provider request continuation)
  "Bound internal mission headroom at the final encoded request boundary.

Validate the original caller policy first. Only the mission-derived allowance
may shrink to the context envelope. Re-estimate after adding the wire field,
then recheck the actual final request. A nonmonotonic custom estimator may
refuse the reduced candidate, but cannot bypass ordinary admission."
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
    (when *provider-input-measurement-context*
      (setf (getf *provider-input-measurement-context* :proof) nil)))
  nil)

(-> provider-input-measurement--identity (model-provider hash-table) (option string))
(defun provider-input-measurement--identity (provider request)
  "Identify the supported full-input interpretation and effective transport route."
  (when (and (typep provider 'codex-subscription-provider)
             *provider-input-measurement-route*
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
  (declare (ignore provider))
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
                    (ceiling (provider-context--request-characters unaccounted) 4))
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
  (let* ((route                              (provider-input-measurement--route provider credentials))
         (*provider-input-measurement-route* route)
         (*provider-input-measurement-context*
           (and (eq conversation (getf *provider-input-measurement-context* :owner))
                *provider-input-measurement-context*)))
    (unwind-protect (call-next-method)
      ;; A mutable credential object must not attest a route changed during transport.
      (unless (equal route (provider-input-measurement--route provider credentials))
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
