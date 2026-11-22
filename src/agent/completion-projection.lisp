(in-package #:autolith)

;;;; -- Owned Completion Snapshots --

(defclass tool-completion-result (tool-result)
  ((details
    :initarg       :details
    :reader        tool-result-details
    :documentation "Owned accepted metadata, without producer or provenance roots."))
  (:documentation
   "An immutable text-only completion snapshot. Consumers must not mutate its metadata."))

(defparameter *tool-completion-method-closures*
  (loop for class-name in '(tool-result shell-command-tool-result task-tool-result mission-tool-result
                            skill-load-result executable-skill-result mcp-content-result
                            contract-recording-result)
        for readers = (case class-name
                        (skill-load-result
                         '(skill-load-result-name skill-load-result-newly-selected-p))
                        (executable-skill-result
                         '(executable-skill-result-record))
                        (mcp-content-result
                         '(mcp-content-result-sections))
                        (contract-recording-result
                         '(contract-recording-result-original
                           contract-recording-result-prefix-length
                           contract-recording-result-diagnostic)))
        collect
        (cons class-name
              (loop for name in (append
                                 '(tool-result-content tool-result-content-blocks
                                   tool-result-image-attachments tool-result-success-p
                                   tool-result-category tool-result-denied-p tool-result-error-code
                                   tool-result-details tool-contract--prefix-details)
                                 readers)
                    for generic = (fdefinition name)
                    for owner = (if (member name readers) class-name 'tool-result)
                    for specific = (cond
                                     ((and (eq name 'tool-result-details)
                                           (not (eq class-name 'tool-result)))
                                      class-name)
                                     ((and (eq name 'tool-contract--prefix-details)
                                           (eq class-name 'mcp-content-result))
                                      class-name))
                    for extra = (when (eq name 'tool-contract--prefix-details)
                                  (list (find-class t)))
                    collect
                    (list name generic
                          (append
                           (when specific
                             (list (find-method generic nil
                                                (cons (find-class specific) extra))))
                           (list (find-method generic nil
                                              (cons (find-class owner) extra))))))))
  "Reviewed builtin primary method identities. Extra applicable methods decline.
This assumes stable runtime definitions, not hostile concurrent CLOS mutation.")

(-> tool-completion--methods-p (tool-result) boolean)
(defun tool-completion--methods-p (result)
  "Check exact class and every reviewed consumer method without invoking it."
  (let ((entry (assoc (class-name (class-of result)) *tool-completion-method-closures*)))
    (and entry
         (eq (class-of result) (find-class (first entry)))
         (every
          (lambda (closure)
            (destructuring-bind (name generic methods) closure
              (and (eq (fdefinition name) generic)
                   (equal methods
                          (compute-applicable-methods
                           generic
                           (if (eq name 'tool-contract--prefix-details)
                               (list result 0)
                               (list result)))))))
          (rest entry))
         t)))

(-> tool-completion--copy-string (string) simple-string)
(defun tool-completion--copy-string (text)
  "Copy only TEXT's logical characters into independently owned storage."
  (let ((copy (make-string (length text))))
    (replace copy text)
    copy))

(-> tool-completion--copy-tree (t) t)
(defun tool-completion--copy-tree (tree)
  "Copy validated bounded metadata structure and strings, never backing arrays."
  (cond
    ((stringp tree)
     (tool-completion--copy-string tree))
    ((consp tree)
     (cons (tool-completion--copy-tree (first tree))
           (tool-completion--copy-tree (rest tree))))
    (t
     tree)))

(-> tool-completion--accepted-details-p (t) boolean)
(defun tool-completion--accepted-details-p (details)
  "Test the existing portable metadata budget without omission or overflow effects."
  (validate-tree
   details
   (make-source-grammar
    :label                     "Tool completion metadata"
    :maximum-depth 64 :maximum-nodes 8192
    :maximum-string-characters *conversation-tool-details-maximum-octets*
    :allowed-atom-predicate
    (lambda (value)
      (or (null value) (eq value t) (keywordp value) (stringp value) (numberp value)))))
  (with-standard-io-syntax
    (let ((*print-circle* nil) (*print-readably* t))
      (write details :stream
             (make-instance 'conversation-tool-details-stream
                            :limit *conversation-tool-details-maximum-octets*))))
  t)

(defgeneric tool-completion--details (result ancestors)
  (:documentation "Construct pure builtin metadata; return metadata and eligibility.")
  (:method (result ancestors)
    (declare (ignore result ancestors))
    (values nil nil)))

(defmethod tool-completion--details ((result tool-result) ancestors)
  "Recognize only empty exact-base metadata."
  (declare (ignore ancestors))
  (values nil (eq (class-of result) (find-class 'tool-result))))

(defmethod tool-completion--details ((result shell-command-tool-result) ancestors)
  "Read the shell producer's completed portable record."
  (declare (ignore ancestors))
  (values (slot-value result 'details) t))

(defmethod tool-completion--details ((result task-tool-result) ancestors)
  "Read the task producer's detached portable snapshot, not its live job."
  (declare (ignore ancestors))
  (values (slot-value result 'details) t))

(defmethod tool-completion--details ((result mission-tool-result) ancestors)
  "Read the mission producer's portable goal snapshot."
  (declare (ignore ancestors))
  (values (slot-value result 'details) t))

(defmethod tool-completion--details ((result skill-load-result) ancestors)
  "Construct skill selection metadata without invoking accessors."
  (declare (ignore ancestors))
  (values (list :kind ':skill-load :name (slot-value result 'name)
                :newly-selected-p (slot-value result 'newly-selected-p)) t))

(defmethod tool-completion--details ((result executable-skill-result) ancestors)
  "Read the private validated executable result record."
  (declare (ignore ancestors))
  (values (list :kind ':executable-skill :record (slot-value result 'record)) t))

(defmethod tool-completion--details ((result mcp-content-result) ancestors)
  "Construct section metadata without invoking accessors."
  (declare (ignore ancestors))
  (values (list :mcp-content :sections (slot-value result 'sections)) t))

(-> tool-completion--capture (tool-result list) (option tool-completion-result))
(defun tool-completion--capture (result ancestors)
  "Capture eligible producer-owned text, declining malformed and unsupported values."
  (when (and (< (length ancestors) 32)
             (not (member result ancestors :test #'eq))
             (tool-completion--methods-p result))
    (let ((content   (slot-value result 'content))
          (blocks    (slot-value result 'content-blocks))
          (success-p (slot-value result 'success-p))
          (category  (slot-value result 'category))
          (denied-p  (slot-value result 'denied-p))
          (code      (slot-value result 'error-code)))
      (when (and (stringp content) (<= (length content) 8000)
                 (null (slot-value result 'image-attachments))
                 (member success-p '(t nil)) (member denied-p '(t nil))
                 (member category '(:success :failure :neutral :mechanics))
                 (or (null code) (keywordp code))
                 (loop for tail = blocks then (rest tail)
                       repeat 257
                       when (null tail) return t
                       unless (consp tail) return nil
                       finally (return nil))
                 (every #'stringp blocks)
                 (<= (loop for text in blocks sum (length text)) 8000))
        (multiple-value-bind (details eligible-p)
            (tool-completion--details result (cons result ancestors))
          (when (and eligible-p (tool-completion--accepted-details-p details))
            (make-instance
             'tool-completion-result
             :content        (tool-completion--copy-string content)
             :content-blocks (mapcar #'tool-completion--copy-string blocks)
             :success-p success-p :category category :denied-p denied-p :error-code code
             :details        (tool-completion--copy-tree details))))))))

(defmethod tool-completion--details ((result contract-recording-result) ancestors)
  "Rebuild bounded recursive provenance without retaining original results."
  (let* ((original   (slot-value result 'original))
         (prefix     (slot-value result 'prefix-length))
         (diagnostic (slot-value result 'diagnostic)))
    (when (and (typep original 'tool-result) (typep prefix '(integer 0))
               (stringp diagnostic) (<= (length diagnostic) 8000))
      ;; Check the actual second argument too, including EQL prefix methods.
      (let* ((closure  (assoc 'tool-contract--prefix-details
                              (rest (assoc (class-name (class-of original))
                                           *tool-completion-method-closures*))))
             (snapshot (when (and closure
                                  (equal (third closure)
                                         (compute-applicable-methods
                                          (second closure) (list original prefix))))
                         (tool-completion--capture original ancestors))))
        (when snapshot
          (let* ((details (slot-value snapshot 'details))
                 (retained
                   (if (eq (class-of original) (find-class 'mcp-content-result))
                       (list :mcp-content :sections
                             (loop for (kind start end) in (getf (rest details) :sections)
                                   when (<= start prefix)
                                     collect (list kind start (min end prefix))))
                       details)))
            (return-from tool-completion--details
              (values (if retained
                          (append retained
                                  (unless (equal details retained)
                                    (list :external-details details))
                                  (list :contract-recording diagnostic))
                          (list :contract-recording :diagnostic diagnostic))
                      t)))))))
  (values nil nil))

(defmethod tool-result-completion-projection ((result tool-result))
  "Opt reviewed exact builtin classes into immutable producer-thread snapshots.
Styled asynchronous Lisp results, images and oversized observer details stay legacy."
  (handler-case
      (tool-completion--capture result nil)
    ((or unbound-slot type-error sexp-config-error print-not-readable
         utf8-conversion-failed) ()
      nil)))
