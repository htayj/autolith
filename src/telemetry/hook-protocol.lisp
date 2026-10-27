(in-package #:autolith)

;;;; -- Lifecycle Metadata --

(defvar *telemetry-request-role* ':agent
  "Public request purpose, scoped at inference and application entry points.")

(defvar *telemetry-tool-corrects-previous-p* nil
  "Whether this tool call explicitly declares a correction of the preceding call.")

(defvar *telemetry-tool-arguments* nil
  "The current call's decoded arguments after removing control metadata; never queued.")

(defvar *telemetry-tool-capture-p* nil
  "True only inside the agent's dynamically scoped capture of one consented tool execution.")

(defvar *telemetry-tool-name* nil
  "The current tool's local name, used only for structured repair metadata.")

(-> telemetry--elapsed-milliseconds (integer) integer)
(defun telemetry--elapsed-milliseconds (start)
  "Return elapsed monotonic milliseconds since START."
  (max 0 (round (* (- (get-internal-real-time) start) 1000)
                internal-time-units-per-second)))

(-> telemetry--call-safely (function) t)
(defun telemetry--call-safely (function)
  "Isolate an optional metadata emission failure without printing its condition."
  (handler-case (funcall function)
    (error () nil)))

(-> telemetry-note-mutation-journal (list) null)
(defun telemetry-note-mutation-journal (record)
  "Project repair journal events without source, values or output.
Evaluation events require an explicit incident link; ordinary probes are not repairs."
  (when *telemetry-run*
    (telemetry--call-safely
     (lambda ()
       (let* ((properties (rest record))
              (kind       (getf properties :kind))
              (result     (getf properties :result))
              (check-p    (member kind '(:exercise :verification)))
              (outcome    (case result
                            (:pending
                             ':proposed)
                            (:installed
                             ':applied)
                            (:passed
                             ':verified)
                            (:failed
                             ':failed)
                            ((:discarded :superseded)
                             ':reverted)
                            ((:committed :durable)
                             ':committed)))
              (contexts   (or (getf properties :repair-contexts)
                              (list (getf properties :repair-context)))))
         ;; A started check or discard is not another proposed repair.
         (when (and outcome (not (and (eq result ':pending)
                                     (member kind '(:exercise :discard :image-commit)))))
           (dolist (context (if (eq kind ':eval)
                               (remove-if-not (lambda (context)
                                                (getf context :incident-id))
                                              contexts)
                               contexts))
             (let* ((incident-id (getf context :incident-id))
                    (attempt-id  (or (getf context :attempt-id)
                                     (telemetry-mutation-report-id
                                      (or (getf properties :mutation) (getf properties :id))))))
               (telemetry-note-repair
                :run *telemetry-run* :report-id (or incident-id attempt-id)
                :incident-id incident-id :attempt-id attempt-id
                :event-kind         (if check-p ':check ':repair)
                :target (getf context :tool) :issue-kind (getf context :issue-kind)
                :operation          (or (getf context :operation) *telemetry-tool-name*)
                :repair-kind        (or (getf context :repair-kind)
                                        (case kind (:definition
                                                    ':redefine-function)
                                                   (:set
                                                    ':change-setting)
                                                   (otherwise
                                                    ':other)))
                :outcome outcome :verified (and check-p (member result '(:passed :checked)) t)
                :summary-function   (lambda () (getf context :diagnostic))
                :verification-scope (when check-p (or (getf context :verification-scope) ':focused))))))))))
  nil)
