(in-package #:autolith)

;;;; -- Explicit Provider Routes --

(defclass provider-route ()
  ((provider :initarg       :provider
             :documentation "Canonical provider name, private to this descriptor.")
   (model :initarg       :model
          :documentation "Exact registry model identifier, private to this descriptor."))
  (:documentation "Immutable route identity; public readers return detached strings."))

(-> provider-route-provider (provider-route) string)
(defun provider-route-provider (route)
  "Return a detached copy of ROUTE's canonical provider name."
  (copy-seq (slot-value route 'provider)))

(-> provider-route-model (provider-route) string)
(defun provider-route-model (route)
  "Return a detached copy of ROUTE's exact model identifier."
  (copy-seq (slot-value route 'model)))

(-> provider-route--resolve (string string) provider-route)
(defun provider-route--resolve (provider model)
  "Validate an explicit pair using local registry metadata only."
  (multiple-value-bind (identifier registration)
      (provider-resolve-model model :provider provider)
    (unless (and registration (string= identifier model))
      (error 'configuration-error :message "Fallback routes require exact registered model identifiers."))
    (make-instance 'provider-route
                   :provider (copy-seq (provider-registration-name registration))
                   :model    (copy-seq identifier))))

(-> provider-route--key (provider-route) list)
(defun provider-route--key (route)
  "Return a detached equality key for ROUTE."
  (list (provider-route-provider route) (provider-route-model route)))

(-> provider-routes-resolve (configuration) (values list (integer 1)))
(defun provider-routes-resolve (configuration)
  "Return primary-first immutable route descriptors and the failure threshold.

Resolve local metadata only, never factories, credentials, discovery or login.
Explicit mappings authorize content disclosure, not request compatibility. The
caller must check each alternate against the actual request before dispatch."
  (let* ((model        (config :model configuration))
         (registration (configuration-model-registration configuration))
         (primary      (if registration
                           (provider-route--resolve (provider-registration-name registration) model)
                           (error 'configuration-error :message "Primary route requires a registered provider.")))
         (chains       nil)
         (primaries    nil))
    (dolist (mapping (configuration--fallback-mappings
                      (config :provider-fallback-mappings configuration)))
      (let* ((routes (mapcar (lambda (pair)
                               (provider-route--resolve (first pair) (second pair)))
                             mapping))
             (keys   (mapcar #'provider-route--key routes)))
        (when (or (member (first keys) primaries :test #'equal)
                  (/= (length keys) (length (remove-duplicates keys :test #'equal))))
          (error 'configuration-error :message "Duplicate or self-referential fallback route."))
        (push (first keys) primaries)
        (push (cons (first keys) (rest routes)) chains)))
    (values (cons primary (rest (assoc (provider-route--key primary) chains :test #'equal)))
            (config :provider-fallback-threshold configuration))))

(-> provider-route-configuration (provider-route configuration) configuration)
(defun provider-route-configuration (route configuration)
  "Copy CONFIGURATION for ordinary PROVIDER-CREATE, without constructing it.

Use the normal factory and credential manager later. Reproject the logical
request and recheck capabilities, context, output and continuation limits before
sending content. Never carry primary wire headers or encoded requests over."
  (configuration-copy configuration
                      :model-provider (provider-route-provider route)
                      :model          (provider-route-model route)))
