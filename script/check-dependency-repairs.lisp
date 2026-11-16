;;;; Cold finite fixture for the explicit diagnostic-derived candidate.
(require :asdf)
(in-package #:cl-user)

;; Exercise the production entrypoint cold, before preloading selector helpers.
;; Any unresolved source reference must fail this owning dependency check.
(handler-bind ((warning #'error))
  (load (merge-pathnames "production-dependencies.lisp"
                         (uiop:pathname-directory-pathname *load-truename*))))

(defparameter *dependency-repair-arguments* (uiop:command-line-arguments))
(unless (= 5 (length *dependency-repair-arguments*))
  (error "Expected MANIFEST SOURCE-ROOT DERIVED-ROOT LOGS FRESH-CACHE-ROOT."))
(defparameter *dependency-repair-manifest* (pathname (first *dependency-repair-arguments*)))
(defparameter *dependency-repair-sources* (pathname (second *dependency-repair-arguments*)))
(defparameter *dependency-repair-derived* (pathname (third *dependency-repair-arguments*)))
(defparameter *dependency-repair-logs* (pathname (fourth *dependency-repair-arguments*)))
(defparameter *dependency-repair-cache* (pathname (fifth *dependency-repair-arguments*)))
(asdf:initialize-source-registry
 `(:source-registry (:tree ,*dependency-repair-sources*) :ignore-inherited-configuration))
(if (uiop:getenv "AUTOLITH_DEPENDENCY_SELECTION")
    (let ((root (uiop:pathname-parent-directory-pathname
                 (uiop:pathname-directory-pathname *load-truename*))))
      (load (merge-pathnames "script/production-dependencies.lisp" root))
      (uiop:symbol-call '#:cl-user '#:autolith-setup-dependency-sources root))
    (select-derived-dependency-sources
     :manifest *dependency-repair-manifest* :source-root *dependency-repair-sources*
     :derived-root *dependency-repair-derived* :logs *dependency-repair-logs*
     :cache-root *dependency-repair-cache* :authority :reference-matched-diagnostic))

(defparameter *dependency-repair-warnings* nil)
(defparameter *dependency-repair-phase* nil)
(defparameter *dependency-repair-results* nil)

(defun dependency-repair--compile ()
  "Compile the factory consumers and ENCODE before PARSE, retaining raw warnings."
  (handler-bind
      ((warning (lambda (condition)
                  (push (list :phase *dependency-repair-phase*
                              :class (class-name (class-of condition))
                              :message (princ-to-string condition))
                        *dependency-repair-warnings*))))
    (let ((*dependency-repair-phase* :babel-cffi))
      (with-compilation-unit ()
        (asdf:load-system :babel)
        (asdf:load-system :cffi)))
    (asdf:load-system :trivial-gray-streams)
    (let* ((root (merge-pathnames "yason/" *dependency-repair-derived*))
           (package-file (merge-pathnames "package.lisp" root)))
      (load package-file)
      (let ((true (find-symbol "TRUE" "YASON"))
            (false (find-symbol "FALSE" "YASON")))
        (assert (and (not (boundp true)) (not (boundp false))))
        (let ((*dependency-repair-phase* :cold-encode))
          (with-compilation-unit ()
            (multiple-value-bind (fasl warnings failure)
                (compile-file (merge-pathnames "encode.lisp" root)
                              :output-file (merge-pathnames "encode.fasl"
                                                            *dependency-repair-cache*))
              (push (list :compile :encode :warnings warnings :failure failure)
                    *dependency-repair-results*)
              (assert (not failure))
              (load fasl))))
        (assert (and (not (boundp true)) (not (boundp false))))
        (let ((*dependency-repair-phase* :parse))
          (with-compilation-unit ()
            (multiple-value-bind (fasl warnings failure)
                (compile-file (merge-pathnames "parse.lisp" root)
                              :output-file (merge-pathnames "parse.fasl"
                                                            *dependency-repair-cache*))
              (push (list :compile :parse :warnings warnings :failure failure)
                    *dependency-repair-results*)
              (assert (not failure))
              (load fasl))))))))

(dependency-repair--compile)

(defun dependency-repair--iso ()
  "Check both returned values for vector and pointer counter instantiations."
  (dolist (case '((:empty () 0 0 -1 (0 0))
                  (:ascii-cap (65 66) 0 2 1 (1 1))
                  (:slice (90 65 66 89) 1 3 1 (1 2))
                  (:unlimited (65 66) 0 2 -1 (2 2))
                  (:leading-jis (27 36 66 48 33 27 40 66 65) 0 9 1 (1 5))
                  (:transition (27 36 66 48 33 27 40 66 65) 0 9 2 (2 9))
                  (:cap-before-escape (65 27) 0 2 1 (1 1))))
    (destructuring-bind (name bytes start end max expected) case
      (let* ((vector (make-array (+ 2 (length bytes)) :element-type '(unsigned-byte 8)
                                 :initial-element 255))
             (mapping (babel-encodings:lookup-mapping cffi::*foreign-string-mappings*
                                                      :iso-2022-jp)))
        (replace vector bytes :start1 1)
        (let ((actual (multiple-value-list
                       (babel:vector-size-in-chars vector :start (1+ start) :end (1+ end)
                                                  :encoding :iso-2022-jp
                                                  :max (if (plusp max) max 1000)))))
          ;; The unlimited public call omits MAX rather than supplying -1.
          (unless (plusp max)
            (setf actual (multiple-value-list
                          (babel:vector-size-in-chars vector :start (1+ start)
                                                     :end (1+ end)
                                                     :encoding :iso-2022-jp))))
          (assert (equal actual (list (first expected) (1+ (second expected)))))
          (push (list :vector name :values actual) *dependency-repair-results*))
        (cffi:with-foreign-object (pointer :uint8 (length vector))
          (dotimes (index (length vector))
            (setf (cffi:mem-aref pointer :uint8 index) (aref vector index)))
          (let ((actual (multiple-value-list
                         (funcall (babel-encodings:code-point-counter mapping)
                                  pointer (1+ start) (1+ end) max))))
            (assert (equal actual (list (first expected) (1+ (second expected)))))
            (dotimes (index (length vector))
              (assert (= (aref vector index) (cffi:mem-aref pointer :uint8 index))))
            (push (list :pointer name :values actual) *dependency-repair-results*)))
        (assert (= 255 (aref vector 0) (aref vector (1- (length vector)))))))))

(defun dependency-repair--yason ()
  "Check default and rebound Boolean sentinels and parser symbol selection."
  (flet ((encoded (value) (with-output-to-string (stream) (yason:encode value stream))))
    (assert (string= "true" (encoded yason:true)))
    (assert (string= "false" (encoded yason:false)))
    (let* ((yes (make-symbol "DYNAMIC-YES"))
           (no (make-symbol "DYNAMIC-NO"))
           (yason:true yes)
           (yason:false no)
           (yason:*parse-json-booleans-as-symbols* t))
      (assert (string= "true" (encoded yes)))
      (assert (string= "false" (encoded no)))
      (assert (eq yes (yason:parse "true")))
      (assert (eq no (yason:parse "false"))))
    (push '(:yason :default-encode 2 :dynamic-encode 2 :dynamic-parse 2
            :pre-parse-unbound t) *dependency-repair-results*)))

(defun dependency-repair--ksc-condition (thunk source position code)
  "Assert the genuine encoding condition and its complete source context."
  (let ((caught nil))
    (handler-case (funcall thunk)
      (babel-encodings:character-encoding-error (condition)
        (setf caught t)
        (assert (= code (babel-encodings:character-encoding-error-code condition)))
        (assert (eq :ksc_5601 (babel-encodings:character-coding-error-encoding condition)))
        (assert (eq source (babel-encodings:character-coding-error-buffer condition)))
        (assert (= position (babel-encodings:character-coding-error-position condition)))))
    (assert caught)))

(defun dependency-repair--ksc ()
  "Check actual vector/pointer encoders, mapped replacement and round trips."
  (let* ((geta (string (code-char #x3013)))
         (geta-bytes (babel:string-to-octets geta :encoding :ksc_5601 :errorp t))
         (packed (babel-encodings::utf8-to-ksc-5601 #x3013)))
    (assert (integerp packed))
    (assert (equalp geta-bytes (vector (ldb (byte 8 8) packed)
                                      (ldb (byte 8 0) packed))))
    (assert (not (babel-encodings::utf8-to-ksc-5601 #x1f600)))
    (assert (not (babel-encodings::utf8-to-ksc-5601 #xfffd)))
    (dolist (case '((:geta (#x3013) (#x3013))
                    (:mapped (#x3000 #xac00) (#x3000 #xac00))
                    (:replacement (#x1f600) (#x3013))
                    (:mixed (#x3000 #x1f600 #xac00) (#x3000 #x3013 #xac00))
                    (:empty () ())))
      (destructuring-bind (name codes expected-codes) case
        (let* ((input (map 'string #'code-char codes))
               (expected-text (map 'string #'code-char expected-codes))
               (expected (babel:string-to-octets expected-text :encoding :ksc_5601 :errorp t))
               (public (babel:string-to-octets input :encoding :ksc_5601 :errorp nil))
               (source (concatenate 'string geta input geta))
               (count (length input))
               (bytes (* 2 count)))
          (assert (equalp expected public))
          (assert (every (lambda (b) (<= #x21 b #x7e)) public))
          (assert (string= expected-text (babel:octets-to-string public :encoding :ksc_5601 :errorp t)))
          (push (list :ksc :public name :octets (coerce public 'list)
                      :decoded expected-codes) *dependency-repair-results*)
          (dolist (representation '(:vector :pointer))
            (let* ((mapping (babel-encodings:lookup-mapping
                             (ecase representation
                               (:vector babel::*string-vector-mappings*)
                               (:pointer cffi::*foreign-string-mappings*)) :ksc_5601))
                   (vector (make-array (+ 4 bytes) :element-type '(unsigned-byte 8)
                                                  :initial-element 255)))
              (labels ((check (destination read-byte)
                         (let* ((babel-encodings::*suppress-character-coding-errors* t)
                                (values (multiple-value-list
                                         (funcall (babel-encodings:encoder mapping)
                                                  source 1 (1+ count) destination 2)))
                                (decoded (make-string (+ 2 count) :initial-element #\!))
                                (decode-values (multiple-value-list
                                                (funcall (babel-encodings:decoder mapping)
                                                         destination 2 (+ 2 bytes) decoded 1))))
                           (assert (equal values (list bytes)))
                           (assert (equal decode-values (list count)))
                           (assert (string= decoded expected-text :start1 1 :end1 (1+ count)))
                           (assert (char= #\! (char decoded 0) (char decoded (1+ count))))
                           (dotimes (i (+ 4 bytes))
                             (assert (= (funcall read-byte i)
                                        (if (<= 2 i (1+ bytes)) (aref expected (- i 2)) 255))))
                           (push (list :ksc representation name :values values
                                       :decode-values decode-values :bounds t)
                                 *dependency-repair-results*))))
                (ecase representation
                  (:vector (check vector (lambda (i) (aref vector i))))
                  (:pointer
                   (cffi:with-foreign-object (pointer :uint8 (length vector))
                     (dotimes (i (length vector)) (setf (cffi:mem-aref pointer :uint8 i) 255))
                    (check pointer (lambda (i) (cffi:mem-aref pointer :uint8 i))))))))))))
    (let* ((source (map 'string #'code-char '(#x3013 #x3000 #x1f600 #x3013)))
           (ascii (coerce "A" 'simple-base-string)))
      (dolist (errorp '(t :default))
        (dependency-repair--ksc-condition
         (lambda () (if (eq errorp :default)
                        (babel:string-to-octets source :start 1 :end 3 :encoding :ksc_5601)
                        (babel:string-to-octets source :start 1 :end 3 :encoding :ksc_5601 :errorp t)))
         source 2 #x1f600)
        (push (list :ksc :strict-public errorp :position 2 :code #x1f600)
              *dependency-repair-results*))
      (assert (equalp geta-bytes (babel:string-to-octets ascii :encoding :ksc_5601 :errorp nil)))
      (dependency-repair--ksc-condition
       (lambda () (babel:string-to-octets ascii :encoding :ksc_5601 :errorp t)) ascii 0 65)
      (push '(:ksc :base-string :suppressed-and-strict t) *dependency-repair-results*)
      (let ((babel-encodings::*suppress-character-coding-errors* t))
        (assert (equalp geta-bytes (babel:string-to-octets (string (code-char #x1f600))
                                                           :encoding :ksc_5601))))
      (push '(:ksc :dynamic-default :suppressed t) *dependency-repair-results*)
      (dolist (representation '(:vector :pointer))
        (let* ((mapping (babel-encodings:lookup-mapping
                         (if (eq representation :vector) babel::*string-vector-mappings*
                             cffi::*foreign-string-mappings*) :ksc_5601))
               (babel-encodings::*suppress-character-coding-errors* nil))
          (labels ((check (destination byte-at)
                     (dependency-repair--ksc-condition
                      (lambda () (funcall (babel-encodings:encoder mapping) source 1 3 destination 2))
                      source 2 #x1f600)
                     (assert (= 255 (funcall byte-at 0) (funcall byte-at 1)
                                    (funcall byte-at 4) (funcall byte-at 5)))
                     (push (list :ksc :strict representation :position 2 :code #x1f600
                                 :failed-unit-unwritten t) *dependency-repair-results*)))
            (ecase representation
              (:vector (let ((dest (make-array 6 :element-type '(unsigned-byte 8) :initial-element 255)))
                         (check dest (lambda (i) (aref dest i)))))
              (:pointer (cffi:with-foreign-object (dest :uint8 6)
                          (dotimes (i 6) (setf (cffi:mem-aref dest :uint8 i) 255))
                          (check dest (lambda (i) (cffi:mem-aref dest :uint8 i))))))))))))

(dependency-repair--iso)
(dependency-repair--yason)
(dependency-repair--ksc)
(let ((output (merge-pathnames "preloaded-refusal/" *dependency-repair-logs*)))
  (assert
   (handler-case
       (progn
         (prepare-dependency-sources
          :manifest *dependency-repair-manifest* :source-root *dependency-repair-sources*
          :output-root output :logs *dependency-repair-logs*
          :authority :reference-matched-diagnostic)
         nil)
     (dependency-source-mismatch (condition)
       (eq :preloaded-provider (dependency-source-mismatch-reason condition)))))
  (assert (not (probe-file output))))
(dependency-source--write
 (merge-pathnames "ORIGINS.sexp" *dependency-repair-logs*)
 (loop for (system component-name) in '(("babel" ("src" "strings"))
                                      ("cffi" ("src" "strings")))
       for component = (asdf:find-component (asdf:find-system system) component-name)
       for source = (asdf:component-pathname component)
       for fasls = (asdf:output-files (asdf:make-operation 'asdf:compile-op) component)
       do (assert (every #'probe-file fasls))
          (assert (every (lambda (path) (uiop:subpathp path *dependency-repair-cache*))
                         fasls))
       collect (list :system system
                     :asd (asdf:system-source-file (asdf:find-system system))
                     :source source :fasls fasls)))
(dependency-source--write (merge-pathnames "WARNINGS.sexp" *dependency-repair-logs*)
                          (setf *dependency-repair-warnings* (nreverse *dependency-repair-warnings*)))
(dependency-source--write (merge-pathnames "CONTROLS.sexp" *dependency-repair-logs*)
                          (setf *dependency-repair-results* (nreverse *dependency-repair-results*)))
