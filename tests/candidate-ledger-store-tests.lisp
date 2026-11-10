(in-package #:autolith)

;;;; -- Candidate Ledger Store Tests --

(-> ledger-tests--baseline () list)
(defun ledger-tests--baseline ()
  "Return a legacy baseline with unrelated unknown and child attribution."
  '(:migration-version 1 :turns-used 2 :tokens-used 11
    :reservation-charged-tokens 3 :unknown-usage 1
    :child-charges ((:child-id "legacy-child" :tokens 3))
    :legacy-reservations ((:id "legacy-request" :reserved 7 :quarantined t))))

(-> ledger-tests--event (conversation keyword &key (:baseline t) (:payload t)) list)
(defun ledger-tests--event (conversation event &key baseline payload)
  "Return one exact portable event in CONVERSATION's origin."
  (list ':candidate-ledger :version 1
        :origin                     (conversation-candidate-ledger-origin conversation)
        :candidate-id               "candidate-A"
        :operation-id               "12345678-1234-4234-8234-123456789abc"
        :event event :baseline baseline
        :payload                    (or payload
                                        (case event
                                          (:admission
                                           (copy-tree '(:revision 1 :digest "digest-A" :reserved 20
                                                        :review-id "review-A" :job-id "job-A" :execution-id nil)))
                                          (:terminal
                                           '(:kind :measured :tokens 35))
                                          (:recovery
                                           '(:kind :interrupted :evidence "restart-A"))))))

(-> ledger-tests--refused-p (function) boolean)
(defun ledger-tests--refused-p (function)
  "Recognize a typed ledger consistency refusal."
  (handler-case (progn (funcall function) nil)
    (candidate-ledger-consistency-error () t)))

(-> test-candidate-ledger-admission () null)
(defun test-candidate-ledger-admission ()
  "Test atomic baseline, duplicate admission and independent snapshot lookup."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (event        (ledger-tests--event conversation ':admission
                                             :baseline (ledger-tests--baseline))))
      (conversation-candidate-ledger-append conversation event)
      (let ((sequence (conversation-next-sequence conversation)))
        (multiple-value-bind (record outcome)
            (conversation-candidate-ledger-append conversation (copy-tree event))
          (test-assert (and record (eq outcome ':duplicate)) "equal admission is idempotent"))
        (test-assert (= sequence (conversation-next-sequence conversation)) "duplicate appends no bytes"))
      (let ((entry (conversation-candidate-ledger-lookup conversation "candidate-A")))
        (test-assert (equal (getf entry :baseline) (ledger-tests--baseline)) "legacy baseline is exact")
        (test-assert (= 1 (length (getf entry :operations))) "one operation is indexed")
        (setf (getf (getf entry :baseline) :tokens-used) 999)
        (test-assert (= 11 (getf (getf (conversation-candidate-ledger-lookup conversation "candidate-A") :baseline) :tokens-used)) "lookup is detached"))))
  nil)

(-> test-candidate-ledger-terminal () null)
(defun test-candidate-ledger-terminal ()
  "Test exact terminal deduplication, recovery retention and conflicting evidence."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (admission    (ledger-tests--event conversation ':admission :baseline (ledger-tests--baseline)))
           (terminal     (ledger-tests--event conversation ':terminal)))
      (conversation-candidate-ledger-append conversation admission)
      (conversation-candidate-ledger-append conversation (ledger-tests--event conversation ':recovery))
      (conversation-candidate-ledger-append conversation terminal)
      (test-assert (eq ':duplicate (nth-value 1 (conversation-candidate-ledger-append conversation terminal))) "equal terminal is idempotent")
      (test-assert (ledger-tests--refused-p (lambda () (conversation-candidate-ledger-append conversation (ledger-tests--event conversation ':terminal :payload '(:kind :unknown))))) "contradictory terminal refuses")
      (let* ((entry     (conversation-candidate-ledger-lookup conversation "candidate-A"))
             (operation (first (getf entry :operations))))
        (test-assert (getf entry :quarantined-p) "contradiction persists quarantine")
        (test-assert (equal (getf operation :terminal) '(:kind :measured :tokens 35)) "first genuine terminal survives")
        (test-assert (= 1 (length (getf operation :recoveries))) "recovery is separate history")
        (test-assert (= 1 (length (getf operation :conflicts))) "contradiction evidence retained"))))
  nil)

(-> test-candidate-ledger-schema () null)
(defun test-candidate-ledger-schema ()
  "Refuse six malformed or foreign events before durable mutation."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration)))
      (dolist (mutation '(version negative uuid origin terminal baseline))
        (let ((event (ledger-tests--event conversation ':admission :baseline (ledger-tests--baseline))))
          (ecase mutation
            (version
             (setf (getf (rest event) :version) 2))
            (negative
             (setf (getf (getf (rest event) :payload) :reserved) -1))
            (uuid
             (setf (getf (rest event) :operation-id) "restart-counter-1"))
            (origin
             (setf (getf (getf (rest event) :origin) :conversation) "foreign"))
            (terminal
             (setf event (ledger-tests--event conversation ':terminal)))
            (baseline
             (setf (getf (rest event) :baseline) nil)))
          (test-assert (ledger-tests--refused-p (lambda () (conversation-candidate-ledger-append conversation event))) "invalid event refuses")))
      (test-assert (= 1 (conversation-next-sequence conversation)) "no rejected event consumed sequence")
      (test-assert (null (conversation-candidate-ledger-lookup conversation "candidate-A")) "no rejected event published state")))
  nil)

(-> test-candidate-ledger-admission-conflict () null)
(defun test-candidate-ledger-admission-conflict ()
  "Refuse admission identity reuse and preserve the immutable migration baseline."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (event        (ledger-tests--event conversation ':admission :baseline (ledger-tests--baseline))))
      (conversation-candidate-ledger-append conversation event)
      (setf (getf (getf (rest event) :payload) :reserved) 21)
      (test-assert (ledger-tests--refused-p (lambda () (conversation-candidate-ledger-append conversation event))) "same UUID with changed admission refuses")
      (let ((entry (conversation-candidate-ledger-lookup conversation "candidate-A")))
        (test-assert (getf entry :quarantined-p) "conflicting admission quarantines")
        (test-assert (= 20 (getf (getf (first (getf entry :operations)) :admission) :reserved)) "original admission retained"))))
  nil)

(-> test-candidate-ledger-checkpoint () null)
(defun test-candidate-ledger-checkpoint ()
  "Keep accounting separate from clear, edited and replacement specifications."
  (dolist (draft '((:mission-draft :state :cleared)
                   (:mission-draft :state :draft :candidate-id "candidate-A" :specification (:title "edited"))
                   (:mission-draft :state :draft :candidate-id "candidate-B" :specification (:title "replacement"))))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (event        (ledger-tests--event conversation ':admission :baseline (ledger-tests--baseline))))
        (conversation-candidate-ledger-append conversation event)
        (conversation-candidate-ledger-append conversation (ledger-tests--event conversation ':terminal))
        (let ((latest (conversation-append-record conversation draft)))
          (conversation-append-record conversation '(:summary :content "checkpoint" :through-seq 3))
          (let* ((loaded (conversation-load (conversation-pathname conversation)))
                 (entry  (conversation-candidate-ledger-lookup loaded "candidate-A")))
            (test-assert (equal latest (conversation-latest-mission-draft-record loaded)) "latest specification is authoritative")
            (test-assert (equal entry (conversation-candidate-ledger-lookup conversation "candidate-A")) "checkpoint ledger is self-contained")
            (test-assert (eq ':duplicate (nth-value 1 (conversation-candidate-ledger-append loaded event))) "checkpoint retains exact dedup identity")
            (test-assert (equal (getf (first (getf entry :operations)) :terminal) '(:kind :measured :tokens 35)) "terminal survived rotation"))))))
  nil)

(-> ledger-tests--write-native () null)
(defun ledger-tests--write-native ()
  "Write a checkpoint stream for a separate fresh native reader."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration :identifier "ledger-native"
                                              :storage-root            (uiop:ensure-directory-pathname (uiop:getenv "AUTOLITH_LEDGER_NATIVE_ROOT")))))
      (conversation-candidate-ledger-append conversation (ledger-tests--event conversation ':admission :baseline (ledger-tests--baseline)))
      (conversation-candidate-ledger-append conversation (ledger-tests--event conversation ':recovery))
      (conversation-candidate-ledger-append conversation (ledger-tests--event conversation ':terminal))
      (conversation-append-record conversation '(:mission-draft :state :promoted))
      (conversation-append-record conversation '(:summary :content "checkpoint" :through-seq 4))
      (test-assert (= 6 (conversation-next-sequence conversation)) "writer published all events")
      (test-assert (probe-file (conversation-log-pathname conversation)) "writer checkpoint exists")))
  nil)

(-> ledger-tests--read-native () null)
(defun ledger-tests--read-native ()
  "Read a writer's checkpoint using a new native worker process."
  (let* ((path         (merge-pathnames "ledger-native.sexp" (uiop:ensure-directory-pathname (uiop:getenv "AUTOLITH_LEDGER_NATIVE_ROOT"))))
         (conversation (conversation-load path))
         (entry        (conversation-candidate-ledger-lookup conversation "candidate-A"))
         (operation    (first (getf entry :operations))))
    (test-assert (= 11 (getf (getf entry :baseline) :tokens-used)) "baseline survived process restart")
    (test-assert (equal (getf operation :terminal) '(:kind :measured :tokens 35)) "genuine terminal survived process restart")
    (test-assert (= 1 (length (getf operation :recoveries))) "provisional recovery retained separately")
    (test-assert (eq ':promoted (getf (rest (conversation-latest-mission-draft-record conversation)) :state)) "promotion tombstone survived")
    (test-assert (eq ':duplicate (nth-value 1 (conversation-candidate-ledger-append conversation (ledger-tests--event conversation ':terminal)))) "fresh reader deduplicates"))
  nil)

(-> test-candidate-ledger-native-writer () null)
(defun test-candidate-ledger-native-writer ()
  "Write a checkpoint in the supplied origin or an isolated test directory."
  (if (non-empty-string-p (uiop:getenv "AUTOLITH_LEDGER_NATIVE_ROOT"))
      (ledger-tests--write-native)
      (with-test-configuration (configuration root)
        (declare (ignore configuration))
        (with-test-environment (("AUTOLITH_LEDGER_NATIVE_ROOT" (namestring root)))
          (ledger-tests--write-native))))
  nil)

(-> test-candidate-ledger-native-reader () null)
(defun test-candidate-ledger-native-reader ()
  "Read an external checkpoint, or create one in a separate matching process."
  (if (non-empty-string-p (uiop:getenv "AUTOLITH_LEDGER_NATIVE_ROOT"))
      (ledger-tests--read-native)
      (with-test-configuration (configuration root)
        (declare (ignore configuration))
        (with-test-environment (("AUTOLITH_LEDGER_NATIVE_ROOT" (namestring root)))
          (let* ((core     (uiop:getenv "AUTOLITH_TEST_WORKER_CORE"))
                 (request  (merge-pathnames "writer-request.sexp" root))
                 (result   (merge-pathnames "writer-result.sexp" root))
                 (fixtures (merge-pathnames "writer-fixtures/" root)))
            (test-assert (and (non-empty-string-p core) (probe-file core))
                         "native restart uses the source-matched test image")
            (ensure-directories-exist fixtures)
            (with-open-file (stream request :direction ':output :if-exists ':error)
              (write (list :version 1 :cases '("test-candidate-ledger-native-writer")
                           :temporary-root fixtures) :stream stream :readably t))
            (uiop:run-program
             (list (namestring sb-ext:*runtime-pathname*) "--noinform" "--core" core
                   "--end-runtime-options"
                   (namestring (asdf:system-source-directory :autolith))
                   (namestring request) (namestring result))
             :output ':interactive :error-output ':interactive)
            (test-assert (null (getf (let ((*read-eval* nil))
                                      (uiop:read-file-form result)) :failures))
                         "the separate checkpoint writer passed"))
          (ledger-tests--read-native))))
  nil)

(-> test-candidate-ledger-headers () null)
(defun test-candidate-ledger-headers ()
  "Validate empty legacy headers and refuse invalid or legacy ledger snapshots."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (identity     (conversation-pathname conversation))
           (header       (conversation--header-record conversation)))
      (test-assert (not (conversation--property-present-p (rest header) :candidate-ledger))
                   "an unused ledger does not add a header extension")
      (remf (rest header) :candidate-ledger)
      (test-assert (null (conversation-candidate-ledger-lookup
                          (conversation--from-header identity (conversation-log-pathname conversation) header)
                          "candidate-A")) "legacy absent ledger is empty")
      (dolist (snapshot '((:version 2 :records nil)
                          (:version 1 :records (bad-record))
                          (:version 1 :records nil :extra 1)))
        (let ((bad (copy-tree header)))
          (setf (getf (rest bad) :candidate-ledger) snapshot)
          (test-assert (ledger-tests--refused-p
                        (lambda () (conversation--from-header identity (conversation-log-pathname conversation) bad)))
                       "malformed or unsupported checkpoint refuses")))
      (let ((legacy (copy-tree header)))
        (setf (getf (rest legacy) :version) 1)
        (test-assert (null (conversation-candidate-ledger-lookup
                            (conversation--from-header identity identity legacy) "candidate-A"))
                     "genuine legacy header without a ledger loads"))
      (conversation-candidate-ledger-append
       conversation (ledger-tests--event conversation :admission :baseline (ledger-tests--baseline)))
      (dolist (snapshot (list '(:version 1 :records nil)
                             '(:version 2 :records nil)
                             (conversation-candidate-ledger-snapshot conversation)))
        (let ((legacy (copy-tree header)))
          (setf (getf (rest legacy) :version) 1
                (getf (rest legacy) :candidate-ledger) snapshot)
          (test-assert (ledger-tests--refused-p
                        (lambda () (conversation--from-header identity identity legacy)))
                       "legacy ledger extension refuses even when empty")))))
  nil)

(-> test-candidate-ledger-isolation () null)
(defun test-candidate-ledger-isolation ()
  "Enforce baseline once and global UUID collision refusal within an origin."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (first        (ledger-tests--event conversation ':admission :baseline (ledger-tests--baseline)))
           (second       (copy-tree first)))
      (conversation-candidate-ledger-append conversation first)
      (setf (getf (rest second) :candidate-id) "candidate-B")
      (test-assert (ledger-tests--refused-p (lambda () (conversation-candidate-ledger-append conversation second))) "UUID cannot move between candidates")
      (setf (getf (rest second) :candidate-id) "candidate-A"
            (getf (rest second) :operation-id) "22345678-1234-4234-8234-123456789abc")
      (test-assert (ledger-tests--refused-p (lambda () (conversation-candidate-ledger-append conversation second))) "later admission cannot reset baseline")
      (setf (getf (rest second) :baseline) nil)
      (conversation-candidate-ledger-append conversation second)
      (test-assert (= 2 (length (getf (conversation-candidate-ledger-lookup conversation "candidate-A") :operations))) "new operation uses existing baseline")
      (let ((loaded (conversation-load (conversation-pathname conversation))))
        (test-assert (equal (conversation-candidate-ledger-snapshot loaded)
                            (conversation-candidate-ledger-snapshot conversation)) "complete unrotated replay preserves events"))))
  nil)

(-> ledger-tests--append-failure (keyword) null)
(defun ledger-tests--append-failure (mode)
  "Probe admission and terminal append failures using real durable stream writes."
  (dolist (event-kind '(:admission :terminal))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (admission    (ledger-tests--event conversation ':admission :baseline (ledger-tests--baseline)))
             (event        (if (eq event-kind :admission) admission (ledger-tests--event conversation ':terminal)))
             (original     (symbol-function 'log-append))
             (cause        (make-condition 'simple-error :format-control "injected append failure"))
             (observed     nil))
        ;; Persist a neutral record so log-append is the actual append seam.
        (conversation-append-record conversation '(:mission-draft :state :draft))
        (when (eq event-kind :terminal) (conversation-candidate-ledger-append conversation admission))
        (test-call-with-function-replacements
         (list (list 'log-append
                     (lambda (pathname record &key repair-tail-p)
                       (case mode
                         (:committed
                          (funcall original pathname record :repair-tail-p repair-tail-p))
                         (:partial
                          (with-open-file (stream pathname :direction ':output :if-exists ':append)
                            (write-string "(:candidate-ledger :seq " stream) (finish-output stream))))
                       (error cause))))
         (lambda ()
           (handler-case
               (multiple-value-bind (record outcome original-condition)
                   (conversation-candidate-ledger-append conversation event)
                 (setf observed outcome)
                 (test-assert (and record (eq original-condition cause)) "committed outcome preserves original failure"))
             (candidate-ledger-consistency-error (condition)
               (setf observed (candidate-ledger-error-outcome condition))
               (test-assert (eq cause (candidate-ledger-error-original-condition condition)) "refusal preserves original failure")))))
        (test-assert (eq observed (ecase mode (:absent
                                               ':absent)
                                              (:committed
                                               ':committed)
                                              (:partial
                                               ':uncertain))) "append outcome is exact")
        (when (eq mode :committed)
          (test-assert (eq ':duplicate (nth-value 1 (conversation-candidate-ledger-append conversation event))) "same-key retry cannot append again")
          (test-assert (equal (conversation-candidate-ledger-snapshot conversation)
                              (conversation-candidate-ledger-snapshot (conversation-load (conversation-pathname conversation)))) "recovered commit is replayable")))))
  nil)

(-> test-candidate-ledger-append-absent () null)
(defun test-candidate-ledger-append-absent ()
  "Check two definitely-absent admission and terminal failures."
  (ledger-tests--append-failure ':absent))

(-> test-candidate-ledger-append-committed () null)
(defun test-candidate-ledger-append-committed ()
  "Check two committed-then-signaled admission and terminal writes."
  (ledger-tests--append-failure ':committed))

(-> test-candidate-ledger-terminal-kinds () null)
(defun test-candidate-ledger-terminal-kinds ()
  "Preserve exact-child attribution and genuine unbounded unknown evidence."
  (dolist (payload '((:kind :exact-child :tokens 20 :child-id "child-A" :evidence "proof-A")
                     (:kind :unknown)))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (admission    (ledger-tests--event conversation ':admission :baseline (ledger-tests--baseline))))
        (conversation-candidate-ledger-append conversation admission)
        (conversation-candidate-ledger-append conversation (ledger-tests--event conversation ':terminal :payload payload))
        (conversation-append-record conversation '(:summary :content "checkpoint" :through-seq 2))
        (let* ((loaded    (conversation-load (conversation-pathname conversation)))
               (operation (conversation-candidate-ledger-lookup loaded "candidate-A" "12345678-1234-4234-8234-123456789abc")))
          (test-assert (equal payload (getf operation :terminal)) "terminal kind and exact evidence survive")
          (test-assert (null (getf operation :recoveries)) "genuine terminal is not recovery")))))
  nil)

(-> test-candidate-ledger-baseline-schema () null)
(defun test-candidate-ledger-baseline-schema ()
  "Refuse six malformed migration and counter baselines without writing."
  (with-test-configuration (configuration)
    (let ((conversation (conversation-create configuration)))
      (dolist (kind '(version turns unknown child reservation shape))
        (let* ((baseline (copy-tree (ledger-tests--baseline)))
               (event    (ledger-tests--event conversation ':admission :baseline baseline)))
          (ecase kind
            (version
             (setf (getf baseline :migration-version) 2))
            (turns
             (setf (getf baseline :turns-used) -1))
            (unknown
             (setf (getf baseline :unknown-usage) -1))
            (child
             (setf (getf (first (getf baseline :child-charges)) :tokens) -1))
            (reservation
             (setf (getf (first (getf baseline :legacy-reservations)) :reserved) -1))
            (shape
             (remf baseline :tokens-used) (setf (getf (rest event) :baseline) baseline)))
          (test-assert (ledger-tests--refused-p (lambda () (conversation-candidate-ledger-append conversation event))) "malformed baseline refuses")))
      (test-assert (= 1 (conversation-next-sequence conversation)) "malformed baselines did not write")))
  nil)

(-> test-candidate-ledger-append-partial () null)
(defun test-candidate-ledger-append-partial ()
  "Check two invalid-tail admission and terminal outcomes fail closed."
  (ledger-tests--append-failure ':partial))

(-> test-candidate-ledger-collision () null)
(defun test-candidate-ledger-collision ()
  "Refuse cross-candidate UUID reuse and repeated migration baselines."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (admission    (ledger-tests--event conversation :admission :baseline (ledger-tests--baseline))))
      (conversation-candidate-ledger-append conversation admission)
      (let ((foreign (copy-tree admission)))
        (setf (getf (rest foreign) :candidate-id) "candidate-B")
        (test-assert (ledger-tests--refused-p (lambda () (conversation-candidate-ledger-append conversation foreign))) "UUID collision refuses"))
      (let ((second (copy-tree admission)))
        (setf (getf (rest second) :operation-id) "22345678-1234-4234-8234-123456789abc")
        (test-assert (ledger-tests--refused-p (lambda () (conversation-candidate-ledger-append conversation second))) "second baseline refuses")
        (setf (getf (rest second) :baseline) nil)
        (conversation-candidate-ledger-append conversation second))
      (let ((entry (conversation-candidate-ledger-lookup conversation "candidate-A")))
        (test-assert (= 2 (length (getf entry :operations))) "distinct admission retains both operations")
        (test-assert (equal (ledger-tests--baseline) (getf entry :baseline)) "baseline remains exact"))))
  nil)

(-> test-candidate-ledger-terminal-matrix () null)
(defun test-candidate-ledger-terminal-matrix ()
  "Retain exact duplicates and contradictory evidence for all terminal kinds."
  (dolist (payload '((:kind :measured :tokens 35)
                     (:kind :exact-child :tokens 20 :child-id "child-A" :evidence "proof-A")
                     (:kind :unknown)))
    (with-test-configuration (configuration)
      (let* ((conversation (conversation-create configuration))
             (terminal     (ledger-tests--event conversation :terminal :payload payload))
             (conflict     (ledger-tests--event conversation :terminal :payload '(:kind :measured :tokens 36))))
        (conversation-candidate-ledger-append conversation (ledger-tests--event conversation :admission :baseline (ledger-tests--baseline)))
        (conversation-candidate-ledger-append conversation terminal)
        (test-assert (eq :duplicate (nth-value 1 (conversation-candidate-ledger-append conversation terminal))) "terminal duplicate is exact")
        (test-assert (ledger-tests--refused-p (lambda () (conversation-candidate-ledger-append conversation conflict))) "different genuine terminal refuses")
        (let ((sequence (conversation-next-sequence conversation)))
          (test-assert (ledger-tests--refused-p (lambda () (conversation-candidate-ledger-append conversation conflict))) "conflict retry refuses")
          (test-assert (= sequence (conversation-next-sequence conversation)) "equal conflict appends nothing"))
        (conversation-append-record conversation '(:summary :content "checkpoint" :through-seq 3))
        (let* ((loaded    (conversation-load (conversation-pathname conversation)))
               (entry     (conversation-candidate-ledger-lookup loaded "candidate-A"))
               (operation (first (getf entry :operations))))
          (test-assert (getf entry :quarantined-p) "quarantine survives checkpoint")
          (test-assert (equal payload (getf operation :terminal)) "first terminal is authoritative")
          (test-assert (= 1 (length (getf operation :conflicts))) "conflicting payload survives checkpoint")))))
  nil)

(-> test-candidate-ledger-evidence-unavailable () null)
(defun test-candidate-ledger-evidence-unavailable ()
  "Fail closed with exact original condition when durable reconciliation is unavailable."
  (dolist (kind '(:admission :terminal))
    (with-test-configuration (configuration)
      (let* ((conversation  (conversation-create configuration))
             (admission     (ledger-tests--event conversation :admission :baseline (ledger-tests--baseline)))
             (event         (if (eq kind :admission) admission (ledger-tests--event conversation :terminal)))
             (cause         (make-condition 'simple-error :format-control "original write failure"))
             (read-original (symbol-function 'candidate-ledger--durable-records))
             (reads         0)
             (observed      nil))
        (conversation-append-record conversation '(:mission-draft :state :draft))
        (when (eq kind :terminal) (conversation-candidate-ledger-append conversation admission))
        (test-call-with-function-replacements
         (list (list 'log-append (lambda (&rest arguments) (declare (ignore arguments)) (error cause)))
               (list 'candidate-ledger--durable-records
                     (lambda (origin)
                       (if (= 1 (incf reads)) (funcall read-original origin) (error "evidence unavailable")))))
         (lambda ()
           (handler-case (conversation-candidate-ledger-append conversation event)
             (candidate-ledger-consistency-error (condition)
               (setf observed condition)))))
        (test-assert (and observed (eq :uncertain (candidate-ledger-error-outcome observed))) "unavailable evidence is uncertain")
        (test-assert (and observed (eq cause (candidate-ledger-error-original-condition observed))) "uncertainty retains original object")
        (test-assert (= (if (eq kind :terminal) 3 2) (conversation-next-sequence conversation)) "uncertainty grants no admission or publication"))))
  nil)
