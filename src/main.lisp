(in-package #:ap)

(defvar *version* nil "Application version")
(defvar *ignore-preallocations* nil "Ignore any person-activity pre-allocation")
(defvar *enable-heuristic* nil "Boolean to tell the planner to use or not heuristic")
(defvar *person-productivity* 1 "Person productivity")
(defvar *today* nil "First day of the simulation")

; Date utils --------------------------------------------------------------------------------------

(defun parse-date (str)
  "Parses a YYYY-MM-DD string, and returns its universal-time value.

  => (parse-date \"1900-01-01\")
  0

  => (parse-date 1986-11-28)
  2742508800"
  (destructuring-bind (second minute hour timezone) (list 0 0 0 0)
    (destructuring-bind (year month date)
        (mapcar #'parse-integer (split-sequence:split-sequence #\- str))
      (encode-universal-time second minute hour date month year timezone))))

(defun next-business-day (n)
  (recursively ((n n)
                (curr-day *today*))
    (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
        (decode-universal-time curr-day)
      (declare (ignore sec min hr dst-p tz))
      (if (>= dow 5)
        (recur n (+ curr-day (* 24 60 60)))
        (if (<  n 1)
          (format nil "~d-~2,'0d-~2,'0d" yr mon day)
          (recur (1- n) (+ curr-day (* 24 60 60))))))))

; Input / parsing ---------------------------------------------------------------------------------

(defun read-from-stream (s)
  (loop
    :for line = (read-line s NIL :eof)
    :until (eq line :eof)
    :collecting line :into lines
    :finally (return (format nil "~{~a~^~&~}" lines))))

(defstruct activity id effort depends-on)

(defun parse-float (s)
  (with-input-from-string (in s)
    (* 1.0 (read in))))

(defun parse-percentage (s &aux (percentage (/ (parse-float s) 100)))
  (unless (and (<= 0 percentage 1))
    (error "~a not in the range [0, 100]" s))
  percentage)

(defun parse-activity (s &aux (parts (split-sequence:split-sequence #\Space s)))
  (make-activity :id (second parts)
                 :effort (parse-float (third parts))
                 :depends-on (nthcdr 3 parts)))

(defstruct person id allocation working-on)

(defun parse-person (s &aux (parts (split-sequence:split-sequence #\Space s)))
  (make-person :id (second parts)
               :allocation (parse-percentage (third parts))
               :working-on (nthcdr 3 parts)))

(defstruct simulation
  activities
  dependencies
  people
  already-working-on
  already-been-busy-for
  already-completed)

(defun calculate-cost (person activity)
  (with-slots (allocation) person
    (with-slots (effort) activity
      (/ effort allocation *person-productivity*))))

(defun parse-simulation (string)
  (let ((activity-id-map (make-hash-table :test 'equal))
        activities
        people)
    (loop
      :for line :in (split-sequence:split-sequence #\Newline string)
      :until (zerop (length line))
      :when (string= (subseq line 0 8) "activity") :do (push (parse-activity line) activities)
      :when (string= (subseq line 0 6) "person") :do (push (parse-person line) people))
    (setf activities (reverse activities)
          people (reverse people))
    (let ((dependencies (make-array (length activities) :initial-element 0))
          (already-working-on (make-array (length people) :initial-element -1))
          (already-been-busy-for (make-array (length people) :initial-element 0))
          (already-completed 0))
      (loop
        :for i :below (length activities)
        :for activity :in activities
        :do (setf (gethash (activity-id activity) activity-id-map) i))
      (loop
        :for i :below (length activities)
        :for activity :in activities
        :do (loop
              :for dep-id :in (activity-depends-on activity)
              :for j = (gethash dep-id activity-id-map)
              :unless j :do (error "Cannot find ~a's dependency: ~a" (activity-id activity) dep-id)
              :do (setf (aref dependencies i)
                        (logior (aref dependencies i) (ash 1 j)))))
      (unless *ignore-preallocations*
        (loop
          :for i :below (length people)
          :for person :in people
          :do (loop
                :for act-id :in (person-working-on person)
                :for j = (gethash act-id activity-id-map)
                :do (setf (aref already-working-on i) j
                          (aref already-been-busy-for i) (calculate-cost person
                                                                         (nth j activities))
                          already-completed (logior already-completed (ash 1 j))))))
      (make-simulation :activities (coerce activities 'vector)
                       :dependencies dependencies
                       :people (coerce people 'vector)
                       :already-working-on already-working-on
                       :already-been-busy-for already-been-busy-for
                       :already-completed already-completed))))

; Search ------------------------------------------------------------------------------------------

(defstruct (cost-info (:conc-name nil)) days dependencies)

(defun precompute-costs (sim)
  (with-slots (activities dependencies people) sim
    (let* ((n (length people))
           (m (length activities))
           (costs (make-array (list n m))))
      (loop
        :for i :below n
        :for person :across people
        :do (loop
              :for j :below m
              :for activity :across activities
              :for days = (calculate-cost person activity)
              :do (setf (aref costs i j) (make-cost-info :days days
                                                         :dependencies (aref dependencies j)))))
      costs)))

(defstruct (worker (:conc-name nil)) been-working-on been-busy-for)
(defstruct (state (:conc-name nil)) workers completed complete-dates)

(defun change (seq i el)
  (let ((c (copy-seq seq)))
    (setf (aref c i) el)
    c))

(defun target-date (state)
  (maximization (workers state) :key #'been-busy-for))

(defun activity-completed-p (j completed)
  (= (logand (ash 1 j) completed) 0))

(defun dependencies-completed-p (dependencies completed)
  (= (logandc2 dependencies completed) 0))

(defun dependencies-already-completed-p (dependencies completed complete-dates been-busy-for)
  (and (dependencies-completed-p dependencies completed)
       (loop
         :for j :from 0 :below (length complete-dates)
         :for complete-date = (aref complete-dates j)
         :always (or (activity-completed-p j dependencies)
                     (>= been-busy-for complete-date)))))

(defun neighbors (costs activity-count state)
  (let ((target-date (target-date state)))
    (with-slots (workers completed complete-dates) state
      (loop
        :for i :below (length workers)
        :for w :across workers
        :for been-busy-for = (been-busy-for w)
        :appending (loop
                     :for j :below activity-count
                     :for cost = (aref costs i j)
                     :when (and
                             cost
                             (activity-completed-p j completed)
                             (dependencies-already-completed-p (dependencies cost) completed complete-dates been-busy-for))
                     :collecting (let* ((been-busy-for (+ (days cost) been-busy-for))
                                        (next-worker (make-worker :been-working-on (logior (been-working-on w) (ash 1 j))
                                                                  :been-busy-for been-busy-for))
                                        (workers (change workers i next-worker))
                                        (complete-dates (change complete-dates j been-busy-for))
                                        (next-state (make-state :workers workers
                                                                :completed (logior completed (ash 1 j))
                                                                :complete-dates complete-dates))
                                        (next-target-date (target-date next-state)))
                                   (cons
                                     next-state
                                     (max 0 (- next-target-date target-date)))))))))

(defun activities-remaining (activities state)
  (with-slots (completed) state
    (loop
      :for j :below (length activities)
      :for a :across activities
      :when (activity-completed-p j completed)
      :collect a)))

(defun effort-remaining (activities state)
  (reduce #'+ (activities-remaining activities state)
          :key #'activity-effort))

; (defun heuristic (people activities worker-count state)
;   (let* ((target-date (target-date state))
;          (effort-remaining (effort-remaining activities state)))
;     (prl target-date effort-remaining (map 'vector #'been-busy-for (workers state)))
;     (loop
;       :while (plusp effort-remaining)
;       :for p :across people
;       :for w :across (workers state)
;       :for remaining = (- target-date (been-busy-for w))
;       :for effort = (* remaining (person-allocation p) *person-productivity*)
;       :do (setf effort (min effort effort-remaining)
;                 effort-remaining (- effort-remaining effort)))
;     (pr effort-remaining)
;     (pr (/ effort-remaining worker-count))
;     (break)
;     (/ effort-remaining worker-count)))

; (defun heuristic (people activities worker-count state)
;   (declare (ignore people worker-count))
;   (let* ((effort-remaining (effort-remaining activities state)))
;     effort-remaining))

(defun heuristic (people activities worker-count state)
  (declare (ignore people))
  (let* ((effort-remaining (effort-remaining activities state)))
    (/ effort-remaining worker-count)))

; Output ------------------------------------------------------------------------------------------

(defun dummy-initial-state (worker-count)
  (make-state :workers (make-array worker-count
                                   :initial-element (make-worker :been-working-on 0
                                                                 :been-busy-for 0))))

(defun create-shedule (sim steps &aux (end-state (nth (1- (length steps)) steps)))
  (let* ((worker-count (length (workers end-state))))
    (loop
      :for (s1 s2) :on (cons (dummy-initial-state worker-count) steps)
      :while s2
      :appending (loop
                   :for i :below worker-count
                   :for w1 :across (workers s1)
                   :for w2 :across (workers s2)
                   :for changed = (- (been-working-on w2) (been-working-on w1))
                   :unless (zerop changed)
                   :collecting (let ((completed (truncate (log changed 2))))
                                 (list
                                   (activity-id (aref (simulation-activities sim) completed))
                                   (been-busy-for w1)
                                   (been-busy-for w2)
                                   (person-id (aref (simulation-people sim) i))))))))

(defun schedule-activities (sim-string)
  (let* ((sim (parse-simulation sim-string))
         (workers (coerce (loop
                            :for been-working-on :across (simulation-already-working-on sim)
                            :for been-busy-for :across (simulation-already-been-busy-for sim)
                            :collect (make-worker :been-working-on (ash 1 been-working-on)
                                                  :been-busy-for been-busy-for))
                          'vector))
         (worker-count (length workers))
         (activity-count (length (simulation-activities sim)))
         (all-activities (1- (ash 1 activity-count)))
         (costs (precompute-costs sim))
         (complete-dates (loop
                           :with result = (make-array activity-count :initial-element -1)
                           :for been-working-on :across (simulation-already-working-on sim)
                           :for been-busy-for :across (simulation-already-been-busy-for sim)
                           :when (>= been-working-on 0)
                           :do (setf (aref result been-working-on) been-busy-for)
                           :finally (return result)))
         (init-state (make-state :workers workers
                                 :completed (simulation-already-completed sim)
                                 :complete-dates complete-dates)))
    (multiple-value-bind (end-state cost-so-far come-from)
        (a* init-state
            :init-cost (target-date init-state)
            :goalp (lambda (state) (= (completed state) all-activities))
            :neighbors (partial-1 #'neighbors costs activity-count)
            :heuristic (and *enable-heuristic* (partial-1 #'heuristic (simulation-people sim)
                                                          (simulation-activities sim)
                                                          worker-count))
            :test 'equalp)
      (declare (ignore cost-so-far))
      (when end-state
        (values
          end-state
          (create-shedule sim (search-backtrack come-from end-state)))))))

(defun sort-schedule(schedule)
    (sort
      (copy-seq schedule)
      (lambda (a1 a2)
        (if (< (second a1) (second a2))
          T
          (if (= (second a1) (second a2))
            (< (third a1) (third a2)))))))

(defun pprint-schedule (schedule &aux (schedule (sort-schedule schedule)))
  (loop
    :for (activity-id from to person-id) :in schedule
    :do (format t "~a ~a ~a ~a~&"
                activity-id
                (next-business-day from)
                (next-business-day to)
                person-id)))

; Options -----------------------------------------------------------------------------------------

(opts:define-opts
  (:name :help
         :description "print the help text and exit"
         :short #\h
         :long "help")
  (:name :version
         :description "print the version and exit"
         :short #\v
         :long "version")
  (:name :ignore-preallocations
         :description "ignore any pre-allocated activity"
         :long "ignore-preallocations")
  (:name :enable-heuristic
         :description "enable heuristic (warning: ap might converge to a sub-optimal solution)"
         :long "enable-heuristic")
  (:name :today
         :description "use DATE (YYYY-MM-DD) as first day of the simulation (defaults to today)"
         :long "today"
         :arg-parser #'parse-date
         :meta-var "DATE")
  (:name :productivity
         :description "percentage of day spent by people working on activities (defaults to 100)"
         :long "productivity"
         :arg-parser #'parse-percentage
         :meta-var "PROD"))

(define-condition exit (error)
  ((code
     :initarg :code
     :initform 0
     :reader exit-code))
  (:report (lambda (condition stream)
             (format stream "Trying to exit with code: ~S"
                     (exit-code condition)))))

(defun parse-opts (argv)
  (multiple-value-bind (options)
      (handler-case
          (handler-bind ((opts:missing-required-option (lambda (condition)
                                                         (if (or (member "-h" argv :test #'equal)
                                                                 (member "--help" argv :test #'equal)
                                                                 (member "-v" argv :test #'equal)
                                                                 (member "--version" argv :test #'equal))
                                                           (invoke-restart 'opts:skip-option)
                                                           (progn
                                                             (format t "~a~%" condition)
                                                             (error 'exit :code 1)))))
                         (opts:arg-parser-failed (lambda (condition)
                                                   (format t "~a~%" condition)
                                                   (error 'exit :code 1))))
            (opts:get-opts argv))
        (opts:unknown-option (condition)
          (format t "~a~%" condition)
          (error 'exit :code 1))
        (opts:missing-arg (condition)
          (format t "~a~%" condition)
          (error 'exit :code 1)))
    (if (getf options :help)
      (progn
        (opts:describe
          :prefix "XXX"
          :args "[keywords]")
        (error 'exit)))
    (if (getf options :version)
      (progn
        (format T "~a~%" *version*)
        (error 'exit)))
    ; optional ones
    (if (getf options :ignore-preallocations)
      (setf *ignore-preallocations* T))
    (if (getf options :enable-heuristic)
      (setf *enable-heuristic* T))
    (setf *today* (or (getf options :today) (get-universal-time)))
    (setf *person-productivity* (or (getf options :productivity) 1))))

; API ---------------------------------------------------------------------------------------------

(defun completion-day (sim-string &key ignore-preallocations disable-heuristic)
  (let ((*ignore-preallocations* ignore-preallocations)
        (*enable-heuristic* (not disable-heuristic)))
    (multiple-value-bind (end-state)
        (schedule-activities sim-string)
      (target-date end-state))))

(defun toplevel ()
  (handler-case (parse-opts (opts:argv))
    (exit (condition)
      (opts:exit (exit-code condition))))
  (multiple-value-bind (end-state schedule)
      (handler-case (schedule-activities (read-from-stream *standard-input*))
        (error (condition)
          (format t "Unexpected error: ~a~&" condition)
          (opts:exit -1)))
    (declare (ignore end-state))
    (pprint-schedule schedule)))

; Scratch -----------------------------------------------------------------------------------------

#+nil
(time
  (let ((*today* (parse-date "2020-03-30"))
        (*ignore-preallocations* nil)
        (*enable-heuristic* t))
    (multiple-value-bind (end-state schedule)
        (schedule-activities (uiop:read-file-string #P"test/known-scenario.txt"))
      (declare (ignore end-state))
      (pprint-schedule schedule))))

#+nil
(time
  (let ((*today* (parse-date "2020-04-10"))
        (*ignore-preallocations* nil)
        (*enable-heuristic* nil))
    (multiple-value-bind (end-state schedule)
        (schedule-activities (uiop:read-file-string #P"test/known-scenario-2.txt"))
      (declare (ignore end-state))
      (pr end-state)
      (pprint-schedule schedule))))
