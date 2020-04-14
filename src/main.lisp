(in-package #:ap)

(defparameter *ignore-preallocations* nil)
(defparameter *enable-heuristic* nil)
(defparameter *dev-productivity* 1)
(defparameter *round-up* nil)
(defparameter *today* nil)

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

(defun parse-activity (s &aux (parts (split-sequence:split-sequence #\Space s)))
  (make-activity :id (second parts)
                 :effort (parse-float (third parts))
                 :depends-on (nthcdr 3 parts)))

(defstruct person id allocation working-on)

(defun parse-person (s &aux (parts (split-sequence:split-sequence #\Space s)))
  (make-person :id (second parts)
               :allocation (/ (parse-float (third parts)) 100)
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
      (let ((cost (/ effort allocation *dev-productivity*)))
        (if *round-up* (ceiling cost) cost)))))

(defun parse-simulation (string)
  (let ((activity-id-map (make-hash-table :test 'equal))
        activities
        people)
    (loop
      :for line :in (split-sequence:split-sequence #\Newline string)
      :until (zerop (length line))
      :when (string= (subseq line 0 8) "activity") :do (push (parse-activity line) activities)
      :when (string= (subseq line 0 6) "person") :do (push (parse-person line) people))
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
(defstruct (state (:conc-name nil)) workers completed)

(defun change (seq i el)
  (let ((c (copy-seq seq)))
    (setf (aref c i) el)
    c))

(defun target-date (state)
  (maximization (workers state) :key #'been-busy-for))

(defun neighbors (costs activity-count state)
  (let ((target-date (target-date state)))
    (with-slots (workers completed) state
      (loop
        :for i :below (length workers)
        :for w :across workers
        :appending (loop
                     :for j :below activity-count
                     :for cost = (aref costs i j)
                     :when (and
                             cost
                             (= (logand (ash 1 j) completed) 0)
                             (= (logandc2 (dependencies cost) completed) 0))
                     :collecting (let* ((next-worker (make-worker :been-working-on (logior (been-working-on w) (ash 1 j))
                                                                  :been-busy-for (+ (days cost) (been-busy-for w))))
                                        (workers (change workers i next-worker))
                                        (next-state (make-state :workers workers
                                                                :completed (logior completed (ash 1 j))))
                                        (next-target-date (target-date next-state)))
                                   (cons
                                     next-state
                                     (max 0 (- next-target-date target-date)))))))))

(defun activities-remaining (activities state)
  (loop
    :for j :below (length activities)
    :for a :across activities
    :when (= (logand (ash 1 j) (completed state)) 0)
    :collect a))

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
;       :for effort = (* remaining (person-allocation p) *dev-productivity*)
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
         (init-state (make-state :workers workers
                                 :completed (simulation-already-completed sim))))
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

(defun completion-day (sim-string &key ignore-preallocations disable-heuristic)
  (let ((*ignore-preallocations* ignore-preallocations)
        (*enable-heuristic* (not disable-heuristic)))
    (multiple-value-bind (end-state)
        (schedule-activities sim-string)
      (target-date end-state))))

(defun today ()
  (if (not *today*)
    (get-universal-time)
    (destructuring-bind (second minute hour) (list 0 0 0)
      (destructuring-bind (year month date)
          (mapcar #'parse-integer (split-sequence:split-sequence #\- *today*))
        (encode-universal-time second minute hour date month year)))))

(defun next-business-day (n)
  (recursively ((n n)
                (curr-day (today)))
    (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
        (decode-universal-time curr-day)
      (declare (ignore sec min hr dst-p tz))
      (if (>= dow 5)
        (recur n (+ curr-day (* 24 60 60)))
        (if (<  n 1)
          (format nil "~d-~2,'0d-~2,'0d" yr mon day)
          (recur (1- n) (+ curr-day (* 24 60 60))))))))

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

(defun toplevel ()
  (multiple-value-bind (end-state schedule)
      (schedule-activities (read-from-stream *standard-input*))
    (declare (ignore end-state))
    (pprint-schedule schedule)))

; Scratch -----------------------------------------------------------------------------------------

#+nil
(let ((*today* "2020-03-30"))
  (multiple-value-bind (end-state schedule)
      (schedule-activities (uiop:read-file-string #P"test/known-scenario.txt"))
    (declare (ignore end-state))
    (pprint-schedule schedule)))
