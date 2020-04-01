(in-package #:ap)

(defparameter *dev-productivity* 1)
(defparameter *round-up* nil)
(defparameter *today* nil)

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
      (loop
        :for i :below (length people)
        :for person :in people
        :do (loop
              :for act-id :in (person-working-on person)
              :for j = (gethash act-id activity-id-map)
              :do (setf (aref already-working-on i) j
                        (aref already-been-busy-for i) (calculate-cost person
                                                                       (nth j activities))
                        already-completed (logior already-completed (ash 1 j)))))
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

(defstruct (state (:conc-name nil)) devs been-busy-for completed)

(defun claim (devs i activity)
  (let ((w (copy-seq devs)))
    (setf (aref w i) activity)
    w))

(defun timetrack (devs i time)
  (let ((w (copy-seq devs)))
    (setf (aref w i) (+ (aref devs i) time))
    w))

(defun cost-change (curr-complete-date next-state)
  (let ((new-complete-date (maximization (been-busy-for next-state))))
    (if (> new-complete-date curr-complete-date)
      (- new-complete-date curr-complete-date)
      0)))

(defun neighbors (costs activity-count state)
  (let ((complete-date (maximization (been-busy-for state))))
    (with-slots (devs been-busy-for completed) state
      (loop
        :for i :below (length devs)
        :appending (loop
                     :for j :below activity-count
                     :for cost = (aref costs i j)
                     :when (and
                             cost
                             (= (logand (ash 1 j) completed) 0)
                             (= (logandc2 (dependencies cost) completed) 0))
                     :collecting (let* ((next-state (make-state :devs (claim devs i j)
                                                                :been-busy-for (timetrack been-busy-for i (days cost))
                                                                :completed (logior completed (ash 1 j))))
                                        (cost (cost-change complete-date next-state)))
                                   (cons next-state cost)))))))

(defun create-shedule (sim steps &aux (end-state (nth (1- (length steps)) steps)))
  (let* ((dev-count (length (devs end-state)))
         (dummy-step (make-state :devs (make-array dev-count :initial-element -1)
                                 :been-busy-for (make-array dev-count :initial-element 0)
                                 :completed 0)))
    (loop
      :for (s1 s2) :on (cons dummy-step steps)
      :while s2
      :appending (loop
                   :for i :below dev-count
                   :for a1 :across (devs s1)
                   :for a2 :across (devs s2)
                   :unless (eq a1 a2)
                   :collecting (list
                                 (activity-id (aref (simulation-activities sim) a2))
                                 (aref (been-busy-for s1) i)
                                 (aref (been-busy-for s2) i)
                                 (person-id (aref (simulation-people sim) i)))))))

(defun schedule-activities (sim-string)
  (let* ((sim (parse-simulation sim-string))
         (activity-count (length (simulation-activities sim)))
         (all-activities (1- (ash 1 activity-count)))
         (costs (precompute-costs sim)))
    (flet ((state-key (s)
             (mkstr (been-busy-for s) (completed s))))
      (multiple-value-bind (end-state cost-so-far come-from)
          (a* (make-state :devs (simulation-already-working-on sim)
                          :been-busy-for (simulation-already-been-busy-for sim)
                          :completed (simulation-already-completed sim))
              :goalp (lambda (state) (= (completed state) all-activities))
              :neighbors (partial-1 #'neighbors costs activity-count)
              :state-key #'state-key
              ; :heuristic (partial-1 #'heuristic all-activities)
              :test 'equal)
        (declare (ignore cost-so-far))
        (when end-state
          (values
            end-state
            (create-shedule sim (search-backtrack come-from end-state :state-key #'state-key))))))))

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

; Scratch -----------------------------------------------------------------------------------------

(defun toplevel ()
  (let ((*today* "2020-03-30"))
    (multiple-value-bind (end-state schedule)
        (schedule-activities (uiop:read-file-string #P"~/Dropbox/resource-allocation.txt"))
      (declare (ignore end-state))
      (pprint-schedule schedule))))
