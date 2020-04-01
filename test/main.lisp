(defpackage :ap/tests #.cl-user::*ap-use*)

(in-package :ap/tests)

(1am:test ap/known-scenario/been-busy-for-max
  (multiple-value-bind (end-state)
      (schedule-activities (uiop:read-file-string #P"test/known-scenario.txt"))
    (prl end-state)
    (1am:is (= 16.25
               (maximization (been-busy-for end-state))))))
