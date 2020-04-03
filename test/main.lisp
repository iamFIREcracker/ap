(defpackage :ap/tests
  (:use :cl :pmdb :ap.quickutils :ap.utils :ap))

(in-package :ap/tests)

(1am:test ap/known-scenario/been-busy-for-max
  (multiple-value-bind (end-state)
      (schedule-activities (uiop:read-file-string #P"test/known-scenario.txt"))
    (1am:is (= 16.25 (target-date end-state)))))
