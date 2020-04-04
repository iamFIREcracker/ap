(defpackage :ap/tests
  (:use :cl :pmdb :ap.quickutils :ap.utils :ap))

(in-package :ap/tests)

(1am:test ap/known-scenario/completion-day
  (1am:is (= 16.25 (completion-day (uiop:read-file-string #P"test/known-scenario.txt")))))
