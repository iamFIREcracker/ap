(defpackage :ap/tests
  (:use :cl :pmdb :ap.quickutils :ap.utils :ap))

(in-package :ap/tests)

(1am:test ap/simple/completion-day
  (1am:is (= 15 (completion-day (uiop:read-file-string #P"test/simple.txt")))))

(1am:test ap/simple/completion-day/preallocations-ignored
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/simple.txt")
                                :ignore-preallocations T))))

(1am:test ap/dependencies/completion-day
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/dependencies.txt")))))

(1am:test ap/dependencies/completion-day/preallocations-ignored
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/dependencies.txt")
                                :ignore-preallocations T))))

(1am:test ap/known-scenario/completion-day
  (1am:is (= 16.25 (completion-day (uiop:read-file-string #P"test/known-scenario.txt")))))
