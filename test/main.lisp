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

(1am:test ap/dependencies/completion-day/no-heuristic
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/dependencies.txt")
                                :disable-heuristic T))))

; (1am:test ap/multiple-preallocations-per-person/completion-day
;   (1am:is (= 5 (completion-day (uiop:read-file-string #P"test/multiple-preallocations-per-person.txt")))))

(1am:test ap/multiple-preallocations-per-person/completion-day/preallocations-ignored
  (1am:is (= 5 (completion-day (uiop:read-file-string #P"test/multiple-preallocations-per-person.txt")
                               :ignore-preallocations T))))

; (1am:test ap/multiple-preallocations-per-person/completion-day/no-heuristic
;   (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/multiple-preallocations-per-person.txt")
;                                 :disable-heuristic T))))

(1am:test ap/known-scenario/completion-day
  (1am:is (= 16.25 (completion-day (uiop:read-file-string #P"test/known-scenario.txt")))))

(1am:test ap/known-scenario/completion-day/skip-weekends
  (1am:is (= 22.25 (completion-day (uiop:read-file-string #P"test/known-scenario.txt")
                                   :skip-weekends T
                                   :today "2020-03-30"))))

(1am:test ap/known-scenario/completion-day/no-heuristic
  (1am:is (= 16.25 (completion-day (uiop:read-file-string #P"test/known-scenario.txt")
                                   :disable-heuristic T))))

(1am:test ap/known-scenario/completion-day/no-heuristic/skip-weekends
  (1am:is (= 22.25 (completion-day (uiop:read-file-string #P"test/known-scenario.txt")
                                   :disable-heuristic T
                                   :skip-weekends T
                                   :today "2020-03-30"))))

; (1am:test ap/known-scenario/completion-day/preallocations-ignored
;   (1am:is (= 16.25 (completion-day (uiop:read-file-string #P"test/known-scenario.txt")
;                                    :ignore-preallocations T))))

(1am:test ap/known-scenario-2/completion-day
  (1am:is (= 15 (completion-day (uiop:read-file-string #P"test/known-scenario-2.txt")))))

(1am:test ap/known-scenario-2/completion-day/skip-weekends
  (1am:is (= 21 (completion-day (uiop:read-file-string #P"test/known-scenario-2.txt")
                                :skip-weekends T
                                :today "2020-04-10"))))

(1am:test ap/known-scenario-2/completion-day/no-heuristic
  (1am:is (= 15 (completion-day (uiop:read-file-string #P"test/known-scenario-2.txt")
                                :disable-heuristic T))))

(1am:test ap/known-scenario-2/completion-day/no-heuristic/skip-weekends
  (1am:is (= 21 (completion-day (uiop:read-file-string #P"test/known-scenario-2.txt")
                                :disable-heuristic T
                                :skip-weekends T
                                :today "2020-04-10"))))

; (1am:test ap/known-scenario-2/completion-day/preallocations-ignored
;   (1am:is (= 13.75 (completion-day (uiop:read-file-string #P"test/known-scenario-2.txt")
;                                    :ignore-preallocations T))))

(1am:test ap/known-scenario-3/completion-day
  (1am:is (= 7.5 (completion-day (uiop:read-file-string #P"test/known-scenario-3.txt")))))

(1am:test ap/known-scenario-3/completion-day/skip-weekends
  (1am:is (= 11.5 (completion-day (uiop:read-file-string #P"test/known-scenario-3.txt")
                                  :skip-weekends T
                                  :today "2020-04-10"))))

(1am:test ap/known-scenario-3/completion-day/no-heuristic
  (1am:is (= 7.5 (completion-day (uiop:read-file-string #P"test/known-scenario-3.txt")
                                 :disable-heuristic T))))

(1am:test ap/known-scenario-3/completion-day/no-heuristic/skip-weekends
  (1am:is (= 11.5 (completion-day (uiop:read-file-string #P"test/known-scenario-3.txt")
                                  :disable-heuristic T
                                  :skip-weekends T
                                  :today "2020-04-10"))))

; (1am:test ap/known-scenario-3/completion-day/preallocations-ignored
;   (1am:is (= 13.75 (completion-day (uiop:read-file-string #P"test/known-scenario-3.txt")
;                                    :ignore-preallocations T))))
