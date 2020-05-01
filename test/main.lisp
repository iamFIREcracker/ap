(defpackage :ap/tests
  (:use :cl :pmdb :ap.quickutils :ap.utils :ap))

(in-package :ap/tests)

(1am:test ap/simple/completion-day
  (1am:is (= 15 (completion-day (uiop:read-file-string #P"test/simple.txt")
                                :disable-skip-weekends t))))

(1am:test ap/simple/completion-day/ignore-preallocations/enable-heuristic
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/simple.txt")
                                :disable-skip-weekends t
                                :ignore-preallocations t
                                :enable-heuristic t))))


(1am:test ap/dependencies/completion-day
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/dependencies.txt")
                                :disable-skip-weekends t))))

(1am:test ap/dependencies/completion-day/ignore-preallocations/enable-heuristic
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/dependencies.txt")
                                :disable-skip-weekends t
                                :ignore-preallocations t
                                :enable-heuristic t))))


(1am:test ap/multiple-preallocations-per-person/completion-day
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/multiple-preallocations-per-person.txt")
                                :disable-skip-weekends t))))

(1am:test ap/multiple-preallocations-per-person/completion-day/ignore-preallocations/enable-heuristic
  (1am:is (= 5 (completion-day (uiop:read-file-string #P"test/multiple-preallocations-per-person.txt")
                               :disable-skip-weekends t
                               :ignore-preallocations t
                               :enable-heuristic t))))


(1am:test ap/out-of-office/completion-day
  (1am:is (= 8 (completion-day (uiop:read-file-string #P"test/out-of-office.txt")
                               :today "2020-04-28"))))

(1am:test ap/out-of-office/completion-day/ignore-preallocations/enable-heuristic
  (1am:is (= 8 (completion-day (uiop:read-file-string #P"test/out-of-office.txt")
                               :today "2020-04-28"
                               :ignore-preallocations t
                               :enable-heuristic t))))


(1am:test ap/known-scenario-1/completion-day
  (1am:is (= 22.25 (completion-day (uiop:read-file-string #P"test/known-scenario-1.txt")
                                   :today "2020-03-30"))))

; (1am:test ap/known-scenario-1/completion-day/ignore-preallocations/enable-heuristic
;   (1am:is (= 22.25 (completion-day (uiop:read-file-string #P"test/known-scenario-1.txt")
;                                    :today "2020-03-30"
;                                    :ignore-preallocations t
;                                    :enable-heuristic t))))


(1am:test ap/known-scenario-2/completion-day
  (1am:is (= 21 (completion-day (uiop:read-file-string #P"test/known-scenario-2.txt")
                                :today "2020-04-10"))))

; (1am:test ap/known-scenario-2/completion-day/ignore-preallocations/enable-heuristic
;   (1am:is (= 15 (completion-day (uiop:read-file-string #P"test/known-scenario-2.txt")
;                                 :today "2020-04-10"
;                                 :ignore-preallocations t
;                                 :enable-heuristic t))))


(1am:test ap/known-scenario-3/completion-day
  (1am:is (= 11.5 (completion-day (uiop:read-file-string #P"test/known-scenario-3.txt")
                                  :today "2020-04-24"))))

; (1am:test ap/known-scenario-3/completion-day/ignore-preallocations/enable-heuristic
;   (1am:is (= 11.5 (completion-day (uiop:read-file-string #P"test/known-scenario-3.txt")
;                                   :today "2020-04-24"
;                                   :ignore-preallocations t
;                                   :enable-heuristic t))))
