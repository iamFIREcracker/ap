(defpackage :ap/tests
  (:use :cl :pmdb :ap.quickutils :ap.utils :ap)
  (:export #:run))

(in-package :ap/tests)

(defvar *tests* nil "A list of tests; the default argument to `run'.")

(defun run () (1am:run *tests*))

(defmacro deftest (name &body body)
  "Define a test function and add it to `*tests*`."
  `(progn
     (let ((1am:*tests* nil))
       (1am:test ,name ,@body))
     (pushnew ',name *tests*)))


(deftest ap/days-to-complete/no-affinities-uses-default
  (1am:is (= (let ((ap::*person-productivity* 0.5)
                   (person (ap::make-person :id "person" :allocation 1.0))
                   (activity (ap::make-activity :id "activity"
                                                :effort 10)))
               (ap::days-to-complete nil person activity))
             20)))
#+#:excluded (ap/days-to-complete/no-affinities-uses-default)

(deftest ap/days-to-complete/matching-affinity
  (1am:is (= (let ((ap::*person-productivity* 0.5)
                   (person (ap::make-person :id "person" :allocation 1.0))
                   (activity (ap::make-activity :id "activity"
                                                :effort 10))
                   (affinity (ap::make-affinity :person "person"
                                                :productivity 1.0
                                                :activity "activity")))
               (ap::days-to-complete (list affinity) person activity))
             10)))
#+#:excluded (ap/days-to-complete/matching-affinity)


(deftest ap/simple/completion-day
  (1am:is (= 15 (completion-day (uiop:read-file-string #P"test/simple.txt")
                                :disable-skip-weekends t))))

(deftest ap/simple/completion-day/disable-find-optimal-solution
  (1am:is (= 15 (completion-day (uiop:read-file-string #P"test/simple.txt")
                                :disable-skip-weekends t
                                :disable-find-optimal-solution t))))

(deftest ap/simple/completion-day/ignore-claims/disable-find-optimal-solution
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/simple.txt")
                                :disable-skip-weekends t
                                :ignore-claims t
                                :disable-find-optimal-solution t))))

(deftest ap/simple/completion-day/ignore-claims/disable-find-optimal-solution/half-productivity
  (1am:is (= 20 (completion-day (uiop:read-file-string #P"test/simple.txt")
                                :disable-skip-weekends t
                                :ignore-claims t
                                :disable-find-optimal-solution t
                                :person-productivity "50"))))

(deftest ap/affinities/completion-day
  (1am:is (= 6 (completion-day (uiop:read-file-string #P"test/affinities.txt")
                               :disable-skip-weekends t
                               :person-productivity "50"))))

(deftest ap/affinities/completion-day/ignore-affinity
  (1am:is (= 12 (completion-day (uiop:read-file-string #P"test/affinities.txt")
                                :ignore-affinities t
                                :disable-skip-weekends t
                                :person-productivity "50"))))

#+#:excluded (ap/affinities/completion-day)


(deftest ap/dependencies/completion-day
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/dependencies.txt")
                                :disable-skip-weekends t))))

(deftest ap/dependencies/completion-day/disable-find-optimal-solution
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/dependencies.txt")
                                :disable-skip-weekends t
                                :disable-find-optimal-solution t))))

(deftest ap/dependencies/completion-day/ignore-claims/disable-find-optimal-solution
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/dependencies.txt")
                                :disable-skip-weekends t
                                :ignore-claims t
                                :disable-find-optimal-solution t))))


(deftest ap/multiple-claims-per-person/completion-day
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/multiple-claims-per-person.txt")
                                :disable-skip-weekends t))))

(deftest ap/multiple-claims-per-person/completion-day/disable-find-optimal-solution
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/multiple-claims-per-person.txt")
                                :disable-skip-weekends t
                                :disable-find-optimal-solution t))))

(deftest ap/multiple-claims-per-person/completion-day/ignore-claims/disable-find-optimal-solution
  (1am:is (= 5 (completion-day (uiop:read-file-string #P"test/multiple-claims-per-person.txt")
                               :disable-skip-weekends t
                               :ignore-claims t
                               :disable-find-optimal-solution t))))


(deftest ap/out-of-office/completion-day
  (1am:is (= 8 (completion-day (uiop:read-file-string #P"test/out-of-office.txt")
                               :today "2020-04-28"))))

(deftest ap/out-of-office/completion-day/disable-find-optimal-solution
  (1am:is (= 8 (completion-day (uiop:read-file-string #P"test/out-of-office.txt")
                               :today "2020-04-28"
                               :disable-find-optimal-solution t))))


(deftest ap/multiple-out-of-office-per-line/completion-day
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/multiple-out-of-office-per-line.txt")
                                :today "2020-04-28"))))

(deftest ap/multiple-out-of-office-per-line/completion-day/disable-find-optimal-solution
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/multiple-out-of-office-per-line.txt")
                                :today "2020-04-28"
                                :disable-find-optimal-solution t))))


(deftest ap/multiple-out-of-office-per-line-with-date-ranges/completion-day
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/multiple-out-of-office-per-line-with-date-ranges.txt")
                                :today "2020-04-28"))))

(deftest ap/multiple-out-of-office-per-line-with-date-ranges/completion-day/disable-find-optimal-solution
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/multiple-out-of-office-per-line-with-date-ranges.txt")
                                :today "2020-04-28"
                                :disable-find-optimal-solution t))))


(deftest ap/known-scenario-1/completion-day
  (1am:is (= 22.25 (completion-day (uiop:read-file-string #P"test/known-scenario-1.txt")
                                   :today "2020-03-30"))))

(deftest ap/known-scenario-1/completion-day/disable-find-optimal-solution
  (1am:is (= 22.25 (completion-day (uiop:read-file-string #P"test/known-scenario-1.txt")
                                   :today "2020-03-30"
                                   :disable-find-optimal-solution t))))

; (deftest ap/known-scenario-1/completion-day/ignore-claims/disable-find-optimal-solution
;   (1am:is (= 22.25 (completion-day (uiop:read-file-string #P"test/known-scenario-1.txt")
;                                    :today "2020-03-30"
;                                    :ignore-claims t
;                                    :disable-find-optimal-solution t))))


(deftest ap/known-scenario-2/completion-day
  (1am:is (= 21 (completion-day (uiop:read-file-string #P"test/known-scenario-2.txt")
                                :today "2020-04-10"))))

(deftest ap/known-scenario-2/completion-day/disable-find-optimal-solution
  (1am:is (= 21 (completion-day (uiop:read-file-string #P"test/known-scenario-2.txt")
                                :today "2020-04-10"
                                :disable-find-optimal-solution t))))

(deftest ap/known-scenario-2/completion-day/ignore-claims/disable-find-optimal-solution
  (1am:is (= 19.75 (completion-day (uiop:read-file-string #P"test/known-scenario-2.txt")
                                   :today "2020-04-10"
                                   :ignore-claims t
                                   :disable-find-optimal-solution t))))


(deftest ap/known-scenario-3/completion-day
  (1am:is (= 11.5 (completion-day (uiop:read-file-string #P"test/known-scenario-3.txt")
                                  :today "2020-04-24"))))

(deftest ap/known-scenario-3/completion-day/disable-find-optimal-solution
  (1am:is (= 11.5 (completion-day (uiop:read-file-string #P"test/known-scenario-3.txt")
                                  :today "2020-04-24"
                                  :disable-find-optimal-solution t))))

(deftest ap/known-scenario-3/completion-day/ignore-claims/disable-find-optimal-solution
  (1am:is (= 11.5 (completion-day (uiop:read-file-string #P"test/known-scenario-3.txt")
                                  :today "2020-04-24"
                                  :ignore-claims t
                                  :disable-find-optimal-solution t))))


(deftest ap/known-scenario-4/completion-day
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/known-scenario-4.txt")
                                :today "2020-05-01"))))

(deftest ap/known-scenario-4/completion-day/disable-find-optimal-solution
  (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/known-scenario-4.txt")
                                :today "2020-05-01"
                                :disable-find-optimal-solution t))))

; (deftest ap/known-scenario-4/completion-day/ignore-claims/disable-find-optimal-solution
;   (1am:is (= 10 (completion-day (uiop:read-file-string #P"test/known-scenario-4.txt")
;                                 :today "2020-05-01"
;                                 :ignore-claims t
;                                 :disable-find-optimal-solution t))))
