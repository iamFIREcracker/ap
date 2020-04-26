(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "AP.UTILS")
    (defpackage "AP.UTILS"
      (:documentation "Package that contains utility functions.")
      (:use #:cl :pileup :ap.quickutils))))

(in-package "AP.UTILS")

;; need to define this here so sbcl will shut the hell up about it being
;; undefined when compiling quickutils.lisp.  computers are trash.
(defparameter *utilities* nil)
