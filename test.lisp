(format t "~a/~a~&" (lisp-implementation-type) (lisp-implementation-version))

;; By adding the current directory to ql:*local-project-directories*, we can
;; QL:QUICKLOAD this without asking users to symlink this repo inside
;; ~/quicklisp/local-projects, or clone it right there in the first place.
(push #P"." ql:*local-project-directories*)
(ql:quickload :ap :silent T)

(ql:quickload :ap/tests :verbose T)
(let ((exit-code 0))
  (handler-case (asdf:test-system :ap)
    (error (c)
      (format T "~&~A~%" c)
      (setf exit-code 1)))
  (uiop:quit exit-code))
