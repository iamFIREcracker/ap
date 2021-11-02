(handler-case (ql:quickload :ap/tests)
  (error (a) (format t "caught error ~s~%~a~%" a a) (uiop:quit 17)))

(handler-case (time (asdf:test-system :ap))
  (error (a)
    (format T "caught error ~s~%~a~%" a a) (uiop:quit 13)))
