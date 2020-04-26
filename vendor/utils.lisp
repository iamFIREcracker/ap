(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "AP.UTILS")
    (defpackage "AP.UTILS"
      (:documentation "Package that contains utility functions.")
      (:use #:cl :pileup :ap.quickutils))))

(in-package "AP.UTILS")

(when (boundp '*utilities*)
  (setf *utilities* (union *utilities* '(:A* :HASH-TABLE-INSERT :MAXIMIZATION :PARTIAL-1
                                         :RECURSIVELY :RECUR :SEARCH-BACKTRACK))))

(defun make-hq ()
  "Creates an heap queue."
  (pileup:make-heap #'< :key #'cdr))

(defun hq-empty-p (hq)
  "Returns true if the heap is empty."
  (pileup:heap-empty-p hq))

(defun hq-pop (hq)
  "Pops the first element of the queue (i.e. the element with lowest priority)."
  (car (pileup:heap-pop hq)))

(defun hq-insert (hq item priority)
  "Adds `item` to the queue, and assigns it priority `priority`."
  (pileup:heap-insert (cons item priority ) hq))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun args-append (args name)
    (append args (list name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun args-replace (placeholder args name)
    (subst name placeholder args)))

(defmacro recursively (bindings &body body)
  "Execute `body` recursively, like Clojure's `loop`/`recur`.

  `bindings` should contain a list of symbols and (optional) starting values.

  In `body` the symbol `recur` will be bound to the function for recurring."
  (let ((names (mapcar #'(lambda (b) (if (atom b) b (first b))) bindings))
        (values (mapcar #'(lambda (b) (if (atom b) nil (second b))) bindings)))
    `(labels ((recur (,@names)
                ,@body))
        (recur ,@values))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun args-contain-placeholder-p (args placeholder)
    (recursively ((args args))
      (if (atom args)
        (string= args placeholder)
        (or (recur (car args))
            (recur (cdr args)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun args-replace-placeholder-or-append (args placeholder name)
    (if (args-contain-placeholder-p args placeholder)
      (args-replace placeholder args name)
      (args-append args name))))

(defmacro partial-1 (fn &rest args)
  "Returns a function that invokes `fn` with `args` prepended to the argument it
  receives.  The symbol _ may be used as a placeholder for where the received
  argument should be placed in the argument list.

  Example:
  (defun greet (greeting name)
    (list greeting name))

  (funcall (partial-1 #'greet 'hello) 'fred)
  ; =>
  (HELLO FRED)

  (funcall (partial-1 #'greet _ 'fred) 'hi)
  ; =>
  (HI FRED)
  "
  (with-gensyms (more-arg)
    (let ((actual-args (args-replace-placeholder-or-append args '_ more-arg)))
      `(lambda (,more-arg)
          (funcall ,fn ,@actual-args)))))

(defun a* (init-state &key (init-cost 0) goal-state goalp neighbors
                      heuristic (test 'eql)
                      &aux (cost-so-far (make-hash-table :test test))
                      (come-from (make-hash-table :test test)))
  (when goal-state
    (setf goalp (partial-1 test goal-state)))
  (unless heuristic
    (setf heuristic (constantly 0)))
  (flet ((calc-priority (state-cost state)
           (+ state-cost (funcall heuristic state))))
    (hash-table-insert cost-so-far init-state init-cost)
    (values
      (loop
        :with frontier = (make-hq)
        :initially (hq-insert frontier (cons init-state init-cost) (calc-priority init-cost init-state))
        :until (hq-empty-p frontier)
        :for (state . state-cost) = (hq-pop frontier)
        :when (funcall goalp state) :return state
        :do (when (= state-cost (gethash state cost-so-far))
              (loop
                :for (next-state . cost) :in (funcall neighbors state)
                :for next-cost = (+ state-cost cost)
                :do (multiple-value-bind (existing-cost present-p) (gethash next-state cost-so-far)
                      (when (or (not present-p) (< next-cost existing-cost))
                        (hash-table-insert cost-so-far next-state next-cost)
                        (hash-table-insert come-from next-state state)
                        (hq-insert frontier (cons next-state next-cost) (calc-priority next-cost next-state)))))))
      cost-so-far
      come-from)))

(defun hash-table-insert (ht key value) ;; XXX this cannot be defined as macro, somehow..
  (setf (gethash key ht) value))

(defun maximization (x &key (key 'identity))
  "Returns the max of all the elements of `x`, or NIL if `x` is _empty_.

  If `key` is specified, this function will return the max of all
  the values of `x`, `map`-ed using `key`."
  (loop
    :for e :being :the :elements :of x
    :for v = (funcall key e)
    :maximizing v))

(defun search-backtrack (come-from curr)
  (nreverse (recursively ((curr curr))
              (when curr
                (cons curr (recur (gethash curr come-from)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(a* hash-table-insert maximization partial-1 recursively recur search-backtrack)))
