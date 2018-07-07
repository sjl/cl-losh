(in-package :losh.functions)

(defun juxt (&rest functions)
  "Return a function that will juxtapose the results of `functions`.

  This is like Clojure's `juxt`.  Given functions `(f0 f1 ... fn)`, this will
  return a new function which, when called with some arguments, will return
  `(list (f0 ...args...) (f1 ...args...) ... (fn ...args...))`.

  Example:

    (funcall (juxt #'list #'+ #'- #'*) 1 2)
    => ((1 2) 3 -1 2)

  "
  (lambda (&rest args)
    (mapcar (rcurry #'apply args) functions)))

(defun nullary (function &optional result)
  "Return a new function that acts as a nullary-patched version of `function`.

  The new function will return `result` when called with zero arguments, and
  delegate to `function` otherwise.

  Examples:

    (max 1 10 2) ; => 10
    (max)        ; => invalid number of arguments

    (funcall (nullary #'max))          ; => nil
    (funcall (nullary #'max 0))        ; => 0
    (funcall (nullary #'max 0) 1 10 2) ; => 10

    (reduce #'max nil)                  ; => invalid number of arguments
    (reduce (nullary #'max) nil)        ; => nil
    (reduce (nullary #'max :empty) nil) ; => :empty
    (reduce (nullary #'max) '(1 10 2))  ; => 10

  "
  (lambda (&rest args)
    (if (null args) result (apply function args))))

(defun fixed-point (function data &key (test 'eql) (limit nil))
  "Find a fixed point of `function`, starting with `data`.

  Successive runs of `function` will be compared with `test`.  Once `test`
  returns true the last result will be returned.

  `limit` can be an integer to limit the maximum number of iterations performed.

  A second value is also returned: `t` if a fixed point was found or `nil` if
  the iteration limit was reached.

  "
  (if (and limit (zerop limit))
    (values data nil)
    (let ((next (funcall function data)))
      (if (funcall test data next)
        (values next t)
        (fixed-point function next :test test :limit (when limit (1- limit)))))))

