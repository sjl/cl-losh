(in-package :losh)

;;;; Sanity -------------------------------------------------------------------
(defmacro -<> (expr &rest forms)
  "Thread the given forms, with `<>` as a placeholder."
  ;; I am going to lose my fucking mind if I have to program lisp without
  ;; a threading macro, but I don't want to add another dep to this library, so
  ;; here we are.
  `(let* ((<> ,expr)
          ,@(mapcar (lambda (form)
                      (if (symbolp form)
                        `(<> (,form <>))
                        `(<> ,form)))
                    forms))
     <>))


;;;; Types --------------------------------------------------------------------
(deftype array-index (&optional (length (1- array-dimension-limit)))
  "An integer in the range `[0, length)`.

  From Alexandria.

  "
  `(integer 0 (,length)))


;;;; Chili Dogs ---------------------------------------------------------------
(defmacro defun-inlineable (name &body body)
  "Like `defun-inline`, but declaims `name` to be `notinline` afterword.

  This is useful when you don't want to inline a function everywhere, but *do*
  want to have the ability to inline it on demand with (declare (inline ...)).

  "
  `(progn
     (declaim (inline ,name))
     (defun ,name ,@body)
     (declaim (notinline ,name))
     ',name))

(defmacro defun-inline (name &body body)
  "Like `defun`, but declaims `name` to be `inline`."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,@body)
     ',name))


;;;; Math ---------------------------------------------------------------------
(defconstant tau (* pi 2)) ; fuck a pi

(defconstant tau/2 (* tau 1/2))
(defconstant 1/2tau (* tau 1/2))

(defconstant tau/4 (* tau 1/4))
(defconstant 1/4tau (* tau 1/4))
(defconstant 2/4tau (* tau 2/4))
(defconstant 3/4tau (* tau 3/4))

(defconstant tau/8 (* tau 1/8))
(defconstant 1/8tau (* tau 1/8))
(defconstant 2/8tau (* tau 2/8))
(defconstant 3/8tau (* tau 3/8))
(defconstant 4/8tau (* tau 4/8))
(defconstant 5/8tau (* tau 5/8))
(defconstant 6/8tau (* tau 6/8))
(defconstant 7/8tau (* tau 7/8))


(defun-inline not= (number &rest more-numbers)
  "Return `nil` if all arguments are numerically equal, `t` otherwise."
  (not (apply #'= number more-numbers)))


(defun-inline degrees (radians)
  "Convert `radians` into degrees.

  The result will be the same type as `tau` and `pi`.

  "
  (* radians (/ 360 tau)))

(defun-inline radians (degrees)
  "Convert `degrees` into radians.

  The result will be the same type as `tau` and `pi`.

  "
  (* degrees (/ tau 360)))


(defun-inline square (x)
  (* x x))

(defun-inline dividesp (n divisor)
  "Return whether `n` is evenly divisible by `divisor`."
  (zerop (mod n divisor)))


(declaim (ftype (function (real real real)
                          (values real &optional))
                norm lerp precise-lerp clamp))

(declaim (ftype (function (real real real real real)
                          (values real &optional))
                map-range))


(defun-inline norm (min max val)
  "Normalize `val` to a number between `0` and `1` (maybe).

  If `val` is between `max` and `min`, the result will be a number between `0`
  and `1`.

  If `val` lies outside of the range, it'll be still be scaled and will end up
  outside the 0/1 range.

  "
  (/ (- val min)
     (- max min)))

(defun-inline lerp (from to n)
  "Lerp together `from` and `to` by factor `n`.

  You might want `precise-lerp` instead.

  "
  (+ from
     (* n (- to from))))

(defun-inline precise-lerp (from to n)
  "Lerp together `from` and `to` by factor `n`, precisely.

  Vanilla lerp does not guarantee `(lerp from to 0.0)` will return exactly
  `from` due to floating-point errors.  This version will return exactly `from`
  when given a `n` of `0.0`, at the cost of an extra multiplication.

  "
  (+ (* (- 1 n) from)
     (* n to)))

(defun-inline map-range (source-from source-to dest-from dest-to source-val)
  "Map `source-val` from the source range to the destination range.

  Example:

    ;          source    dest        value
    (map-range 0.0 1.0   10.0 20.0   0.2)
    => 12.0

  "
  (lerp dest-from dest-to
        (norm source-from source-to source-val)))

(defun-inline clamp (from to value)
  "Clamp `value` between `from` and `to`."
  (let ((max (max from to))
        (min (min from to)))
    (cond
      ((> value max) max)
      ((< value min) min)
      (t value))))

(defun-inline in-range-p (low value high)
  "Return whether `low` <= `value` < `high`."
  (and (<= low value)
       (< value high)))


(defun-inline digit (position integer &optional (base 10))
  "Return the value of the digit at `position` in `integer`.

  Examples:

    (digit 0 135) ; => 5
    (digit 1 135) ; => 3
    (digit 2 135) ; => 1

    (digit 0 #xD4 16) ; => 4
    (digit 1 #xD4 16) ; => 13

  "
  (-<> integer
    (floor <> (expt base position))
    (mod <> base)))


;;;; Bits ---------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *signed-add-docstring-template*
    "Perform ~D-bit signed addition of `x` and `y`.

  Returns two values: the result and a boolean specifying whether
  underflow/overflow occurred.

  ")

  (defparameter *signed-sub-docstring-template*
    "Perform ~D-bit signed subtraction of `x` and `y`.

  Returns two values: the result and a boolean specifying whether
  underflow/overflow occurred.

  "))

(macrolet
    ((define-ops (size)
       (let ((overflow (symb 'overflow- size)))
         `(progn
           (defun-inline ,overflow (value)
             (cond ((> value (1- (expt 2 ,(1- size))))
                    (values (- value (expt 2 ,size)) t))
                   ((< value (- (expt 2 ,(1- size))))
                    (values (+ value (expt 2 ,size)) t))
                   (t (values value nil))))

           (defun-inlineable ,(symb '+/ size) (x y)
             ,(format nil *signed-add-docstring-template* size)
             (declare (optimize speed)
                      (type (signed-byte ,size) x y))
             (,overflow (+ x y)))

           (defun-inlineable ,(symb '-/ size) (x y)
             ,(format nil *signed-sub-docstring-template* size)
             (declare (optimize speed)
                      (type (signed-byte ,size) x y))
             (,overflow (- x y)))))))
  (define-ops 8)
  (define-ops 16)
  (define-ops 32)
  (define-ops 64))


;;;; Random -------------------------------------------------------------------
(defun-inline epsilon (val)
  (etypecase val
    (integer 1)
    (short-float short-float-epsilon)
    (long-float long-float-epsilon)
    (single-float single-float-epsilon)
    (double-float double-float-epsilon)))

(defun-inlineable randomp (&optional (chance 0.5))
  "Return a random boolean with `chance` probability of `t`."
  (< (random 1.0) chance))

(defun random-elt (seq)
  "Return a random element of `seq`, and whether one was available.

  This will NOT be efficient for lists.

  Examples:

    (random-elt #(1 2 3))
    => 1
       T

    (random-elt nil)
    => nil
       nil

  "
  (let ((length (length seq)))
    (if (zerop length)
      (values nil nil)
      (values (elt seq (random length)) t))))

(defun-inlineable random-range (min max)
  "Return a random number in [`min`, `max`)."
  (+ min (random (- max min))))

(defun-inlineable random-range-inclusive (min max)
  "Return a random number in [`min`, `max`]."
  (+ min (random (+ (- max min) (epsilon min)))))

(defun-inlineable random-range-exclusive (min max)
  "Return a random number in (`min`, `max`)."
  (+ (epsilon min) min (random (- max min (epsilon min)))))

(defun-inlineable random-around (value spread)
  "Return a random number within `spread` of `value` (inclusive)."
  (random-range-inclusive (- value spread)
                          (+ value spread)))


(let (spare)
  (defun random-gaussian (&optional (mean 0.0) (standard-deviation 1.0))
    "Return a random float from a gaussian distribution.  NOT THREAD-SAFE (yet)!"
    ;; https://en.wikipedia.org/wiki/Marsaglia_polar_method
    (declare (optimize (speed 3))
             (inline random-range))
    (flet ((scale (n)
             (+ mean (* n standard-deviation))))
      (if spare
        (prog1
            (scale spare)
          (setf spare nil))
        (loop :for u = (random-range -1.0 1.0)
              :for v = (random-range -1.0 1.0)
              :for s = (+ (square u) (square v))
              :while (or (>= s 1.0) (= s 0.0))
              :finally
              (setf s (sqrt (/ (* -2.0 (the (single-float * (0.0)) (log s)))
                               s))
                    spare (* v s))
              (return (scale (* u s))))))))

(defun random-gaussian-integer (&optional (mean 0) (standard-deviation 1))
  "Return a random integer from a gaussian distribution.  NOT THREAD-SAFE (yet)!"
  (values (round (random-gaussian mean standard-deviation))))


(defun d (n sides &optional (plus 0))
  "Roll some dice.

  Examples:

    (d 1 4)     ; rolls 1d4
    (d 2 8)     ; rolls 2d8
    (d 1 10 -1) ; rolls 1d10-1

  "
  (+ (iterate (repeat n)
              (sum (1+ (random sides))))
     plus))


;;;; Functions ----------------------------------------------------------------
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


;;;; Control Flow -------------------------------------------------------------
(defmacro recursively (bindings &body body)
  "Execute `body` recursively, like Clojure's `loop`/`recur`.

  `bindings` should contain a list of symbols and (optional) starting values.

  In `body` the symbol `recur` will be bound to the function for recurring.

  This macro doesn't perform an explicit tail-recursion check like Clojure's
  `loop`.  You know what you're doing, right?

  Example:

      (defun length (some-list)
        (recursively ((list some-list)
                      (n 0))
          (if (null list)
            n
            (recur (cdr list) (1+ n)))))

  "
  (flet ((extract-var (binding)
           (if (atom binding) binding (first binding)))
         (extract-val (binding)
           (if (atom binding) nil (second binding))))
    `(labels ((recur ,(mapcar #'extract-var bindings)
                ,@body))
      (recur ,@(mapcar #'extract-val bindings)))))

(defmacro when-found (var lookup-expr &body body)
  "Perform `body` with `var` bound to the result of `lookup-expr`, when valid.

  `lookup-expr` should be an expression that returns two values, the first being
  the result (which will be bound to `var`) and the second indicating whether
  the lookup was successful.  The standard `gethash` is an example of a function
  that behaves like this.

  If the lookup was successful, `body` will be executed and its value returned.

  Example:

    (multiple-value-bind (val found) (gethash :foo hash)
      (when found
        body))

    ; becomes

    (when-found val (gethash :foo hash)
      body)

  "
  (with-gensyms (found)
    `(multiple-value-bind (,var ,found) ,lookup-expr
       ;; We could preserve and pass along the value of found as a secondary
       ;; return value from the form, but that would kill potential last-call
       ;; optimization (and the ability to return multiple values from `body`).
       (when ,found
         ,@body))))

(defmacro if-found (var lookup-expr then else)
  "Perform `then` or `else` depending on the results of `lookup-expr`.

  `lookup-expr` should be an expression that returns two values, the first being
  the result and the second indicating whether the lookup was successful.  The
  standard `gethash` is an example of a function that behaves like this.

  If the lookup was successful, `then` will be executed with `var` bound to the
  result, and its value returned.

  Otherwise `else` will be executed and returned, without any extra bindings.

  Example:

    (multiple-value-bind (val found) (gethash :foo hash)
      (if found
        'yes
        'no))

    ; becomes

    (if-found val (gethash :foo hash)
      'yes
      'no)

  "
  (with-gensyms (found result)
    `(multiple-value-bind (,result ,found) ,lookup-expr
      (if ,found
        (let ((,var ,result))
          ,then)
        ,else))))

(defmacro gathering (&body body)
  "Run `body` to gather some things and return a fresh list of them.

  `body` will be executed with the symbol `gather` bound to a function of one
  argument.  Once `body` has finished, a list of everything `gather` was called
  on will be returned.

  It's handy for pulling results out of code that executes procedurally and
  doesn't return anything, like `maphash` or Alexandria's `map-permutations`.

  The `gather` function can be passed to other functions, but should not be
  retained once the `gathering` form has returned (it would be useless to do so
  anyway).

  Examples:

    (gathering
      (dotimes (i 5)
        (gather i))
    =>
    (0 1 2 3 4)

    (gathering
      (mapc #'gather '(1 2 3))
      (mapc #'gather '(a b)))
    =>
    (1 2 3 a b)

  "
  (with-gensyms (result)
    `(let ((,result (make-queue)))
      (flet ((gather (item)
               (enqueue item ,result)))
        (declare (dynamic-extent #'gather))
        ,@body)
      (queue-contents ,result))))

(defmacro gathering-vector (options &body body)
  "Run `body` to gather some things and return a fresh vector of them.

  `body` will be executed with the symbol `gather` bound to a function of one
  argument.  Once `body` has finished, a vector of everything `gather` was
  called on will be returned.  This vector will be adjustable and have a fill
  pointer.

  It's handy for pulling results out of code that executes procedurally and
  doesn't return anything, like `maphash` or Alexandria's `map-permutations`.

  The `gather` function can be passed to other functions, but should not be
  retained once the `gathering` form has returned (it would be useless to do so
  anyway).

  Examples:

    (gathering-vector ()
      (dotimes (i 5)
        (gather i))
    =>
    #(0 1 2 3 4)

    (gathering-vector ()
      (mapc #'gather '(1 2 3))
      (mapc #'gather '(a b)))
    =>
    #(1 2 3 a b)

  "
  (destructuring-bind (&key (size 16))
      options
    (with-gensyms (result)
      `(let ((,result (make-array ,size :adjustable t :fill-pointer 0)))
         (flet ((gather (item)
                  (vector-push-extend item ,result)))
           (declare (dynamic-extent #'gather))
           ,@body)
         ,result))))

(defmacro when-let* (binding-forms &body body)
  "Bind the forms in `binding-forms` in order, short-circuiting on `nil`.

  This is like Clojure's `when-let`.  It takes a list of binding and binds them
  like `let*`, but if any of the expressions evaluate to `nil` the process stops
  there and `nil` is immediately returned.

  Examples:

    (when-let* ((a (progn (print :a) 1))
                (b (progn (print :b) 2))
                (c (progn (print :c) 3)))
      (list a b c))
    ; =>
    :A
    :B
    :C
    (1 2 3)

    (when-let* ((a (progn (print :a) 1))
                (b (progn (print :b) nil))
                (c (progn (print :c) 3)))
      (list a b c))
    ; =>
    :A
    :B
    NIL

  "
  (if (null binding-forms)
    `(progn ,@body)
    (destructuring-bind ((symbol expr) . remaining-bindings)
      binding-forms
      `(let ((,symbol ,expr))
         (when ,symbol
           (when-let* ,remaining-bindings ,@body))))))

(defmacro multiple-value-bind* (bindings &body body)
  "Bind each pair in `bindings` with `multiple-value-bind` sequentially.

  Example:

    (multiple-value-bind*
        (((a b) (values 0 1))
         ((c) (values (1+ b)))
      (list a b c))
    ; =>
    ; (0 1 2)

  From https://github.com/phoe/m-m-v-b

  "
  (if (null bindings)
    `(progn ,@body)
    (destructuring-bind ((vars form) &rest bindings) bindings
      `(multiple-value-bind ,vars ,form
         (multiple-value-bind* ,bindings ,@body)))))


;;;; Mutation -----------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun build-zap (place expr env)
    (multiple-value-bind (temps exprs stores store-expr access-expr)
        (get-setf-expansion place env)
      `(let* (,@(mapcar #'list temps exprs)
              (,(car stores) (symbol-macrolet ((% ,access-expr))
                               ,expr)))
         ,store-expr))))

(defmacro zapf (&rest place-expr-pairs &environment env)
  "Update each `place` by evaluating `expr` with `%` bound to the current value.

  `zapf` works like `setf`, but when evaluating the value expressions the symbol
  `%` will be bound to the current value of the place.

  Examples:

    (zapf foo (1+ %)
          (car bar) (if (> % 10) :a :b))

  "
  ;; original idea/name from http://malisper.me/2015/09/29/zap/
  `(progn
    ,@(loop :for (place expr . nil) :on place-expr-pairs :by #'cddr
            :collect (build-zap place expr env))))


(define-modify-macro mulf (factor) *
  "Multiply `place` by `factor` in-place.")


(defun %divf (value &optional divisor)
  (if divisor
    (/ value divisor)
    (/ value)))

(define-modify-macro divf (&optional divisor) %divf
  "Divide `place` by `divisor` in-place.

  If `divisor` is not given, `place` will be set to `(/ 1 place)`.

  ")


(define-modify-macro modf (divisor) mod
  "Modulo `place` by `divisor` in-place.")

(define-modify-macro remainderf (divisor) rem
  "Remainder `place` by `divisor` in-place.")

(define-modify-macro clampf (from to) clamp
  "Clamp `place` between `from` and `to` in-place.")

(define-modify-macro negatef () -
  "Negate the value of `place`.")

(define-modify-macro notf () not
  "Set `place` to `(not place)` in-place.")


(defun %funcall (value function)
  (funcall function value))

(define-modify-macro %callf (function) %funcall
  "Set `place` to the result of calling `function` on its current value.")


(defmacro callf (&rest place-function-pairs)
  "Set each `place` to the result of calling `function` on its current value.

  Examples:

    (let ((x 10) (y 20))
      (callf x #'1-
             y #'1+)
      (list x y))
    =>
    (9 21)
  "
  `(progn
     ,@(loop :for (place function . nil) :on place-function-pairs :by #'cddr
             :collect `(%callf ,place ,function))))


;;;; Arrays -------------------------------------------------------------------
(declaim
  (ftype (function ((array * *) t)) fill-multidimensional-array)
  (ftype (function ((array t *) t)) fill-multidimensional-array-t)
  (ftype (function ((array fixnum *) fixnum)) fill-multidimensional-array-fixnum)
  (ftype (function ((array single-float *) single-float)) fill-multidimensional-array-single-float))


(defmacro do-array ((value array) &body body)
  "Perform `body` once for each element in `array` using `value` for the place.

  `array` can be multidimensional.

  `value` will be `symbol-macrolet`ed to the appropriate `aref`, so you can use
  it as a place if you want.

  Returns the array.

  Example:

    (let ((arr (vector 1 2 3)))
      (do-array (x arr)
        (setf x (1+ x))))
    => #(2 3 4)

  "
  (with-gensyms (i)
    (once-only (array)
      `(iterate (for ,i :index-of-flat-array ,array)
        (symbol-macrolet ((,value (row-major-aref ,array ,i)))
          ,@body)
        (finally (return ,array))))))


(defun-inline fill-mda (array item)
  ;; from #lisp:
  ;;
  ;; <scymtym> sjl: the problem with the displaced array version is that it
  ;; accumulates weak pointers to displaced arrays when the arrays are created
  ;; and only removes them when the arrays are gced. that list is traversed each
  ;; time a displaced array is created. so it can get much worse with more
  ;; repetitions and depends on gc behavior
  ;;
  ;; <sjl> scymtym: ugh, that's an sbcl-specific thing then?
  ;;
  ;; <scymtym> sjl: probably. i don't know how other implementations handle the
  ;; problem. the reason for this weak pointer mechanism is that resizing the
  ;; displaced-to array can propagate to the displaced array which has to be
  ;; a pretty rare case
  #+sbcl
  (fill (sb-ext:array-storage-vector array) item)

  #-(or sbcl)
  (fill (make-array (array-total-size array)
          :adjustable nil
          :fill-pointer nil
          :displaced-to array
          :element-type (array-element-type array))
        item)

  array)


(defun fill-multidimensional-array (array item)
  "Fill `array` with `item`.

  Unlike `fill`, this works on multidimensional arrays.  It won't cons on SBCL,
  but it may in other implementations.

  "
  (fill-mda array item))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fmda-docstring*
    "Fill `array` (which must be of type `(array ~A *)`) with `item`.

  Unlike `fill`, this works on multidimensional arrays.  It won't cons on SBCL,
  but it may in other implementations.

  "))

(defmacro defun-fmda (type)
  `(defun ,(symb 'fill-multidimensional-array- type) (array item)
    ,(format nil *fmda-docstring* type)
    (fill-mda array item)))

(defun-fmda t)
(defun-fmda fixnum)
(defun-fmda single-float)


(defun-inlineable bisect-left (predicate vector target)
  "Bisect `vector` based on `(predicate el target)` and return the LEFT element

  `vector` must be sorted (with `predicate`) before this function is called
  (this is not checked).

  You can think of this function as partitioning the elements into two halves:
  those that satisfy `(predicate el target)` and those that don't, and then
  selecting the element on the LEFT side of the split:

      satisfying  not statisfying
    #(..........  ...............)
               ^
               |
          result

  Two values will be returned: the element and its index.  If no element
  satisfies the predicate `nil` will be returned for both values.

  Examples:

    ;                 index
    ;              0 1 2 3 4 5          val  index
    (bisect #'<  #(1 3 5 7 7 9) 5) ; =>   3, 1
    (bisect #'<= #(1 3 5 7 7 9) 5) ; =>   5, 2
    (bisect #'<= #(1 3 5 7 7 9) 7) ; =>   7, 4
    (bisect #'<  #(1 3 5 7 7 9) 1) ; => nil, nil
    (bisect #'>  #(9 8 8 8 1 0) 5) ; =>   8, 3

  "
  (if (zerop (length vector))
    (values nil nil)
    (iterate
      (with bottom = 0)
      (with top = (length vector))
      (for index = (truncate (+ bottom top) 2))
      (for value = (aref vector index))
      (for result = (funcall predicate value target))
      (if (= bottom index)
        (return (if result
                  (values value index)
                  (values nil nil)))
        (if result
          (setf bottom index)
          (setf top index))))))

(defun-inlineable bisect-right (predicate vector target)
  "Bisect `vector` based on `(predicate el target)` and return the RIGHT element

  `vector` must be sorted (with `predicate`) before this function is called
  (this is not checked).

  You can think of this function as partitioning the elements into two halves:
  those that satisfy `(predicate el target)` and those that don't, and then
  selecting the element on the RIGHT side of the split:

      satisfying  not statisfying
    #(..........  ...............)
                  ^
                  |
                  result

  Two values will be returned: the element and its index.  If every element
  satisfies the predicate `nil` will be returned for both values.

  Examples:

    ;                 index
    ;               0 1 2 3 4 5           val  index
    (rbisect #'<  #(1 3 5 7 7 9) 5)  ; =>   5, 2
    (rbisect #'<= #(1 3 5 7 7 9) 5)  ; =>   7, 3
    (rbisect #'<= #(1 3 5 7 7 9) 7)  ; =>   9, 5
    (rbisect #'<  #(1 3 5 7 7 9) 10) ; => nil, nil
    (rbisect #'>  #(9 8 8 8 1 0) 5)  ; =>   1, 4

  "
  (if (zerop (length vector))
    (values nil nil)
    (iterate
      (with bottom = -1)
      (with top = (1- (length vector)))
      (for index = (ceiling (+ bottom top) 2))
      (for value = (aref vector index))
      (for result = (funcall predicate value target))
      (if (= top index)
        (return (if result
                  (values nil nil)
                  (values value index)))
        (if result
          (setf bottom index)
          (setf top index))))))


;;;; Queues -------------------------------------------------------------------
;;; Based on the PAIP queues (thanks, Norvig), but beefed up a little bit to add
;;; tracking of the queue size.

(declaim (inline make-queue enqueue dequeue queue-empty-p))

(defstruct (queue (:constructor make-queue%))
  (contents nil :type list)
  (last nil :type list)
  (size 0 :type fixnum))


(declaim
  (ftype (function ()
                   (values queue &optional))
         make-queue)
  (ftype (function (queue)
                   (values boolean &optional))
         queue-empty-p)
  (ftype (function (t queue)
                   (values fixnum &optional))
         enqueue)
  (ftype (function (queue)
                   (values t &optional))
         dequeue)
  (ftype (function (queue list)
                   (values fixnum &optional))
         queue-append))


(defun make-queue ()
  "Allocate and return a fresh queue."
  (make-queue%))

(defun queue-empty-p (queue)
  "Return whether `queue` is empty."
  (zerop (queue-size queue)))

(defun enqueue (item queue)
  "Enqueue `item` in `queue`, returning the new size of the queue."
  (let ((cell (cons item nil)))
    (setf (queue-last queue)
          (if (queue-empty-p queue)
            (setf (queue-contents queue) cell)
            (setf (cdr (queue-last queue)) cell))))
  (incf (queue-size queue)))

(defun dequeue (queue)
  "Dequeue an item from `queue` and return it."
  (when (zerop (decf (queue-size queue)))
    (setf (queue-last queue) nil))
  (pop (queue-contents queue)))

(defun queue-append (queue list)
  "Enqueue each element of `list` in `queue` and return the queue's final size."
  (loop :for item :in list
        :for size = (enqueue item queue)
        :finally (return size)))


;;;; Iterate ------------------------------------------------------------------
(defmacro expand-iterate-sequence-keywords ()
  '(list
    :from iterate::from
    :upfrom iterate::upfrom
    :downfrom iterate::downfrom
    :to iterate::to
    :downto iterate::downto
    :above iterate::above
    :below iterate::below
    :by iterate::by
    :with-index iterate::with-index))


(defmacro-driver (FOR var MODULO divisor &sequence)
  "Iterate numerically with `var` bound modulo `divisor`.

  This driver iterates just like the vanilla `for`, but each resulting value
  will be modulo'ed by `divisor` before being bound to `var`.

  Note that the modulo doesn't affect the *iteration*, it just affects the
  variable you *see*.  It is as if you had written two clauses:

    (for temp :from foo :to bar)
    (for var = (mod temp divisor))

  Example:

    (iterate (for i            :from 0 :to 20 :by 3) (collect i))
    (0 3 6 9 12 15 18)

    (iterate (for i :modulo 10 :from 0 :to 20 :by 3) (collect i))
    (0 3 6 9  2  5  8)

  "
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (i d)
      `(progn
        (with ,d = ,divisor)
        (generate ,i ,@(expand-iterate-sequence-keywords))
        (,kwd ,var next (mod (next ,i) ,d))))))


(defmacro-driver (FOR var PAIRS-OF-LIST list)
  "Iterate over the all pairs of `list` (including `(last . first)`).

  Examples:

    (iterate (for p :pairs-of-list (list 1 2 3 4))
             (collect p))
    =>
    ((1 . 2) (2 . 3) (3 . 4) (4 . 1))

  "
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (current l)
      `(progn
        (with ,l = ,list)
        (with ,current = ,l)
        (,kwd ,var next
         (cond
           ((null ,current)
            (terminate))

           ((null (cdr ,current))
            (prog1
                (cons (first ,current) (car ,l))
              (setf ,current nil)))

           (t (prog1
                  (cons (first ,current) (second ,current))
                (setf ,current (cdr ,current))))))))))


(defmacro-clause (AVERAGING expr &optional INTO var)
  "Maintain a running average of `expr` in `var`.

  If `var` is omitted the final average will be returned instead.

  Examples:

    (iterate (for x :in '(0 10 0 10))
             (averaging x))
    =>
    5

    (iterate (for x :in '(1.0 1 2 3 4))
             (averaging (/ x 10) :into avg)
             (collect avg))
    =>
    (0.1 0.1 0.13333334 0.17500001 0.22)

  "
  (with-gensyms (count total)
    (let ((average (or var iterate::*result-var*)))
      `(progn
        (for ,count :from 1)
        (sum ,expr :into ,total)
        (for ,average = (/ ,total ,count))))))

(defmacro-clause (TIMING time-type &optional
                  SINCE-START-INTO since-var
                  PER-ITERATION-INTO per-var)
  "Time [real/run]-time into variables.

  `time-type` should be either the symbol `run-time` or `real-time`, depending
  on which kind of time you want to track.  Times are reported in
  `internal-time-units-per-second`.

  If `since-var` is given, on each iteration it will be bound to the amount of
  time that has passed since the beginning of the loop.

  If `per-var` is given, on each iteration it will be bound to the amount of
  time that has passed since the last time it was set.  On the first iteration
  it will be bound to the amount of time since the loop started.

  If neither var is given, it is as if `since-var` were given and returned as
  the value of the `iterate` statement.

  Note that the position of this clause in the `iterate` statement matters.
  Also, the code movement of `iterate` can change things around.  Overall the
  results should be pretty intuitive, but if you need absolute accuracy you
  should use something else.

  Examples:

    ; sleep BEFORE the timing clause
    (iterate (repeat 3)
             (sleep 1.0)
             (timing real-time :since-start-into s :per-iteration-into p)
             (collect (list (/ s internal-time-units-per-second 1.0)
                            (/ p internal-time-units-per-second 1.0))))
    =>
    ((1.0003 1.0003)
     (2.0050 1.0047)
     (3.0081 1.0030))

    ; sleep AFTER the timing clause
    (iterate (repeat 3)
             (timing real-time :since-start-into s :per-iteration-into p)
             (sleep 1.0)
             (collect (list (/ s internal-time-units-per-second 1.0)
                            (/ p internal-time-units-per-second 1.0))))
    =>
    ((0.0   0.0)
     (1.001 1.001)
     (2.005 1.004))

  "
  (let ((timing-function (ccase time-type
                           ((:real-time real-time) 'get-internal-real-time)
                           ((:run-time run-time) 'get-internal-run-time)))
        (since-var (or since-var (when (null per-var)
                                   iterate::*result-var*))))
    (with-gensyms (start-time current-time previous-time)
      `(progn
        (with ,start-time = (,timing-function))
        (for ,current-time = (,timing-function))
        ,@(when since-var
            `((for ,since-var = (- ,current-time ,start-time))))
        ,@(when per-var
            `((for ,previous-time :previous ,current-time :initially ,start-time)
              (for ,per-var = (- ,current-time ,previous-time))))))))


(defmacro-driver (FOR var IN-LISTS lists)
  "Iterate each element of each list in `lists` in turn.

  Examples:

    (iterate (with things = (list (list 1 2 3) nil (list :a :b :c)))
             (for val :in-lists things)
             (collect val))
    =>
    (1 2 3 :a :b :c)

  "
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (list)
      `(progn
        (generate ,list :in (remove nil ,lists))
        (,kwd ,var next (progn (when (null ,list)
                                 (next ,list))
                               (pop ,list)))))))


(defun seq-done-p (seq len idx)
  (if idx
    (= idx len)
    (null seq)))

(defmacro-driver (FOR var IN-SEQUENCES seqs)
  "Iterate each element of each sequence in `seqs` in turn.

  Examples:

    (iterate (with things = (list (list 1 2 3) nil #(:a :b :c) #()))
             (for val :in-sequences things)
             (collect val))
    =>
    (1 2 3 :a :b :c)

  "
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (seq len idx)
      `(progn
        (with ,len = nil)
        (with ,idx = nil)
        (generate ,seq :in-whatever (remove-if #'emptyp ,seqs))
        (,kwd ,var next
         (progn
           (when (seq-done-p ,seq ,len ,idx)
             (etypecase (next ,seq)
               (cons (setf ,len nil ,idx nil))
               (sequence (setf ,len (length ,seq)
                               ,idx 0))))
           (if ,idx
             (prog1 (elt ,seq ,idx)
               (incf ,idx))
             (pop ,seq))))))))


(defmacro-driver (FOR var IN-WHATEVER seq)
  "Iterate over items in the given sequence.

  Unlike iterate's own `in-sequence` this won't use the horrifyingly inefficient
  `elt`/`length` functions on a list.

  "
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (is-list source i len)
      `(progn
        (with ,source = ,seq)
        (with ,is-list = (typep ,source 'list))
        (with ,len = (if ,is-list -1 (length ,source)))
        (for ,i :from 0)
        (,kwd ,var next (if ,is-list
                          (if ,source
                            (pop ,source)
                            (terminate))
                          (if (< ,i ,len)
                            (elt ,source ,i)
                            (terminate))))))))


(defclause-sequence ACROSS-FLAT-ARRAY INDEX-OF-FLAT-ARRAY
  :access-fn 'row-major-aref
  :size-fn 'array-total-size
  :sequence-type 'array
  :element-type t)


(defun calculate-array-floors (array)
  (iterate (for (nil . later) :on (array-dimensions array))
           (collect (apply #'* later) :result-type vector)))

(defmacro-driver (FOR binding-form IN-ARRAY array)
  "Iterate over `array`, binding the things in `binding-form` each time.

  This driver iterates over every element in `array`.  Multidimensional arrays
  are supported -- the array will be stepped in row-major order.

  `binding-form` should be a list of `(value ...index-vars...)`.  An index
  variable can be `nil` to ignore it.  Missing index variables are ignored.  If
  no index variables are needed, `binding-form` can simply be the value symbol.

  `generate` is supported.  Call `next` on the value symbol to use it.

  Examples:

    (iterate (for (height x y) :in-array some-2d-heightmap-array)
             (draw-terrain x y height))

    (iterate (for (val nil nil z) :in-array some-3d-array)
             (collect (cons z val)))

    (iterate (for val :in-array any-array)
             (print val))

  "
  (destructuring-bind (var &rest index-vars
                           &aux (kwd (if generate 'generate 'for)))
      (ensure-list binding-form)
    (with-gensyms (i arr dims floors)
      `(progn
        (with ,arr = ,array)
        ,@(when (some #'identity index-vars)
            `((with ,dims = (coerce (array-dimensions ,arr) 'vector))
              (with ,floors = (calculate-array-floors ,arr))))
        (generate ,i :from 0 :below (array-total-size ,arr))
        ,@(iterate (for index :in index-vars)
                   (for dim-number :from 0)
                   (when index
                     (collect `(generate ,index :next
                                (mod (floor ,i (svref ,floors ,dim-number))
                                     (svref ,dims ,dim-number))))))
        (,kwd ,var :next
         (progn
           (next ,i)
           ,@(iterate (for index :in index-vars)
                      (when index (collect `(next ,index))))
           (row-major-aref ,arr ,i)))))))


(defun parse-sequence-arguments
    (from upfrom downfrom to downto above below by)
  (let* ((start (or from upfrom downfrom))
         (end (or to downto above below))
         (increment (or by 1))
         (down (or downfrom downto above))
         (exclusive (or below above))
         (done-p (if exclusive
                   (if down '<= '>=)
                   (if down '< '>)))
         (op (if down '- '+)))
    (values start end increment op done-p)))

(defmacro-driver (FOR var CYCLING on-cycle &sequence)
  "Iterate numerically as with `for`, but cycle around once finished.

  `on-cycle` should be a form to execute every time the number cycles back to
  the beginning.  The value of `var` during this form's execution is undefined.

  `generate` is supported.

  Results are undefined if the cycle doesn't contain at least one number.

  Examples:

    (iterate (repeat 10)
             (for x :cycling t :from 0 :to 3)
             (collect x))
    =>
    (0 1 2 3 0 1 2 3 0 1)

    (iterate (repeat 5)
             (for x :cycling (print 'beep) :from 1 :downto 0 :by 0.5)
             (print x))
    =>
    1.0
    0.5
    0.0
    BEEP
    1.0
    0.5

  "
  (declare (ignore iterate::with-index))
  (multiple-value-bind (start end increment op done-p)
      (parse-sequence-arguments iterate::from iterate::upfrom iterate::downfrom
                                iterate::to iterate::downto
                                iterate::above iterate::below
                                iterate::by)
    (let ((kwd (if generate 'generate 'for)))
      (with-gensyms (%counter %start %end %increment)
        `(progn
          (with ,%end = ,end)
          (with ,%increment = ,increment)
          (with ,%counter)
          ;; ugly hack to get numeric contagion right for the first val
          ;; (borrowed from Alexandria)
          (with ,%start = (- (+ ,start ,%increment) ,%increment))
          (,kwd ,var next
           (progn
             (setf ,%counter
                   (if-first-time ,%start (,op ,%counter ,%increment)))
             (if (,done-p ,%counter ,%end)
               (prog1
                   (setf ,%counter ,%start)
                 ,on-cycle)
               ,%counter))))))))


(defmacro-clause (GENERATE-NESTED forms CONTROL-VAR control-var)
  (iterate
    (for (var . args) :in forms)
    (for prev :previous var :initially nil)

    ;; we basically turn
    ;;   (for-nested ((x :from 0 :to n)
    ;;                (y :from 0 :to m)
    ;;                (z :from 0 :to q)))
    ;; into
    ;;   (generate x :from 0 :to n)
    ;;   (generate y :cycling (next x) :from 0 :to m)
    ;;   (generate z :cycling (next y) :from 0 :to q)
    ;;   (generate control-var
    ;;     :next (if-first-time
    ;;             (progn (next x) (next y) (next z))
    ;;             (next z)))
    (collect var :into vars)
    (collect `(generate ,var
               ,@(when prev `(:cycling (next ,prev)))
               ,@args)
             :into cycling-forms)

    (finally (return `(progn
                       ,@cycling-forms
                       (declare (ignorable ,control-var))
                       (generate ,control-var :next
                                 (if-first-time
                                   (progn ,@(iterate (for v :in vars)
                                                     (collect `(next ,v))))
                                   (next ,var))))))))

(defmacro-clause (FOR-NESTED forms)
  "Iterate the given `forms` in a nested fashion.

   `forms` should be a list of iteration forms.  Each one should have the same
   format as a standard `(for var ...)` numeric iteration clause, but WITHOUT
   the `for`.

   The forms will iterate numerically as if in a series of nested loops, with
   later forms cycling around as many times as is necessary.

   Examples:

    (iterate (for-nested ((x :from 0 :to 3)
                          (y :from 0 :below 1 :by 0.4)))
             (print (list x y)))
    =>
    (0 0)
    (0 0.4)
    (0 0.8)
    (1 0)
    (1 0.4)
    (1 0.8)
    (2 0)
    (2 0.4)
    (2 0.8)
    (3 0)
    (3 0.4)
    (3 0.8)

   "
  (with-gensyms (control)
    `(progn
      (generate-nested ,forms :control-var ,control)
      (next ,control))))


(defmacro-clause (FOR delta-vars WITHIN-RADIUS radius &optional
                  SKIP-ORIGIN should-skip-origin)
  "Iterate through a number of delta values within a given radius.

  Imagine you have a 2D array and you want to find all the neighbors of a given
  cell:

     .........
     ...nnn...
     ...nXn...
     ...nnn...
     .........

  You'll need to iterate over the cross product of the array indices from
  `(- target 1)` to `(+ target 1)`.

  You may want to have a larger radius, and you may or may not want to include
  the origin (delta `(0 0)`).

  This clause handles calculating the deltas for you, without needless consing.

  Examples:

    (iterate (for (x) :within-radius 2)
             (collect (list x)))
    =>
    ((-2) (-1) (0) (1) (2))

    (iterate (for (x y) :within-radius 1 :skip-origin t)
             (collect (list x y)))
    =>
    ((-1 -1)
     (-1  0)
     (-1  1)
     ( 0 -1)
     ( 0  1)
     ( 1 -1)
     ( 1  0)
     ( 1  1))

    (iterate (for (x y z) :within-radius 3)
             (collect (list x y z)))
    =>
    ; ... a bigass list of deltas,
    ; the point it is works in arbitrary dimensions.

  "
  (with-gensyms (r -r control skip)
    `(progn
      (with ,r = ,radius)
      (with ,-r = (- ,r))
      (with ,skip = ,should-skip-origin)
      (generate-nested ,(iterate (for var :in (ensure-list delta-vars))
                                 (collect `(,var :from ,-r :to ,r)))
                       :control-var ,control)
      (next ,control)
      (when (and ,skip
                 ,@(iterate (for var :in (ensure-list delta-vars))
                            (collect `(zerop ,var))))
        (next ,control)))))


(defmacro-driver (FOR var EVERY-NTH n DO form)
  "Iterate `var` numerically modulo `n` and run `form` every `n`th iteration.

  The driver can be used to perform an action every N times through the loop.

  `var` itself will be a counter that counts up from to to `n - 1`.

  `generate` is supported.

  Example:

    (iterate (for i :from 1 :to 7)
             (print `(iteration ,i))
             (for tick :every-nth 3 :do (print 'beep))
             (print `(tick ,tick)) (terpri))
    ; =>
    (ITERATION 1)
    (TICK 0)

    (ITERATION 2)
    (TICK 1)

    (ITERATION 3)
    BEEP
    (TICK 2)

    (ITERATION 4)
    (TICK 0)

    (ITERATION 5)
    (TICK 1)

    (ITERATION 6)
    BEEP
    (TICK 2)

    (ITERATION 7)
    (TICK 0)

  "
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (counter limit)
      `(progn
        (with ,limit = ,n)
        (generate ,counter :modulo ,limit :from 0)
        (,kwd ,var :next (prog1 (next ,counter)
                           (when (= ,counter (1- ,limit))
                             ,form)))))))


(defmacro-clause (COLLECT-HASH key-and-value &optional
                  INTO var
                  TEST (test '#'eql))
  "Collect keys and values into a hash table at `var`.

  If `var` is omitted the hash table will be returned instead.

  `key-and-value` should be a list of `(key-expr value-expr)`.

  `test` specifies the test used for the hash table.

  Example:

    (iterate (for x :from 0)
             (for y :in '(a b c))
             (collect-hash ((1+ x) y)))
    ; => {1 a
    ;     2 b
    ;     3 c}

  "
  (destructuring-bind (key value) key-and-value
    (let ((hash-table (or var iterate::*result-var*)))
      `(progn
         (with ,hash-table = (make-hash-table :test ,test))
         (setf (gethash ,key ,hash-table) ,value)))))

(defmacro-clause (ORING expr &optional INTO var)
  (let ((result (or var iterate::*result-var*)))
    `(reducing ,expr :by #'or :into ,var :initial-value nil)))

(defmacro-clause (ANDING expr &optional INTO var)
  (let ((result (or var iterate::*result-var*)))
    `(reducing ,expr :by #'and :into ,var :initial-value t)))


(defun keywordize-clause (clause)
  (iterate
    (for (k v . nil) :on clause :by #'cddr)
    (collect (ensure-keyword k))
    (collect v)))

(defun keywordize-some-of-clause (clause)
  ; please kill me
  (append (take 2 clause) (keywordize-clause (nthcdr 2 clause))))

(defun macroexpand-iterate (clause)
  "Macroexpand the given iterate clause/driver.

  Example:

    (macroexpand-iterate '(averaging (+ x 10) :into avg))
    =>
    (PROGN
     (FOR #:COUNT630 :FROM 1)
     (SUM (+ X 10) :INTO #:TOTAL631)
     (FOR AVG = (/ #:TOTAL631 #:COUNT630)))

  "
  ;; Given a clause like (for foo in-whatever bar) we need to:
  ;;
  ;; 1. Look up the appropriate macro (confusingly named via gentemp).  This
  ;;    requires calling `iterate::get-clause-info` with an appropriately-formed
  ;;    clause.
  ;;
  ;;    The first item in the clause must be a normal (non-keyword) symbol, but
  ;;    the rest of the clause keywords must be actual keyword symbols.
  ;;
  ;; 2. Build the appropriate list to `macroexpand-1`.  This should be of the
  ;;    form `(the-wierdly-named-macro ...)`.
  ;;
  ;;    Note that the macro will be expecting the clause to come in as keyword
  ;;    arguments, so unlike in step 1 ALL the clause keywords need to be actual
  ;;    keywords, including the first.
  ;;
  ;; We'll also bind `iterate::*result-var*` so any macros that use it won't
  ;; immediately shit the bed.
  (let ((iterate::*result-var* 'iterate::*result-var*))
    (values
      (macroexpand-1 (cons (iterate::clause-info-function
                             (iterate::get-clause-info
                               (keywordize-some-of-clause clause)))
                           (keywordize-clause clause))))))


(defmacro-driver (FOR var IN-HASHSET hset)
  (let ((kwd (if generate 'generate 'for)))
    `(,kwd (,var) :in-hashtable (hash-set-storage ,hset))))

(defmacro-driver (FOR var RECURSIVELY expr INITIALLY init)
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (initially (setf ,var ,init))
       (,kwd ,var = ,expr))))


;;;; Hash Tables --------------------------------------------------------------
(defun mutate-hash-values (function hash-table)
  "Replace each value in `hash-table` with the result of calling `function` on it.

  Returns the hash table.

  "
  (iterate (for (key value) :in-hashtable hash-table)
           (setf (gethash key hash-table)
                 (funcall function value)))
  hash-table)

(defun hash-table-contents (hash-table)
  "Return a fresh list of `(key value)` elements of `hash-table`."
  (gathering (maphash (compose #'gather #'list) hash-table)))


;;;; Sequences ----------------------------------------------------------------
(defun prefix-sums (sequence)
  "Return a list of the prefix sums of the numbers in `sequence`.

  Example:

    (prefix-sums '(10 10 10 0 1))
    => (10 20 30 30 31)

  "
  (iterate
    (for i :in-whatever sequence)
    (sum i :into s)
    (collect s)))

(defun frequencies (sequence &key (test 'eql))
  "Return a hash table containing the frequencies of the items in `sequence`.

  Uses `test` for the `:test` of the hash table.

  Example:

    (frequencies '(foo foo bar))
    => {foo 2
        bar 1}

  "
  (iterate
    (with result = (make-hash-table :test test))
    (for i :in-whatever sequence)
    (incf (gethash i result 0))
    (finally (return result))))

(defun proportions (sequence &key (test 'eql) (float t))
  "Return a hash table containing the proportions of the items in `sequence`.

  Uses `test` for the `:test` of the hash table.

  If `float` is `t` the hash table values will be coerced to floats, otherwise
  they will be left as rationals.

  Example:

    (proportions '(foo foo bar))
    => {foo 0.66666
        bar 0.33333}

    (proportions '(foo foo bar) :float nil)
    => {foo 2/3
        bar 1/3}

  "
  (let* ((freqs (frequencies sequence :test test))
         (total (reduce #'+ (hash-table-values freqs)
                        :initial-value (if float 1.0 1))))
    (mutate-hash-values (lambda (v) (/ v total))
                        freqs)))

(defun group-by (function sequence &key (test #'eql) (key #'identity))
  "Return a hash table of the elements of `sequence` grouped by `function`.

  This function groups the elements of `sequence` into buckets.  The bucket for
  an element is determined by calling `function` on it.

  The result is a hash table (with test `test`) whose keys are the bucket
  identifiers and whose values are lists of the elements in each bucket.  The
  order of these lists is unspecified.

  If `key` is given it will be called on each element before passing it to
  `function` to produce the bucket identifier.  This does not effect what is
  stored in the lists.

  Examples:

    (defparameter *items* '((1 foo) (1 bar) (2 cats) (3 cats)))

    (group-by #'first *items*)
    ; => { 1 ((1 foo) (1 bar))
    ;      2 ((2 cats))
    ;      3 ((3 cats)) }

    (group-by #'second *items*)
    ; => { foo  ((1 foo))
    ;      bar  ((1 bar))
    ;      cats ((2 cats) (3 cats)) }

    (group-by #'evenp *items* :key #'first)
    ; => { t   ((2 cats))
    ;      nil ((1 foo) (1 bar) (3 cats)) }

  "
  (iterate
    (with result = (make-hash-table :test test))
    (for i :in-whatever sequence)
    (push i (gethash (funcall function (funcall key i)) result))
    (finally (return result))))


(defun-inline take-list (n list)
  (iterate (declare (iterate:declare-variables))
           (repeat n)
           (for item :in list)
           (collect item)))

(defun-inline take-seq (n seq)
  (subseq seq 0 (min n (length seq))))

(defun take (n seq)
  "Return a fresh sequence of the first `n` elements of `seq`.

  The result will be of the same type as `seq`.

  If `seq` is shorter than `n` a shorter result will be returned.

  Example:

    (take 2 '(a b c))
    => (a b)

    (take 4 #(1))
    => #(1)

  From Serapeum.

  "
  (check-type n array-index)
  (ctypecase seq
    (list (take-list n seq))
    (sequence (take-seq n seq))))


(defun-inline take-while-list (predicate list)
  (iterate (for item :in list)
           (while (funcall predicate item))
           (collect item)))

(defun-inline take-while-seq (predicate seq)
  (subseq seq 0 (position-if-not predicate seq)))

(defun take-while (predicate seq)
  "Take elements from `seq` as long as `predicate` remains true.

  The result will be a fresh sequence of the same type as `seq`.

  Example:

    (take-while #'evenp '(2 4 5 6 7 8))
    ; => (2 4)

    (take-while #'evenp #(1))
    ; => #()

  "
  (ctypecase seq
    (list (take-while-list predicate seq))
    (sequence (take-while-seq predicate seq))))


(defun-inline drop-list (n list)
  (copy-list (nthcdr n list)))

(defun-inline drop-seq (n seq)
  (subseq seq (min n (length seq))))

(defun drop (n seq)
  "Return a fresh copy of the `seq` without the first `n` elements.

  The result will be of the same type as `seq`.

  If `seq` is shorter than `n` an empty sequence will be returned.

  Example:

    (drop 2 '(a b c))
    => (c)

    (drop 4 #(1))
    => #()

  From Serapeum.

  "
  (check-type n array-index)
  (ctypecase seq
    (list (drop-list n seq))
    (sequence (drop-seq n seq))))


(defun-inline drop-while-list (predicate list)
  (iterate (for tail :on list)
           (while (funcall predicate (first tail)))
           (finally (return (copy-list tail)))))

(defun-inline drop-while-seq (predicate seq)
  (let ((start (position-if-not predicate seq)))
    (if start
      (subseq seq start)
      (subseq seq 0 0))))

(defun drop-while (predicate seq)
  "Drop elements from `seq` as long as `predicate` remains true.

  The result will be a fresh sequence of the same type as `seq`.

  Example:

    (drop-while #'evenp '(2 4 5 6 7 8))
    ; => (5 6 7 8)

    (drop-while #'evenp #(2))
    ; => #(2)

  "
  (ctypecase seq
    (list (drop-while-list predicate seq))
    (sequence (drop-while-seq predicate seq))))


(defun extrema (predicate sequence)
  "Return the smallest and largest elements of `sequence` according to `predicate`.

  `predicate` should be a strict ordering predicate (e.g. `<`).

  Returns the smallest and largest elements in the sequence as two values,
  respectively.

  "
  (iterate (with min = (elt sequence 0))
           (with max = (elt sequence 0))
           (for el :in-whatever sequence)
           (when (funcall predicate el min) (setf min el))
           (when (funcall predicate max el) (setf max el))
           (finally (return (values min max)))))


(defun enumerate (sequence &key (start 0) (step 1) key)
  "Return an alist of `(n . element)` for each element of `sequence`.

  `start` and `step` control the values generated for `n`, NOT which elements of
  the sequence are enumerated.

  Examples:

    (enumerate '(a b c))
    ; => ((0 . A) (1 . B) (2 . C))

    (enumerate '(a b c) :start 1)
    ; => ((1 . A) (2 . B) (3 . C))

    (enumerate '(a b c) :key #'ensure-keyword)
    ; => ((0 . :A) (1 . :B) (2 . :C))

  "
  (iterate (for el :in-whatever sequence)
           (for n :from start :by step)
           (collect (cons n (if key
                              (funcall key el)
                              el)))))


;;;; Debugging & Logging ------------------------------------------------------
(defun pr (&rest args)
  "Print `args` readably, separated by spaces and followed by a newline.

  Returns the first argument, so you can just wrap it around a form without
  interfering with the rest of the program.

  This is what `print` should have been.

  "
  (format t "~{~S~^ ~}~%" args)
  (finish-output)
  (first args))

(defmacro prl (&rest args)
  "Print `args` labeled and readably.

  Each argument form will be printed, then evaluated and the result printed.
  One final newline will be printed after everything.

  Returns the last result.

  Examples:

    (let ((i 1)
          (l (list 1 2 3)))
      (prl i (second l)))
    ; =>
    i 1
    (second l) 2

  "
  `(prog1
    (progn ,@(mapcar (lambda (arg) `(pr ',arg ,arg)) args))
    (terpri)
    (finish-output)))


(defun bits (n size &optional (stream t))
  "Print the bits of the `size`-bit two's complement integer `n` to `stream`.

  Examples:

    (bits 5 10)
    => 0000000101

    (bits -5 10)
    => 1111111011

  "
  ;; http://blog.chaitanyagupta.com/2013/10/print-bit-representation-of-signed.html
  (format stream (format nil "~~~D,'0B" size) (ldb (byte size 0) n))
  (values))

(defmacro shut-up (&body body)
  "Run `body` with stdout and stderr redirected to the void."
  `(let ((*standard-output* (make-broadcast-stream))
         (*error-output* (make-broadcast-stream)))
    ,@body))

(defmacro dis (&body body)
  "Disassemble the code generated for a `lambda` with `arglist` and `body`.

  It will also spew compiler notes so you can see why the garbage box isn't
  doing what you think it should be doing.

  "
  (let ((%disassemble #+sbcl 'sb-disassem:disassemble-code-component
                      #-sbcl 'disassemble))
    (destructuring-bind (arglist &body body)
        (iterate (for b :first body :then (cdr b))
                 (while (not (listp (car b))))
                 (finally (return b)))
      `(,%disassemble (compile nil '(lambda ,arglist
                                     (declare (optimize speed))
                                     ,@body))))))

(defmacro comment (&body body)
  "Do nothing with a bunch of forms.

  Handy for block-commenting multiple expressions.

  "
  (declare (ignore body))
  nil)


(defun aesthetic-string (thing)
  "Return the string used to represent `thing` when printing aesthetically."
  (format nil "~A" thing))

(defun structural-string (thing)
  "Return the string used to represent `thing` when printing structurally."
  (format nil "~S" thing))

(defun print-table (rows)
  "Print `rows` as a nicely-formatted table.

  Each row should have the same number of colums.

  Columns will be justified properly to fit the longest item in each one.

  Example:

    (print-table '((1 :red something)
                   (2 :green more)))
    =>
    1 | RED   | SOMETHING
    2 | GREEN | MORE

  "
  (when rows
    (iterate
      (with column-sizes =
            (reduce (curry #'mapcar #'max)
                    (mapcar (curry #'mapcar (compose #'length #'aesthetic-string))
                            rows))) ; lol
      (for row :in rows)
      (format t "~{~vA~^ | ~}~%" (weave column-sizes row))))
  (values))


(defun print-hash-table (hash-table &optional (stream t))
  "Print a pretty representation of `hash-table` to `stream.`

  Respects `*print-length*` when printing the elements.

  "
  (let* ((keys (hash-table-keys hash-table))
         (vals (hash-table-values hash-table))
         (count (hash-table-count hash-table))
         (key-width (-<> keys
                      (mapcar (compose #'length #'prin1-to-string) <>)
                      (reduce #'max <> :initial-value 0)
                      (clamp 0 20 <>))))
    (print-unreadable-object (hash-table stream :type t)
      (princ
        ;; Something shits the bed and output gets jumbled (in SBCL at least) if
        ;; we try to print to `stream` directly in the format statement inside
        ;; `print-unreadable-object`, so instead we can just render to a string
        ;; and `princ` that.
        (format nil ":test ~A :count ~D {~%~{~{  ~vs ~s~}~%~}}"
                (hash-table-test hash-table)
                count
                (loop
                  :with limit = (or *print-length* 40)
                  :for key :in keys
                  :for val :in vals
                  :for i :from 0 :to limit
                  :collect
                  (if (= i limit)
                    (list key-width :too-many-items (list (- count i) :more))
                    (list key-width key val))))
        stream)))
  (terpri stream)
  (values))

(defun pht (hash-table &optional (stream t))
  "Synonym for `print-hash-table` for less typing at the REPL."
  (print-hash-table hash-table stream))

(defun print-hash-table-concisely (hash-table &optional (stream t))
  "Print a concise representation of `hash-table` to `stream.`

  Should respect `*print-length*` when printing the elements.

  "
  (print-unreadable-object (hash-table stream :type t)
    (prin1 (hash-table-test hash-table))
    (write-char #\space stream)
    (prin1 (hash-table-contents hash-table) stream)))

(defmethod print-object ((object hash-table) stream)
  (print-hash-table-concisely object stream))


#+sbcl
(defun dump-profile (filename)
  (with-open-file (*standard-output* filename
                                     :direction :output
                                     :if-exists :supersede)
    (sb-sprof:report :type :graph
                     :sort-by :cumulative-samples
                     :sort-order :ascending)
    (sb-sprof:report :type :flat
                     :min-percent 0.5)))

#+sbcl
(defun start-profiling (&key call-count-packages (mode :cpu))
  "Start profiling performance.  SBCL only.

  `call-count-packages` should be a list of package designators.  Functions in
  these packages will have their call counts recorded via
  `sb-sprof::profile-call-counts`.

  "
  (sb-sprof::reset)
  (-<> call-count-packages
    (mapcar #'mkstr <>)
    (mapcar #'string-upcase <>)
    (mapc #'sb-sprof::profile-call-counts <>))
  (sb-sprof::start-profiling :max-samples 50000
                             :mode mode
                             ; :mode :time
                             :sample-interval 0.01
                             :threads :all))

#+sbcl
(defun stop-profiling (&optional (filename "lisp.prof"))
  "Stop profiling performance and dump a report to `filename`.  SBCL only."
  (sb-sprof::stop-profiling)
  (dump-profile filename))

#+sbcl
(defmacro profile (&body body)
  "Profile `body` and dump the report to `lisp.prof`."
  `(progn
     (start-profiling)
     (unwind-protect
         (time (progn ,@body))
       (stop-profiling))))


(defmacro gimme (n &body body)
  `(iterate (repeat ,n)
     (collect (progn ,@body))))


;;;; CLOS ---------------------------------------------------------------------
(defun build-slot-definition (conc-name slot-spec)
  (destructuring-bind (name &key
                            (type nil type?)
                            (documentation nil documentation?)
                            (initform nil initform?)
                            (accessor (symb conc-name name))
                            (initarg (ensure-keyword name)))
      (ensure-list slot-spec)
    `(,name
      :initarg ,initarg
      :accessor ,accessor
      ,@(when initform? `(:initform ,initform))
      ,@(when type? `(:type ,type))
      ,@(when documentation? `(:documentation ,documentation)))))

(defmacro defclass* (name-and-options direct-superclasses slots &rest options)
  "`defclass` without the tedium.

  This is like `defclass`, but the `:initarg` and `:accessor` slot options will
  automatically be filled in with sane values if they aren't given.

  "
  (destructuring-bind (name &key (conc-name (symb name '-)))
      (ensure-list name-and-options)
    `(defclass ,name ,direct-superclasses
       ,(mapcar (curry #'build-slot-definition conc-name) slots)
       ,@options)))


;;;; Weightlists --------------------------------------------------------------
(defstruct (weightlist (:constructor %make-weightlist))
  weights sums items total)

(defun make-weightlist (items weights)
  "Make a weightlist of the given items and weights.

  Weights can be any `real` numbers.  Weights of zero are fine, as long as at
  least one of the weights is nonzero (otherwise there's nothing to choose).

  "
  (%make-weightlist
    :items items
    :weights weights
    :sums (prefix-sums weights)
    :total (apply #'+ weights)))

(defun weightlist-random (weightlist)
  "Return a random item from the weightlist, taking the weights into account."
  (iterate
    (with n = (random (weightlist-total weightlist)))
    (for item :in (weightlist-items weightlist))
    (for weight :in (weightlist-sums weightlist))
    (finding item :such-that (< n weight))))


;;;; Priority Queues ----------------------------------------------------------
;;; Jankass priority queue implementation.
(defstruct (priority-queue (:conc-name pq-)
                           (:constructor make-priority-queue%))
  (contents nil)
  (predicate #'<)
  (test #'eql))


(defun make-priority-queue (&key (priority-predicate #'<) (element-test #'eql))
  "Create and return a fresh priority queue.

  `priority-predicate` is the comparison function used to compare priorities,
  and should be a `<`-like predicate.

  `element-test` should be the equality predicate for elements.

  "
  (make-priority-queue% :predicate priority-predicate :test element-test))


(defmethod print-object ((object priority-queue) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (prin1 (pq-contents object) stream)))


(defun pqn-priority (node)
  (first node))

(defun pqn-element (node)
  (second node))

(defun pq-resort (pq)
  (zapf (pq-contents pq)
        (sort % (pq-predicate pq) :key #'pqn-priority))
  pq)

(defun pq-lookup (pq element)
  (find element (pq-contents pq)
        :key #'pqn-element
        :test (pq-test pq)))


(defun pq-insert (pq element priority)
  "Insert `element` into `pq` with `priority`.

  Returns `pq` (which has been modified).

  "
  (zapf (pq-contents pq)
        (merge 'list `((,priority ,element)) % (pq-predicate pq)
               :key #'pqn-priority))
  pq)

(defun pq-ensure (pq element priority)
  "Ensure `element` is in `pq` with `priority`.

  If `element` is already in `pq` its priority will be set to `priority`.
  Otherwise it will be inserted as if by calling `pq-insert`.

  Returns `pq` (which may have been modified).

  "
  (let ((existing (pq-lookup pq element)))
    (if existing
      (progn (setf (car existing) priority)
             (pq-resort pq))
      (pq-insert pq element priority)))
  pq)

(defun pq-dequeue (pq)
  "Remove and return the element in `pq` with the lowest-numbered priority.

  If `pq` is empty `nil` will be returned.

  A second value is also returned, which will be `t` if an element was present
  or `nil` if the priority queue was empty.

  "
  (if (pq-contents pq)
    (values (pqn-element (pop (pq-contents pq))) t)
    (values nil nil)))


;;;; Hash Sets ----------------------------------------------------------------
(defstruct (hash-set (:constructor make-hash-set%))
  (storage (error "Required") :type hash-table :read-only t))

(defmethod print-object ((hset hash-set) stream)
  (print-unreadable-object (hset stream :type t :identity t)
    (format stream "~:S" (hset-elements hset))))


(defun make-hash-set (&key (test 'eql) (size 16) (initial-contents '()))
  "Create a fresh hash set.

  `size` should be a hint as to how many elements this set is expected to
  contain.

  `initial-contents` should be a sequence of initial elements for the set
  (duplicates are fine).

  "
  (let* ((result (make-hash-set% :storage (make-hash-table :test test
                                                           :size size))))
    (map nil (curry #'hset-insert! result) initial-contents)
    result))

(defun copy-hash-set (hset)
  "Create a (shallow) copy of the given hash set.

  Only the storage for the hash set itself will be copied -- the elements
  themselves will not be copied.

  "
  (make-hash-set% :storage (copy-hash-table (hash-set-storage hset))))


(defmacro define-hset-op (name arglist &body body)
  (let* ((has-docstring (stringp (first body)))
         (docstring (if has-docstring
                     (first body)
                     ""))
         (body (if has-docstring
                 (rest body)
                 body)))
    `(defun ,name ,arglist
      ,docstring
      (with-slots (storage) ,(first arglist)
       ,@body))))


(define-hset-op hset-empty-p (hset)
  "Return whether `hset` is empty."
  (zerop (hash-table-count storage)))

(define-hset-op hset-contains-p (hset element)
  "Return whether `hset` contains `element`."
  (values (gethash element storage)))

(define-hset-op hset-count (hset)
  "Return the number of elements in `hset`."
  (hash-table-count storage))

(define-hset-op hset-insert! (hset &rest elements)
  "Insert each element in `elements` into `hset`.

  Returns nothing.

  "
  (dolist (element elements)
    (setf (gethash element storage) t))
  (values))

(define-hset-op hset-remove! (hset &rest elements)
  "Remove each element in `elements` from `hset`.

  If an element is not in `hset`, it will be ignored.

  Returns nothing.

  "
  (dolist (element elements)
    (remhash element storage))
  (values))

(define-hset-op hset-pop! (hset)
  "Remove and return an arbitrarily-chosen element from `hset`.

  An error will be signaled if the hash set is empty.

  "
  (assert (not (hset-empty-p hset))
      (hset)
    "Cannot pop from empty hash set ~S"
    hset)
  (iterate (for (k nil) :in-hashtable storage)
           (remhash k storage)
           (return k)))

(define-hset-op hset-clear! (hset)
  "Remove all elements from `hset`.

  Returns nothing.

  "
  (clrhash storage)
  (values))


(define-hset-op hset=% (hset other)
  (iterate (for (k nil) :in-hashtable storage)
           (when (not (hset-contains-p other k))
             (return nil))
           (finally (return t))))

(define-hset-op hset= (hset &rest others)
  "Return whether all the given hash sets contain exactly the same elements.

  All the hash sets are assumed to use the same `test` -- the consequences are
  undefined if this is not the case.

  "
  (if (apply #'not= (hset-count hset) (mapcar #'hset-count others))
    nil
    (iterate (for other :in others)
             (when (not (hset=% hset other))
               (return nil))
             (finally (return t)))))


(define-hset-op hset-union!% (hset other)
  (iterate (for (k nil) :in-hashtable (hash-set-storage other))
           (hset-insert! hset k))
  hset)

(define-hset-op hset-union! (hset &rest others)
  "Destructively update `hset` to contain the union of itself with `others`."
  (reduce #'hset-union!% others :initial-value hset))

(define-hset-op hset-union (hset &rest others)
  "Return a fresh hash set containing the union of the given hash sets."
  (apply #'hset-union! (copy-hash-set hset) others))


(define-hset-op hset-intersection!% (hset other)
  (iterate (for (k nil) :in-hashtable storage)
           (when (not (hset-contains-p other k))
             (remhash k storage)))
  hset)

(define-hset-op hset-intersection! (hset &rest others)
  "Destructively update `hset` to contain the intersection of itself with `others`."
  (reduce #'hset-intersection!% others :initial-value hset))

(define-hset-op hset-intersection (hset &rest others)
  "Return a fresh hash set containing the intersection of the given hash sets."
  (apply #'hset-intersection! (copy-hash-set hset) others))


(define-hset-op hset-difference!% (hset other)
  (iterate (for (k nil) :in-hashtable (hash-set-storage other))
           (remhash k storage))
  hset)

(define-hset-op hset-difference! (hset &rest others)
  "Destructively update `hset` to contain the difference of itself with `others`."
  (reduce #'hset-difference!% others :initial-value hset))

(define-hset-op hset-difference (hset &rest others)
  "Return a fresh hash set containing the difference of the given hash sets."
  (apply #'hset-difference! (copy-hash-set hset) others))


(define-hset-op hset-filter! (hset predicate)
  "Destructively update `hset` to contain only elements satisfying `predicate`."
  (iterate (for (k nil) :in-hashtable storage)
           (when (funcall predicate k)
             (remhash k storage))))

(define-hset-op hset-filter (hset predicate)
  "Return a fresh hash set containing elements of `hset` satisfying `predicate`."
  (let ((new (copy-hash-set hset)))
    (hset-filter! new predicate)
    new))


(define-hset-op hset-map! (hset function &key new-test)
  "Destructively update `hset` by calling `function` on each element.

   If `new-test` is given the hash set's `test` will be updated.

   "
  (let ((results (iterate (for (k nil) :in-hashtable storage)
                          (collect (funcall function k)))))
    (if new-test
      ;; Rebuild the underlying hash table if we have a new test.
      (setf storage (make-hash-table :test new-test
                                     :size (hash-table-count storage)))
      ;; Otherwise just clear and reuse the existing one.
      (clrhash storage))
    (dolist (k results)
      (hset-insert! hset k))
    nil))

(define-hset-op hset-map (hset function &key new-test)
  "Return a fresh hash set containing the results of calling `function` on elements of `hset`.

  If `new-test` is given, the new hash set will use this as its `test`.

  "
  (let ((new (copy-hash-set hset)))
    (hset-map! new function :new-test new-test)
    new))


(define-hset-op hset-elements (hset)
  "Return a fresh list containing the elements of `hset`."
  (hash-table-keys storage))


;;;; Bit Sets -----------------------------------------------------------------
;;; Implementation of the sets-as-integers idea in the Common Lisp Recipes book.
(deftype bset () '(integer 0))


(defun bset-empty ()
  0)

(defun bset-contains-p (bset i)
  (logbitp i bset))

(defun bset-union (s1 s2)
  (logior s1 s2))

(defun bset-intersection (s1 s2)
  (logand s1 s2))

(defun bset-difference (s1 s2)
  (logandc2 s1 s2))

(defun bset-insert (bset i)
  (dpb 1 (byte 1 i) bset))

(defun bset-remove (bset i)
  (dpb 0 (byte 1 i) bset))

(defun bset-count (bset)
  (logcount bset))

(defun bset= (s1 s2)
  (= s1 s2))

(defun bset-empty-p (bset)
  (zerop bset))


(defun make-bset (&rest is)
  (reduce #'bset-insert is :initial-value 0))


(defun bset-to-list (bset)
  (iterate
    (for i :from 0)
    (for s :first bset :then (ash s -1))
    (until (zerop s))
    (when (logbitp 0 s)
      (collect i))))

(defun list-to-bset (list)
  (apply #'make-bset list))


;;;; IO -----------------------------------------------------------------------
(defun read-all-from-string (string)
  "Read all forms from `string` and return them as a fresh list."
  (iterate
    (with done = (gensym))
    (with start = 0)
    (for (values form pos) = (read-from-string string nil done
                                               :start start))
    (while (not (eq form done)))
    (collect form)
    (setf start pos)))

(defun read-all-from-file (path)
  "Read all forms from the file at `path` and return them as a fresh list."
  (with-open-file (file path :direction :input)
    (iterate
      (with done = (gensym))
      (for form = (read file nil done))
      (while (not (eq form done)))
      (collect form))))


;;;; Gnuplot ------------------------------------------------------------------
(defun gnuplot-args% (&rest args)
  (mapcan (lambda (arg) (list "-e" arg))
          (remove nil args)))

(defun gnuplot-args (&key
                     (output :qt)
                     (filename "plot.png")
                     (style :lines)
                     (size-x 1200)
                     (size-y 800)
                     (label-x)
                     (label-y)
                     (line-title 'data)
                     (line-width 4)
                     (smooth nil)
                     (axis-x nil)
                     (axis-y nil)
                     (min-x nil)
                     (max-x nil)
                     (min-y nil)
                     (max-y nil)
                     (tics-x nil)
                     (graph-title)
                     (logscale-x nil)
                     (logscale-y nil)
                     (box-width nil)
                     &allow-other-keys)
  "Return the formatted command line arguments for the given gnuplot arguments.

  You shouldn't call this function directly — it's exposed just so you can see
  the list of possible gnuplot arguments all in one place.

  "
  (flet ((esc (string) (remove #\' (aesthetic-string string)))
         (f (&rest args) (apply #'format nil (substitute "" nil args))))
    (gnuplot-args%
      (ccase output
        ((:x :x11) (f "set terminal x11 persist"))
        (:qt (f "set terminal qt persist"))
        (:png
         (f "set terminal pngcairo dashed size ~D,~D font \"Lucida Grande,20\""
            size-x size-y)
         (f "set output '~A'" (esc filename))))
      (f "set border linewidth 1")
      (f "set style line 10 dashtype 2 linewidth 3 linecolor \"#666666\"")
      (when axis-x (f "set xzeroaxis linestyle 10"))
      (when tics-x (f "set xtics ~A" tics-x))
      (when axis-y (f "set yzeroaxis linestyle 10"))
      (when box-width (f "set boxwidth ~A" box-width))
      (when graph-title (f "set title '~A'" (esc graph-title)))
      (when label-x (f "set xlabel '~A'" (esc label-x)))
      (when label-y (f "set ylabel '~A'" (esc label-y)))
      (when logscale-x (f "set logscale x"))
      (when logscale-y (f "set logscale y"))
      (f "set xrange [~A:~A]" min-x max-x)
      (f "set yrange [~A:~A]" min-y max-y)
      (f "plot '-' using 1:2 title '~A' with ~(~A~) linewidth ~D ~A"
         (esc line-title) style line-width
         (when smooth (f "smooth ~(~A~)" smooth))))))


(defun gnuplot (data
                &rest args
                &key
                (x #'car)
                (y #'cdr)
                (spew-output nil)
                &allow-other-keys)
  "Plot `data` to `filename` with gnuplot.

  This will (silently) quickload the `external-program` system to handle the
  communication with gnuplot.

  `data` should be a sequence of data points to plot.

  `x` should be a function to pull the x-values from each item in data.

  `y` should be a function to pull the y-values from each item in data.

  See the docstring of `gnuplot-args` for other keyword arguments.

  "
  (uiop/package:symbol-call :ql :quickload 'external-program :silent t)
  (let* ((process (uiop/package:symbol-call
                    :external-program :start
                    "gnuplot"
                    (apply #'gnuplot-args args)
                    :input :stream
                    :output (if spew-output *standard-output* nil)))
         (in (uiop/package:symbol-call
               :external-program :process-input-stream
               process)))
    (unwind-protect
        (progn
          (iterate (for item :in-whatever data)
                   (format in "~F ~F~%" (funcall x item) (funcall y item)))
          (finish-output in))
      (close in))
    process))

(defun gnuplot-function (function
                         &rest args
                         &key
                         (start 0.0)
                         (end 1.0)
                         (step 0.1)
                         (include-end nil)
                         &allow-other-keys)
  "Plot `function` over [`start`, `end`) by `step` with gnuplot.

  If `include-end` is `t` the `end` value will also be plotted.

  See the docstring of `gnuplot-args` for other keyword arguments.

  "
  (let* ((x (range start end :step step))
         (x (append x
                    (when (and include-end
                               (not= (car (last x)) end))
                      (list end))))
         (y (mapcar function x))
         (data (mapcar #'cons x y)))
    (apply #'gnuplot data args)))


(defmacro gnuplot-expr (expr &rest args)
  "Plot `expr` (an expression involving `x`) with gnuplot.

  See the docstring of `gnuplot-args` for other keyword arguments.

  "
  `(gnuplot-function (lambda (x) ,expr)
    :line-title ',expr
    ,@args))


(defun gnuplot-histogram (data &key (bin-width 1))
  "Plot `data` as a histogram with gnuplot.

  `bin-width` should be the desired width of the bins.  The bins will be
  centered on multiples of this number, and data will be rounded to the nearest
  bin.

  "
  (-<> data
    (mapcar (lambda (y)
              (* bin-width (round y bin-width)))
            <>)
    frequencies
    hash-table-alist
    (gnuplot <> :style :boxes
             :min-y 0
             :line-width 1
             :box-width (* bin-width 1.0))))


;;;; Licensing ----------------------------------------------------------------
;;; Original code from @dk_jackdaniel:
;;; http://paste.lisp.org/display/327154
(defun license-tree (quicklisp-project-designator)
  (let ((sys (ql-dist:dependency-tree quicklisp-project-designator)))
    (assert (not (null sys)) ()
      "Cannot find Quicklisp project for designator ~S"
      quicklisp-project-designator)
    (shut-up
      (ql:quickload quicklisp-project-designator))
    (map-tree
      (lambda (s)
        (vector (slot-value s 'ql-dist:name)
                (or (asdf:system-license
                      (asdf:find-system
                        (ql-dist:system-file-name s)))
                    "Unspecified")))
      sys)))

(defun license-list (quicklisp-project-designator)
  (remove-duplicates
    (mapcar (rcurry #'coerce 'list)
            (flatten (license-tree quicklisp-project-designator)))
    :key #'car :test #'string=))

(defun print-licenses (quicklisp-project-designator)
  "Print the licenses used by the given project and its dependencies.

  Note that in order to do this the project must be `quickload`ed, so you might
  want to do this in a separate Lisp image if you don't want to clutter your
  current one.

  If the project does not specify its license in its ASDF system definition it
  will be listed as 'Unspecified'.  You should manually figure out what license
  it uses (and maybe send a pull request).

  Example:

    (print-licenses 'fast-io)
    =>
    alexandria           | Public Domain / 0-clause MIT
    babel                | MIT
    cffi                 | MIT
    cffi-grovel          | MIT
    cffi-toolchain       | MIT
    fast-io              | NewBSD
    static-vectors       | MIT
    trivial-features     | MIT
    trivial-gray-streams | MIT
    uiop                 | Unspecified

  "
  (print-table (sort (license-list quicklisp-project-designator)
                     #'string<
                     :key #'car)))


;;;; Eldritch Horrors ---------------------------------------------------------
(defmacro with-flexible-accessors (slot-entries instance-form &rest body)
  (with-gensyms (instance)
    `(let ((,instance ,instance-form))
      (declare (ignorable ,instance))
      (symbol-macrolet
          ,(iterate (for (symbol accessor) :in slot-entries)
                    (collect `(,symbol (,accessor ,instance))))
        ,@body))))

(defmacro define-with-macro (type-and-options &rest slots)
  "Define a with-`type` macro for the given `type` and `slots`.

  This new macro wraps `with-accessors` so you don't have to type `type-`
  a billion times.

  The given `type` must be a symbol naming a struct or class.  It must have the
  appropriate accessors with names exactly of the form `type`-`slot`.

  The defined macro will look something like this:

    (define-with-macro foo a b)
    =>
    (defmacro with-foo ((foo &optional (a-symbol 'a) (b-symbol 'b))
                        &body body)
      `(with-accessors ((,a-symbol foo-a) (,b-symbol foo-b))
           ,foo
         ,@body))

  There's a lot of magic here, but it cuts down on boilerplate for simple things
  quite a lot.

  Example:

    (defstruct foo x y)
    (define-with-macro foo x y)

    (defparameter *f* (make-foo :x 10 :y 20))
    (defparameter *g* (make-foo :x 555 :y 999))

    (with-foo (*f*)
      (with-foo (*g* gx gy)
        (print (list x y gx gy))))
    =>
    (10 20 555 999)

  "
  (destructuring-bind (type &key (conc-name type))
      (ensure-list type-and-options)
    (let* ((accessors (loop :for slot :in slots
                            :collect (symb conc-name '- slot)))
           (symbol-args (loop :for slot :in slots
                              :collect (symb slot '-symbol)))
           (macro-name (symb 'with- type))
           (macro-arglist `((,type &optional
                             ,@(loop :for slot :in slots
                                     :for arg :in symbol-args
                                     :collect `(,arg ',slot)))
                            &body body))
           (accessor-binding-list (loop :for arg :in symbol-args
                                        :for accessor :in accessors
                                        :collect ``(,,arg ,',accessor))))
      `(defmacro ,macro-name ,macro-arglist
        `(with-flexible-accessors ,,`(list ,@accessor-binding-list)
          ,,type
          ,@body)))))

(defmacro eval-dammit (&body body)
  "Just evaluate `body` all the time, jesus christ lisp."
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))

