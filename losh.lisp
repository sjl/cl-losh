(in-package #:losh)

;;;; Chili Dogs
(defmacro defun-inlineable (name &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,@body)
     (declaim (notinline ,name))
     ',name))

(defmacro defun-inline (name &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,@body)
     ',name))


;;;; Math
(defconstant tau (coerce (* pi 2) 'single-float)) ; fuck a pi


(defun-inlineable square (x)
  (* x x))

(defun dividesp (n divisor)
  "Return whether `n` is evenly divisible by `divisor`."
  (zerop (mod n divisor)))


(defun norm (min max val)
  "Normalize `val` to a number between `0` and `1` (maybe).

  If `val` is between `max` and `min`, the result will be a number between `0`
  and `1`.

  If `val` lies outside of the range, it'll be still be scaled and will end up
  outside the 0/1 range.

  "
  (/ (- val min)
     (- max min)))

(defun lerp (from to n)
  "Lerp together `from` and `to` by factor `n`.

  Note that you might want `precise-lerp` instead.

  "
  (+ from
     (* n (- to from))))

(defun precise-lerp (from to n)
  "Lerp together `from` and `to` by factor `n`, precisely.

  Vanilla lerp does not guarantee `(lerp from to 0.0)` will return exactly
  `from` due to floating-point errors.  This version will return exactly `from`
  when given a `n` of `0.0`, at the cost of an extra multiplication.

  "
  (+ (* (- 1 n) from)
     (* n to)))

(defun map-range (source-from source-to dest-from dest-to source-val)
  "Map `source-val` from the source range to the destination range.

  Example:

    ;          source    dest        value
    (map-range 0.0 1.0   10.0 20.0   0.2)
    => 12.0

  "
  (lerp dest-from dest-to
        (norm source-from source-to source-val)))

(defun clamp (from to value)
  "Clamp `value` between `from` and `to`."
  (let ((max (max from to))
        (min (min from to)))
    (cond
      ((> value max) max)
      ((< value min) min)
      (t value))))


;;;; Random
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
  "Return a random number between [`min`, `max`)."
  (+ min (random (- max min))))

(defun-inlineable random-range-exclusive (min max)
  "Return a random number between (`min`, `max`)."
  (+ 1 min (random (- max min 1))))

(defun random-around (value spread)
  "Return a random number within `spread` of `value`."
  (etypecase spread
    (integer (random-range (- value spread)
                           (+ value spread 1)))
    (real (random-range (- value spread)
                        (+ value spread)))))


(let (spare)
  (defun random-gaussian (&optional (mean 0.0) (standard-deviation 1.0))
    "Return a random float from a gaussian distribution.  NOT THREAD-SAFE (yet)!"
    ;; https://en.wikipedia.org/wiki/Marsaglia_polar_method
    (declare (optimize (speed 3))
             (inline square random-range))
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
  (round (random-gaussian mean standard-deviation)))


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


;;;; Functions
(defun juxt (&rest fns)
  "Return a function that will juxtipose the results of `functions`.

  This is like Clojure's `juxt`.  Given functions `(f0 f1 ... fn)`, this will
  return a new function which, when called with some arguments, will return
  `(list (f0 ...args...) (f1 ...args...) ... (fn ...args...))`.

  Example:

    (funcall (juxt #'list #'+ #'- #'*) 1 2)
    => ((1 2) 3 -1 2)

  "
  (lambda (&rest args)
    (mapcar (rcurry #'apply args) fns)))

(defun nullary (function &optional result)
  "Return a new function that acts as a nullary-patched version of `function`.

  The new function will return `result` when called with zero arguments, and
  delegate to `function` otherwise.

  Examples:

    (max 1 10 2)                     => 10
    (max)                            => invalid number of arguments

    (funcall (nullary #'max))          => nil
    (funcall (nullary #'max 0))        => 0
    (funcall (nullary #'max 0) 1 10 2) => 10

    (reduce #'max nil)                  => invalid number of arguments
    (reduce (nullary #'max) nil)        => nil
    (reduce (nullary #'max :empty) nil) => :empty
    (reduce (nullary #'max) '(1 10 2))  => 10

  "
  (lambda (&rest args)
    (if (null args) result (apply function args))))


;;;; Control Flow
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
  "Perform `body` with `var` to the results of `lookup-expr`, when valid.

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
  "Run `body` to gather some things and return them.

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


;;;; Mutation
(defun build-zap (place expr env)
  (multiple-value-bind (temps exprs stores store-expr access-expr)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temps exprs)
            (,(car stores) (symbol-macrolet ((% ,access-expr))
                             ,expr)))
      ,store-expr)))

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
    ,@(loop :for (place expr . rest) :on place-expr-pairs :by #'cddr
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
     ,@(loop :for (place function . rest) :on place-function-pairs :by #'cddr
             :collect `(%callf ,place ,function))))


;;;; Lists
(defun take (n list)
  "Return a fresh list of the first `n` elements of `list`.

  If `list` is shorter than `n` a shorter result will be returned.

  Example:

    (take 2 '(a b c))
    => (a b)

    (take 4 '(1))
    => (1)

  "
  (iterate (repeat n)
           (for item :in list)
           (collect item)))


;;;; Arrays
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


;;;; Queues
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


;;;; Iterate
(defmacro-driver (FOR var PAIRS-OF-LIST list)
  "Iterate over the all pairs of the (including (last . first)).

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

           (t
            (prog1
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
  (with-gensyms (count)
    (let ((average (or var iterate::*result-var*)))
      `(progn
        (for ,count :from 0)
        (for ,average
             :first ,expr
             :then (/ (+ (* ,average ,count)
                         ,expr)
                      (1+ ,count)))))))

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
  (let ((timing-function (ecase time-type
                           ((real-time) #'get-internal-real-time)
                           ((run-time) #'get-internal-run-time)))
        (since (or since-var (when (null per-var)
                               iterate::*result-var*))))
    (with-gensyms (start-time current-time previous-time)
      `(progn
        (with ,start-time = (funcall ,timing-function))
        (for ,current-time = (funcall ,timing-function))
        (for ,previous-time :previous ,current-time :initially ,start-time)
        ,(when since
           `(for ,since = (- ,current-time ,start-time)))
        ,(when per-var
           `(for ,per-var = (- ,current-time ,previous-time)))))))


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
    (with-gensyms (s seq len idx)
      `(progn
        (with ,len = nil)
        (with ,idx = nil)
        (generate ,seq :in (iterate (for ,s :in-sequence ,seqs)
                                    (unless (emptyp ,s)
                                      (collect ,s))))
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
  (destructuring-bind (var &rest index-vars)
      (ensure-list binding-form)
    (with-gensyms (%i i arr dims floors)
      (let ((kwd (if generate 'generate 'for)))
        `(progn
          (with ,arr = ,array)
          ,@(when (some #'identity index-vars)
              `((with ,dims = (coerce (array-dimensions ,arr) 'vector))
                (with ,floors = (calculate-array-floors ,arr))))
          ,@(iterate (for index :in index-vars)
                     (when index (collect `(with ,index = 0))))
          (generate ,%i :from 0 :below (array-total-size ,arr))
          (,kwd ,var next (progn
                            (let ((,i (next ,%i)))
                              ,@(iterate
                                  (for index :in index-vars)
                                  (for n :from 0)
                                  (when index
                                    (collect
                                      `(setf ,index (mod (floor ,i (svref ,floors ,n))
                                                         (svref ,dims ,n))))))
                              (row-major-aref ,arr ,i)))))))))


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
    1
    0.5
    0.0
    BEEP
    1
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
          (with ,%start = ,start)
          (with ,%end = ,end)
          (with ,%increment = ,increment)
          (with ,%counter)
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
      (generate-nested ,(iterate (for var :in delta-vars)
                                 (collect `(,var :from ,-r :to ,r)))
                       :control-var ,control)
      (next ,control)
      (when (and ,skip
                 ,@(iterate (for var :in delta-vars)
                            (collect `(zerop ,var))))
        (next ,control)))))


;;;; Distributions
(defun prefix-sums (list)
  "Return a list of the prefix sums of the numbers in `list`.

  Example:

    (prefix-sums '(10 10 10 0 1))
    => (10 20 30 30 31)

  "
  (iterate
    (for i :in list)
    (sum i :into s)
    (collect s)))

(defun frequencies (seq &key (test 'eql))
  "Return a hash table containing the feqeuencies of the items in `seq`.

  Uses `test` for the `:test` of the hash table.

  Example:

    (frequencies '(foo foo bar))
    => {foo 2
        bar 1}

  "
  (iterate
    (with result = (make-hash-table :test test))
    (for i :in-whatever seq)
    (incf (gethash i result 0))
    (finally (return result))))


;;;; Hash Sets
(defclass hash-set ()
  ((data :initarg :data)))


(defun make-set (&key (test #'eql) (initial-data nil))
  (let ((set (make-instance 'hash-set
                            :data (make-hash-table :test test))))
    (mapcar (curry #'set-add set) initial-data)
    set))


(defun set-contains-p (set value)
  (nth-value 1 (gethash value (slot-value set 'data))))

(defun set-empty-p (set)
  (zerop (hash-table-count (slot-value set 'data))))

(defun set-add (set value)
  (setf (gethash value (slot-value set 'data)) t)
  value)

(defun set-add-all (set seq)
  (map nil (curry #'set-add set) seq))

(defun set-remove (set value)
  (remhash value (slot-value set 'data))
  value)

(defun set-remove-all (set seq)
  (map nil (curry #'set-remove set) seq))

(defun set-clear (set)
  (clrhash (slot-value set 'data))
  set)

(defun set-random (set)
  (if (set-empty-p set)
    (values nil nil)
    (loop :with data = (slot-value set 'data)
          :with target = (random (hash-table-count data))
          :for i :from 0
          :for k :being :the :hash-keys :of data
          :when (= i target)
          :do (return (values k t)))))

(defun set-pop (set)
  (multiple-value-bind (val found) (set-random set)
    (if found
      (progn
        (set-remove set val)
        (values val t))
      (values nil nil))))


(defmethod print-object ((set hash-set) stream)
  (print-unreadable-object (set stream :type t)
    (format stream "~{~S~^ ~}"
            (iterate (for (key nil) :in-hashtable (slot-value set 'data))
                     (collect key)))))


;;;; Debugging & Logging
(defun pr (&rest args)
  "Print `args` readably, separated by spaces and followed by a newline.

  This is what `print` should have been.

  "
  (format t "~{~S~^ ~}~%" args)
  (finish-output)
  (values))

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

(defmacro dis (arglist &body body)
  "Disassemble the code generated for a `lambda` with `arglist` and `body`.

  It will also spew compiler notes so you can see why the garbage box isn't
  doing what you think it should be doing.

  "
  (let ((%disassemble #+sbcl 'sb-disassem:disassemble-code-component
                      #-sbcl 'disassemble))
    `(,%disassemble (compile nil '(lambda ,arglist
                                   (declare (optimize speed))
                                   ,@body)))))


;;;; Weightlists
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


;;;; Eldritch Horrors
(defmacro dlambda (&rest clauses)
  ;;; From Let Over Lambda.
  (with-gensyms (message arguments)
    (flet ((parse-clause (clause)
             (destructuring-bind (key arglist &rest body)
                 clause
               `(,key (apply (lambda ,arglist ,@body) ,arguments)))))
      `(lambda (,message &rest ,arguments)
        (ecase ,message
          ,@(mapcar #'parse-clause clauses))))))

(defmacro define-with-macro (type &rest slots)
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
  (let* ((accessors (loop :for slot :in slots
                          :collect (symb type '- slot)))
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
      `(with-accessors ,,`(list ,@accessor-binding-list)
          ,,type
        ,@body))))

