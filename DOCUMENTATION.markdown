# Documentation for `cl-losh`

This library is my own personal utility belt.

  Everything I write in here is MIT/X11 licensed, so you're free to use it if
  you want.  But I make no guarantees about backwards compatibility -- I might
  change and break things at any time.  Use this at your own risk.
  

  [TOC]

## Package `LOSH`

This package exports all of the symbols in the other packages.

  If you just want to get everything you can `:use` this one and be done with
  it.  Otherwise you can `:use` only the ones you need.

  

## Package `LOSH.ARRAYS`

Utilities related to arrays.

### `DO-ARRAY` (macro)

    (DO-ARRAY (VALUE ARRAY)
      &BODY
      BODY)

Perform `body` once for each element in `array` using `value` for the place.

  `array` can be multidimensional.

  `value` will be `symbol-macrolet`ed to the appropriate `aref`, so you can use
  it as a place if you want.

  Returns the array.

  Example:

    (let ((arr (vector 1 2 3)))
      (do-array (x arr)
        (setf x (1+ x))))
    => #(2 3 4)

  

### `FILL-MULTIDIMENSIONAL-ARRAY` (function)

    (FILL-MULTIDIMENSIONAL-ARRAY ARRAY ITEM)

Fill `array` with `item`.

  Unlike `fill`, this works on multidimensional arrays.  It won't cons on SBCL,
  but it may in other implementations.

  

### `FILL-MULTIDIMENSIONAL-ARRAY-FIXNUM` (function)

    (FILL-MULTIDIMENSIONAL-ARRAY-FIXNUM ARRAY ITEM)

Fill `array` (which must be of type `(array FIXNUM *)`) with `item`.

  Unlike `fill`, this works on multidimensional arrays.  It won't cons on SBCL,
  but it may in other implementations.

  

### `FILL-MULTIDIMENSIONAL-ARRAY-SINGLE-FLOAT` (function)

    (FILL-MULTIDIMENSIONAL-ARRAY-SINGLE-FLOAT ARRAY ITEM)

Fill `array` (which must be of type `(array SINGLE-FLOAT *)`) with `item`.

  Unlike `fill`, this works on multidimensional arrays.  It won't cons on SBCL,
  but it may in other implementations.

  

### `FILL-MULTIDIMENSIONAL-ARRAY-T` (function)

    (FILL-MULTIDIMENSIONAL-ARRAY-T ARRAY ITEM)

Fill `array` (which must be of type `(array T *)`) with `item`.

  Unlike `fill`, this works on multidimensional arrays.  It won't cons on SBCL,
  but it may in other implementations.

  

## Package `LOSH.CONTROL-FLOW`

Utilities for managing control flow.

### `GATHERING` (macro)

    (GATHERING
      &BODY
      BODY)

Run `body` to gather some things and return them.

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

  

### `IF-FOUND` (macro)

    (IF-FOUND VAR LOOKUP-EXPR THEN ELSE)

Perform `then` or `else` depending on the results of `lookup-expr`.

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

  

### `RECURSIVELY` (macro)

    (RECURSIVELY BINDINGS
      &BODY
      BODY)

Execute body recursively, like Clojure's `loop`/`recur`.

  `bindings` should contain a list of symbols and (optional) starting values.

  In `body` the symbol `recur` will be bound to the function for recurring.

  Example:

      (defun length (some-list)
        (recursively ((list some-list) (n 0))
          (if (null list)
            n
            (recur (cdr list) (1+ n)))))

  

### `WHEN-FOUND` (macro)

    (WHEN-FOUND VAR
        LOOKUP-EXPR
      &BODY
      BODY)

Perform `body` with `var` to the results of `lookup-expr`, when valid.

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

  

## Package `LOSH.DEBUGGING`

Utilities for figuring out what the hell is going on.

### `BITS` (function)

    (BITS N SIZE)

### `DIS` (macro)

    (DIS ARGLIST
      &BODY
      BODY)

Disassemble the code generated for a `lambda` with `arglist` and `body`.

  It will also spew compiler notes so you can see why the garbage box isn't
  doing what you think it should be doing.

  

### `PR` (function)

    (PR &REST ARGS)

## Package `LOSH.DISTRIBUTIONS`

Utilities for calculating statistical... things.

### `FREQUENCIES` (function)

    (FREQUENCIES SEQ &KEY (TEST 'EQL))

Return a hash table containing the feqeuencies of the items in `seq`.

  Uses `test` for the `:test` of the hash table.

  Example:

    (frequencies '(foo foo bar))
    => {foo 2
        bar 1}

  

### `PREFIX-SUMS` (function)

    (PREFIX-SUMS LIST)

Return a list of the prefix sums of the numbers in `list`.

  Example:

    (prefix-sums '(10 10 10 0 1))
    => (10 20 30 30 31)

  

## Package `LOSH.ELDRITCH-HORRORS`

Abandon all hope, ye who enter here.

### `DEFINE-WITH-MACRO` (macro)

    (DEFINE-WITH-MACRO TYPE &REST SLOTS)

Define a with-`type` macro for the given `type` and `slots`.

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

  

### `DLAMBDA` (macro)

    (DLAMBDA &REST CLAUSES)

## Package `LOSH.FUNCTIONS`

Utilities for working with higher-order functions.

### `JUXT` (function)

    (JUXT &REST FNS)

Return a function that will juxtipose the results of `functions`.

  This is like Clojure's `juxt`.  Given functions `(f0 f1 ... fn)`, this will
  return a new function which, when called with some arguments, will return
  `(list (f0 ...args...) (f1 ...args...) ... (fn ...args...))`.

  Example:

    (funcall (juxt #'list #'+ #'- #'*) 1 2)
    => ((1 2) 3 -1 2)

  

### `NULLARY` (function)

    (NULLARY FUNCTION &OPTIONAL RESULT)

Return a new function that acts as a nullary-patched version of `function`.

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

  

## Package `LOSH.HASH-SETS`

A simple hash set implementation.

### `HASH-SET` (class)

#### Slot `DATA`

* Allocation: `:INSTANCE`
* Initarg: `:DATA`

### `MAKE-SET` (function)

    (MAKE-SET &KEY (TEST #'EQL) (INITIAL-DATA NIL))

### `SET-ADD` (function)

    (SET-ADD SET VALUE)

### `SET-ADD-ALL` (function)

    (SET-ADD-ALL SET SEQ)

### `SET-CLEAR` (function)

    (SET-CLEAR SET)

### `SET-CONTAINS-P` (function)

    (SET-CONTAINS-P SET VALUE)

### `SET-EMPTY-P` (function)

    (SET-EMPTY-P SET)

### `SET-POP` (function)

    (SET-POP SET)

### `SET-RANDOM` (function)

    (SET-RANDOM SET)

### `SET-REMOVE` (function)

    (SET-REMOVE SET VALUE)

### `SET-REMOVE-ALL` (function)

    (SET-REMOVE-ALL SET SEQ)

## Package `LOSH.ITERATE`

Custom `iterate` drivers and clauses.

## Package `LOSH.LISTS`

Utilities related to lists.

### `TAKE` (function)

    (TAKE N LIST)

Return a fresh list of the first `n` elements of `list`.

  If `list` is shorter than `n` a shorter result will be returned.

  Example:

    (take 2 '(a b c))
    => (a b)

    (take 4 '(1))
    => (1)

  

## Package `LOSH.MATH`

Utilities related to math and numbers.

### `CLAMP` (function)

    (CLAMP FROM TO VALUE)

Clamp `value` between `from` and `to`.

### `DIVIDESP` (function)

    (DIVIDESP N DIVISOR)

Return whether `n` is evenly divisible by `divisor`.

### `LERP` (function)

    (LERP FROM TO N)

Lerp together `from` and `to` by factor `n`.

  Note that you might want `precise-lerp` instead.

  

### `MAP-RANGE` (function)

    (MAP-RANGE SOURCE-FROM SOURCE-TO DEST-FROM DEST-TO SOURCE-VAL)

Map `source-val` from the source range to the destination range.

  Example:

    ;          source    dest        value
    (map-range 0.0 1.0   10.0 20.0   0.2)
    => 12.0

  

### `NORM` (function)

    (NORM MIN MAX VAL)

Normalize `val` to a number between `0` and `1` (maybe).

  If `val` is between `max` and `min`, the result will be a number between `0`
  and `1`.

  If `val` lies outside of the range, it'll be still be scaled and will end up
  outside the 0/1 range.

  

### `PRECISE-LERP` (function)

    (PRECISE-LERP FROM TO N)

Lerp together `from` and `to` by factor `n`, precisely.

  Vanilla lerp does not guarantee `(lerp from to 0.0)` will return exactly
  `from` due to floating-point errors.  This version will return exactly `from`
  when given a `n` of `0.0`, at the cost of an extra multiplication.

  

### `SQUARE` (function)

    (SQUARE X)

### `TAU` (variable)

## Package `LOSH.MUTATION`

Utilities for mutating places in-place.

### `CALLF` (macro)

    (CALLF &REST PLACE-FUNCTION-PAIRS)

Set each `place` to the result of calling `function` on its current value.

  Examples:

    (let ((x 10) (y 20))
      (callf x #'1-
             y #'1+)
      (list x y))
    =>
    (9 21)
  

### `CLAMPF` (macro)

    (CLAMPF PLACE FROM TO)

Clamp `place` between `from` and `to` in-place.

### `DIVF` (macro)

    (DIVF PLACE &OPTIONAL DIVISOR)

Divide `place` by `divisor` in-place.

  If `divisor` is not given, `place` will be set to `(/ 1 place)`.

  

### `MODF` (macro)

    (MODF PLACE DIVISOR)

Modulo `place` by `divisor` in-place.

### `MULF` (macro)

    (MULF PLACE FACTOR)

Multiply `place` by `factor` in-place.

### `NEGATEF` (macro)

    (NEGATEF PLACE)

Negate the value of `place`.

### `NOTF` (macro)

    (NOTF PLACE)

Set `place` to `(not place)` in-place.

### `REMAINDERF` (macro)

    (REMAINDERF PLACE DIVISOR)

Remainder `place` by `divisor` in-place.

### `ZAPF` (macro)

    (ZAPF &REST PLACE-EXPR-PAIRS)

Update each `place` by evaluating `expr` with `%` bound to the current value.

  `zapf` works like `setf`, but when evaluating the value expressions the symbol
  `%` will be bound to the current value of the place.

  Examples:

    (zapf foo (1+ %)
          (car bar) (if (> % 10) :a :b))

  

## Package `LOSH.QUEUES`

A simple queue implementation.

### `DEQUEUE` (function)

    (DEQUEUE QUEUE)

Dequeue an item from `queue` and return it.

### `ENQUEUE` (function)

    (ENQUEUE ITEM QUEUE)

Enqueue `item` in `queue`, returning the new size of the queue.

### `MAKE-QUEUE` (function)

    (MAKE-QUEUE)

Allocate and return a fresh queue.

### `QUEUE` (struct)

Slots: `CONTENTS`, `LAST`, `SIZE`

### `QUEUE-APPEND` (function)

    (QUEUE-APPEND QUEUE LIST)

Enqueue each element of `list` in `queue` and return the queue's final size.

### `QUEUE-CONTENTS` (function)

    (QUEUE-CONTENTS VALUE INSTANCE)

### `QUEUE-EMPTY-P` (function)

    (QUEUE-EMPTY-P QUEUE)

Return whether `queue` is empty.

### `QUEUE-SIZE` (function)

    (QUEUE-SIZE VALUE INSTANCE)

## Package `LOSH.RANDOM`

Utilities related to randomness.

### `D` (function)

    (D N SIDES &OPTIONAL (PLUS 0))

Roll some dice.

  Examples:

    (d 1 4)     ; rolls 1d4
    (d 2 8)     ; rolls 2d8
    (d 1 10 -1) ; rolls 1d10-1

  

### `RANDOM-AROUND` (function)

    (RANDOM-AROUND VALUE SPREAD)

Return a random number within `spread` of `value`.

### `RANDOM-ELT` (function)

    (RANDOM-ELT SEQ)

Return a random element of `seq`, and whether one was available.

  This will NOT be efficient for lists.

  Examples:

    (random-elt #(1 2 3))
    => 1
       T

    (random-elt nil)
    => nil
       nil

  

### `RANDOM-GAUSSIAN` (function)

    (RANDOM-GAUSSIAN &OPTIONAL (MEAN 0.0) (STANDARD-DEVIATION 1.0))

Return a random float from a gaussian distribution.  NOT THREAD-SAFE (yet)!

### `RANDOM-GAUSSIAN-INTEGER` (function)

    (RANDOM-GAUSSIAN-INTEGER &OPTIONAL (MEAN 0) (STANDARD-DEVIATION 1))

Return a random integer from a gaussian distribution.  NOT THREAD-SAFE (yet)!

### `RANDOM-RANGE` (function)

    (RANDOM-RANGE MIN MAX)

Return a random number between [`min`, `max`).

### `RANDOM-RANGE-EXCLUSIVE` (function)

    (RANDOM-RANGE-EXCLUSIVE MIN MAX)

Return a random number between (`min`, `max`).

### `RANDOMP` (function)

    (RANDOMP &OPTIONAL (CHANCE 0.5))

Return a random boolean with `chance` probability of `t`.

## Package `LOSH.WEIGHTLISTS`

A simple data structure for choosing random items with weighted probabilities.

### `MAKE-WEIGHTLIST` (function)

    (MAKE-WEIGHTLIST ITEMS WEIGHTS)

Make a weightlist of the given items and weights.
  
  Weights can be any `real` numbers.  Weights of zero are fine, as long as at
  least one of the weights is nonzero (otherwise there's nothing to choose).

  

### `WEIGHTLIST` (struct)

Slots: `WEIGHTS`, `SUMS`, `ITEMS`, `TOTAL`

### `WEIGHTLIST-ITEMS` (function)

    (WEIGHTLIST-ITEMS VALUE INSTANCE)

### `WEIGHTLIST-RANDOM` (function)

    (WEIGHTLIST-RANDOM WEIGHTLIST)

Return a random item from the weightlist, taking the weights into account.

### `WEIGHTLIST-WEIGHTS` (function)

    (WEIGHTLIST-WEIGHTS VALUE INSTANCE)

