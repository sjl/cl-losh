# Documentation for `cl-losh`

This library is my own personal utility belt.

  Everything I write in here is MIT licensed, so you're free to use it if
  you want.  But I make no guarantees about backwards compatibility -- I might
  change and break things at any time.  Use this at your own risk.
  

  [TOC]

## Package `LOSH`

This package exports all of the symbols in the other packages.

  If you just want to get everything you can `:use` this one and be done with
  it.  Otherwise you can `:use` only the ones you need.

  

## Package `LOSH.ASTAR`

A★ search in a handy package.

### `ASTAR` (function)

    (ASTAR &KEY START NEIGHBORS GOALP COST HEURISTIC TEST LIMIT GET-SEEN SET-SEEN)

Search for a path from `start` to a goal using A★.

  The following parameters are all required:

  * `start`: the starting state.

  * `neighbors`: a function that takes a state and returns all states reachable
    from it.

  * `goalp`: a predicate that takes a state and returns whether it is a goal.

  * `cost`: a function that takes two states `a` and `b` and returns the cost
    to move from `a` to `b`.

  * `heuristic`: a function that takes a state and estimates the distance
    remaining to the goal.

  * `test`: an equality predicate for comparing nodes.  It must be suitable for
    passing to `make-hash-table`.

  If the heuristic function is admissable (i.e. it never overestimates the
  remaining distance) the algorithm will find the shortest path.  If you don't
  have a decent heuristic, just use `(constantly 0)` to degrade to Dijkstra.

  Note that `test` is required.  The only sensible default would be `eql`, but
  if you were using states that need a different predicate and forgot to pass it
  the algorithm would end up blowing the heap, which is unpleasant.

  The following parameters are optional:

  * `limit`: a maximum cost.  Any paths that exceed this cost will not be
    considered.

  * `set-seen`: a function that takes a state and a cost, and records it.
    If not provided a hash table will be used, but sometimes (depending on what
    your states are) it can be faster to store visited nodes more efficiently.

  * `get-seen`: a function that takes a state and retrieves the stored cost, or
    `nil` if the state has not been seen.

  

## Package `LOSH.ARRAYS`

Utilities related to arrays.

### `BISECT-LEFT` (function)

    (BISECT-LEFT PREDICATE VECTOR TARGET &KEY (KEY #'IDENTITY) (START 0) (END (LENGTH VECTOR)))

Bisect `vector` with `predicate` and return the LEFT element.

  Only the subsequence of `vector` bounded by `start` and `end` is considered.

  `vector` must be sorted (with `predicate`) before this function is called
  (this is not checked).

  You can think of this function as partitioning the elements into two halves:
  those that satisfy `(predicate (funcall key element) target)` and those that
  don't, and then selecting the element on the LEFT side of the split:

      satisfying  not statisfying
    #(..........  ...............)
               ^
               |
          result

  Two values will be returned: the element and its index.  If no element
  satisfies the predicate `nil` will be returned for both values.

  Examples:

    ;                     index
    ;                  0 1 2 3 4 5                                val  index
    (bisect-left '<  #(1 3 5 7 7 9) 5)                     ; =>     3, 1
    (bisect-left '<= #(1 3 5 7 7 9) 5)                     ; =>     5, 2
    (bisect-left '<= #(1 3 5 7 7 9) 7)                     ; =>     7, 4
    (bisect-left '<  #(1 3 5 7 7 9) 1)                     ; =>   nil, nil
    (bisect-left '>  #(9 8 8 8 1 0) 5)                     ; =>     8, 3
    (bisect-left '<  #((1) (2 2) (3 3 3)) 2 :key #'length) ; =>   (1), 0
    (bisect-left '<= #((1) (2 2) (3 3 3)) 2 :key #'length) ; => (2 2), 1

  

### `BISECT-RIGHT` (function)

    (BISECT-RIGHT PREDICATE VECTOR TARGET &KEY (KEY #'IDENTITY) (START 0) (END (LENGTH VECTOR)))

Bisect `vector` with `predicate` and return the RIGHT element.

  Only the subsequence of `vector` bounded by `start` and `end` is considered.

  `vector` must be sorted (with `predicate`) before this function is called
  (this is not checked).

  You can think of this function as partitioning the elements into two halves:
  those that satisfy `(predicate (funcall key element) target)` and those that
  don't, and then selecting the element on the RIGHT side of the split:

      satisfying  not statisfying
    #(..........  ...............)
                  ^
                  |
                  result

  Two values will be returned: the element and its index.  If every element
  satisfies the predicate `nil` will be returned for both values.

  Examples:

    ;                       index
    ;                   0 1 2 3 4 5                                  val  index
    (bisect-right '<  #(1 3 5 7 7 9) 5)                     ; =>       5, 2
    (bisect-right '<= #(1 3 5 7 7 9) 5)                     ; =>       7, 3
    (bisect-right '<= #(1 3 5 7 7 9) 7)                     ; =>       9, 5
    (bisect-right '<  #(1 3 5 7 7 9) 10)                    ; =>     nil, nil
    (bisect-right '>  #(9 8 8 8 1 0) 5)                     ; =>       1, 4
    (bisect-right '<  #((1) (2 2) (3 3 3)) 2 :key #'length) ; =>   (2 2), 1
    (bisect-right '<= #((1) (2 2) (3 3 3)) 2 :key #'length) ; => (3 3 3), 2

  

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

  

### `VECTOR-LAST` (function)

    (VECTOR-LAST VECTOR)

Return the last element of `vector`, or `nil` if it is empty.

  A second value is returned, which will be `t` if the vector was not empty and
  `nil` if it was.

  The vector's fill-pointer will be respected.

  

## Package `LOSH.BASE`

A few utilities re-exported from Alexandria, plus some other basic stuff.

### `MKSTR` (function)

    (MKSTR &REST ARGS)

### `SYMB` (function)

    (SYMB &REST ARGS)

### `TIMING` (macro)

    (TIMING (&KEY (TIME :RUN) (RESULT-TYPE 'INTEGER))
      &BODY
      BODY)

Execute `body`, discard its result, and return the time taken.

  `time` must be one of `:run` or `:real`.

  `result-type` must be `integer` (which will return internal time units) or
  `rational`/`single-float`/`double-float` (which will return seconds).

  

## Package `LOSH.BITS`

Utilities for low-level bit stuff.

### `+/16` (function)

    (+/16 X Y)

Perform 16-bit signed addition of `x` and `y`.

  Returns two values: the result and a boolean specifying whether
  underflow/overflow occurred.

  

### `+/32` (function)

    (+/32 X Y)

Perform 32-bit signed addition of `x` and `y`.

  Returns two values: the result and a boolean specifying whether
  underflow/overflow occurred.

  

### `+/64` (function)

    (+/64 X Y)

Perform 64-bit signed addition of `x` and `y`.

  Returns two values: the result and a boolean specifying whether
  underflow/overflow occurred.

  

### `+/8` (function)

    (+/8 X Y)

Perform 8-bit signed addition of `x` and `y`.

  Returns two values: the result and a boolean specifying whether
  underflow/overflow occurred.

  

### `-/16` (function)

    (-/16 X Y)

Perform 16-bit signed subtraction of `x` and `y`.

  Returns two values: the result and a boolean specifying whether
  underflow/overflow occurred.

  

### `-/32` (function)

    (-/32 X Y)

Perform 32-bit signed subtraction of `x` and `y`.

  Returns two values: the result and a boolean specifying whether
  underflow/overflow occurred.

  

### `-/64` (function)

    (-/64 X Y)

Perform 64-bit signed subtraction of `x` and `y`.

  Returns two values: the result and a boolean specifying whether
  underflow/overflow occurred.

  

### `-/8` (function)

    (-/8 X Y)

Perform 8-bit signed subtraction of `x` and `y`.

  Returns two values: the result and a boolean specifying whether
  underflow/overflow occurred.

  

## Package `LOSH.CHILI-DOGS`

Gotta go FAST.

### `DEFUN-INLINE` (macro)

    (DEFUN-INLINE NAME
        ARGS
      &BODY
      BODY)

Like `defun`, but declaims `name` to be `inline`.

### `DEFUN-INLINEABLE` (macro)

    (DEFUN-INLINEABLE NAME
      &BODY
      BODY)

Like `defun-inline`, but declaims `name` to be `notinline` afterword.

  This is useful when you don't want to inline a function everywhere, but *do*
  want to have the ability to inline it on demand with (declare (inline ...)).

  

## Package `LOSH.CLOS`

Utilities for working with CLOS.

### `DEFCLASS*` (macro)

    (DEFCLASS* NAME-AND-OPTIONS DIRECT-SUPERCLASSES SLOTS &REST OPTIONS)

`defclass` without the tedium.

  This is like `defclass`, but the `:initarg` and `:accessor` slot options will
  automatically be filled in with sane values if they aren't given.

  `name-and-options` can be a symbol or a list, which will be destructured
  against `(name &key conc-name)`.

  

### `DEFINE-CONDITION*` (macro)

    (DEFINE-CONDITION* NAME-AND-OPTIONS DIRECT-SUPERCLASSES SLOTS &REST OPTIONS)

`define-condition` without the tedium.

  This is like `define-condition`, but the `:initarg` and `:accessor` slot
  options will automatically be filled in with sane values if they aren't given.

  `name-and-options` can be a symbol or a list, which will be destructured
  against `(name &key conc-name)`.

  

### `ENSURE-SLOT-VALUE` (macro)

    (ENSURE-SLOT-VALUE OBJECT SLOT &OPTIONAL DEFAULT)

Return the `slot-value` of `slot` in `object`, setting it to `default` if unbound.

### `SLOT-VALUE-OR` (function)

    (SLOT-VALUE-OR OBJECT SLOT &OPTIONAL DEFAULT)

Return the `slot-value` of `slot` in `object`, or `default` if unbound.

## Package `LOSH.CONTROL-FLOW`

Utilities for managing control flow.

### `_` (macro)

    (_ EXPR &REST FORMS)

Thread the given forms, with `_` as a placeholder.

### `DO-FILE` (macro)

    (DO-FILE (SYMBOL PATH &REST OPEN-OPTIONS &KEY (READER '#'READ-LINE) &ALLOW-OTHER-KEYS)
      &BODY
      BODY)

Iterate over the contents of `file` using `reader`.

    During iteration, `symbol` will be set to successive values read from the
    file by `reader`.

    `reader` can be any function that conforms to the usual reading interface,
    i.e. anything that can handle `(read-foo stream eof-error-p eof-value)`.

    Any keyword arguments other than `:reader` will be passed along to `open`.

    If `nil` is used for one of the `:if-…` options to `open` and this results
    in `open` returning `nil`, no iteration will take place.

    An implicit block named `nil` surrounds the iteration, so `return` can be
    used to terminate early.

    Returns `nil`.

    Examples:

      (do-file (line "foo.txt")
        (print line))

      (do-file (form "foo.lisp" :reader #'read :external-format :EBCDIC-US)
        (when (eq form :stop)
          (return :stopped-early))
        (print form))

      (do-file (line "does-not-exist.txt" :if-does-not-exist nil)
        (this-will-not-be-executed))

    

### `DO-IRANGE` (macro)

    (DO-IRANGE RANGES
      &BODY
      BODY)

Perform `body` on the given inclusive `ranges`.

  Each range in `ranges` should be of the form `(variable from to &optional by)`.
  During iteration `body` will be executed with `variable` bound to successive
  values according to `by` in the range [`from`, `to`].

  `from` can be larger than `to`, in which case the values will be stepped down
  instead of up.

  If multiple ranges are given they will be iterated in a nested fashion.

  Example:

    (do-irange ((x  0  4  2)
                (y 11 10))
      (pr x y))
    ; =>
    ; 0 11
    ; 0 10
    ; 2 11
    ; 2 10
    ; 4 11
    ; 4 10

  

### `DO-RANGE` (macro)

    (DO-RANGE RANGES
      &BODY
      BODY)

Perform `body` on the given `ranges`.

  Each range in `ranges` should be of the form `(variable from below)`.  During
  iteration `body` will be executed with `variable` bound to successive values
  in the range [`from`, `below`).

  `from` can be larger than `below`, in which case the values will be stepped
  down instead of up.

  If multiple ranges are given they will be iterated in a nested fashion.

  Example:

    (do-range ((x  0  6  2)
               (y 12 10))
      (pr x y))
    ; =>
    ; 0 12
    ; 0 11
    ; 2 12
    ; 2 11
    ; 4 12
    ; 4 11

  

### `DO-REPEAT` (macro)

    (DO-REPEAT N
      &BODY
      BODY)

Perform `body` `n` times.

### `DO-VECTOR` (macro)

    (DO-VECTOR (VAR-OR-VARS VECTOR &KEY (START NIL START?) (END NIL END?))
      &BODY
      BODY)

Iterate over `vector`, performing `body` with `var-or-vars` bound.

  `var-or-vars` can be one of the following:

  * `value-symbol`
  * `(value-symbol)`
  * `(index-symbol value-symbol)`

  Successive elements of `vector` will be bound to `value-symbol` while `body`
  is executed.  If `index-symbol` is given, the current index will be bound to
  it.

  Returns `nil`.

  

### `GATHERING` (macro)

    (GATHERING
      &BODY
      BODY)

Run `body` to gather some things and return a fresh list of them.

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

  

### `GATHERING-VECTOR` (macro)

    (GATHERING-VECTOR (&KEY (SIZE 16) (ELEMENT-TYPE T))
      &BODY
      BODY)

Run `body` to gather some things and return a fresh vector of them.

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

  

### `IF-FOUND` (macro)

    (IF-FOUND (VAR LOOKUP-EXPR) THEN ELSE)

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

    (if-found (val (gethash :foo hash))
      'yes
      'no)

  

### `IF-LET` (macro)

    (IF-LET BINDINGS
      &BODY
      BODY)

Bind `bindings` in parallel and execute `then` if all are true, or `else` otherwise.

  `body` must be of the form `(...optional-declarations... then else)`.

  This macro combines `if` and `let`.  It takes a list of bindings and binds
  them like `let` before executing the `then` branch of `body`, but if any
  binding's value evaluates to `nil` the process stops there and the `else`
  branch is immediately executed (with no bindings in effect).

  If any `optional-declarations` are included they will only be in effect for
  the `then` branch.

  Examples:

    (if-let ((a (progn (print :a) 1))
             (b (progn (print :b) 2))
             (c (progn (print :c) 3)))
      (list a b c)
      'nope)
    ; =>
    :A
    :B
    :C
    (1 2 3)

    (if-let ((a (progn (print :a) 1))
             (b (progn (print :b) nil))
             (c (progn (print :c) 3)))
      (list a b c)
      'nope)
    ; =>
    :A
    :B
    NOPE

  

### `IF-LET*` (macro)

    (IF-LET* BINDINGS
      &BODY
      BODY)

Bind `bindings` sequentially and execute `then` if all are true, or `else` otherwise.

  `body` must be of the form `(...optional-declarations... then else)`.

  This macro combines `if` and `let*`.  It takes a list of bindings and binds
  them like `let*` before executing the `then` branch of `body`, but if any
  binding's value evaluate to `nil` the process stops there and the `else`
  branch is immediately executed (with no bindings in effect).

  If any `optional-declarations` are included they will only be in effect for
  the `then` branch.

  Examples:

    (if-let* ((a (progn (print :a) 1))
              (b (progn (print :b) 2))
              (c (progn (print :c) 3)))
      (list a b c)
      'nope)
    ; =>
    :A
    :B
    :C
    (1 2 3)

    (if-let* ((a (progn (print :a) 1))
              (b (progn (print :b) nil))
              (c (progn (print :c) 3)))
      (list a b c)
      'nope)
    ; =>
    :A
    :B
    NOPE

  

### `MULTIPLE-VALUE-BIND*` (macro)

    (MULTIPLE-VALUE-BIND* BINDINGS
      &BODY
      BODY)

Bind each pair in `bindings` with `multiple-value-bind` sequentially.

  Example:

    (multiple-value-bind*
        (((a b) (values 0 1))
         ((c) (values (1+ b)))
      (list a b c))
    ; =>
    ; (0 1 2)

  From https://github.com/phoe/m-m-v-b

  

### `NEST` (macro)

    (NEST &REST FORMS)

Thread the given forms, putting each as the body of the previous.

  Example:

    (nest (multiple-value-bind (a b c) (foo))
          (when (and a b c))
          (multiple-value-bind (d e f) (bar))
          (when (and d e f))
          (do-something))

  macroexpands to:

    (multiple-value-bind (a b c) (foo)
      (when (and a b c)
        (multiple-value-bind (d e f) (bar)
          (when (and d e f)
            (do-something)))))

  

### `RECURSIVELY` (macro)

    (RECURSIVELY BINDINGS
      &BODY
      BODY)

Execute `body` recursively, like Clojure's `loop`/`recur`.

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

  

### `WHEN-FOUND` (macro)

    (WHEN-FOUND (VAR LOOKUP-EXPR)
      &BODY
      BODY)

Perform `body` with `var` bound to the result of `lookup-expr`, when valid.

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

    (when-found (val (gethash :foo hash))
      body)

  

### `WHEN-LET` (macro)

    (WHEN-LET BINDINGS
      &BODY
      BODY)

Bind `bindings` in parallel and execute `body`, short-circuiting on `nil`.

  This macro combines `when` and `let`.  It takes a list of bindings and binds
  them like `let` before executing `body`, but if any binding's value evaluates
  to `nil` the process stops there and `nil` is immediately returned.

  Examples:

    (when-let ((a (progn (print :a) 1))
               (b (progn (print :b) 2))
               (c (progn (print :c) 3)))
      (list a b c))
    ; =>
    :A
    :B
    :C
    (1 2 3)

    (when-let ((a (progn (print :a) 1))
               (b (progn (print :b) nil))
               (c (progn (print :c) 3)))
      (list a b c))
    ; =>
    :A
    :B
    NIL

  

### `WHEN-LET*` (macro)

    (WHEN-LET* BINDINGS
      &BODY
      BODY)

Bind `bindings` sequentially and execute `body`, short-circuiting on `nil`.

  This macro combines `when` and `let*`.  It takes a list of bindings and binds
  them like `let` before executing `body`, but if any binding's value evaluates
  to `nil` the process stops there and `nil` is immediately returned.

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

  

## Package `LOSH.DEBUGGING`

Utilities for figuring out what the hell is going on.

### `AESTHETIC-STRING` (function)

    (AESTHETIC-STRING THING)

Return the string used to represent `thing` when printing aesthetically.

### `BITS` (function)

    (BITS &OPTIONAL (N *) (SIZE 8) (STREAM T))

Print the bits of the `size`-bit two's complement integer `n` to `stream`.

  Examples:

    (bits 5 10)
    => 0000000101

    (bits -5 10)
    => 1111111011

  

### `COMMENT` (macro)

    (COMMENT
      &BODY
      BODY)

Do nothing with a bunch of forms.

  Handy for block-commenting multiple expressions.

  

### `DIS` (macro)

    (DIS
      &BODY
      BODY)

Disassemble the code generated for a `lambda` with `arglist` and `body`.

  It will also spew compiler notes so you can see why the garbage box isn't
  doing what you think it should be doing.

  

### `GIMME` (macro)

    (GIMME N
      &BODY
      BODY)

### `HEX` (function)

    (HEX &OPTIONAL (N *) (SIZE 8) (STREAM T))

Print the hex of the `size`-bit unsigned byte `n` to `stream`.

  Examples:

    (hex 255)
    => FF

  

### `PHR` (function)

    (PHR)

Print a horizontal rule to aid in visual debugging.

### `PR` (function)

    (PR &REST ARGS)

Print `args` readably, separated by spaces and followed by a newline.

  Returns the first argument, so you can just wrap it around a form without
  interfering with the rest of the program.

  This is what `print` should have been.

  

### `PRETTY-PRINT-HASH-TABLE` (function)

    (PRETTY-PRINT-HASH-TABLE *STANDARD-OUTPUT* HT)

### `PRINT-TABLE` (function)

    (PRINT-TABLE ROWS)

Print `rows` as a nicely-formatted table.

  Each row should have the same number of colums.

  Columns will be justified properly to fit the longest item in each one.

  Example:

    (print-table '((1 :red something)
                   (2 :green more)))
    =>
    1 | RED   | SOMETHING
    2 | GREEN | MORE

  

### `PRL` (macro)

    (PRL &REST ARGS)

Print `args` labeled and readably.

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

  

### `PROFILE` (macro)

    (PROFILE FORM &KEY (MODE :CPU))

Profile `form` and dump the report to `lisp.prof`.

### `PROFILE-WHEN` (macro)

    (PROFILE-WHEN CONDITION
      &BODY
      BODY)

Evaluate and return `body`, profiling when `condition` is true.

### `SHUT-UP` (macro)

    (SHUT-UP
      &BODY
      BODY)

Run `body` with stdout and stderr redirected to the void.

### `START-PROFILING` (function)

    (START-PROFILING &KEY CALL-COUNT-PACKAGES (MODE :CPU))

Start profiling performance.  SBCL only.

  `call-count-packages` should be a list of package designators.  Functions in
  these packages will have their call counts recorded via
  `sb-sprof::profile-call-counts`.

  

### `STOP-PROFILING` (function)

    (STOP-PROFILING &OPTIONAL (FILENAME lisp.prof))

Stop profiling performance and dump a report to `filename`.  SBCL only.

### `STRUCTURAL-STRING` (function)

    (STRUCTURAL-STRING THING)

Return the string used to represent `thing` when printing structurally.

## Package `LOSH.ELDRITCH-HORRORS`

Abandon all hope, ye who enter here.

### `DEFINE-WITH-MACRO` (macro)

    (DEFINE-WITH-MACRO TYPE-AND-OPTIONS &REST SLOTS)

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

  

### `EVAL-DAMMIT` (macro)

    (EVAL-DAMMIT
      &BODY
      BODY)

Just evaluate `body` all the time, jesus christ lisp.

### `SCRATCH` (macro)

    (SCRATCH
      &BODY
      FORMS)

Evaluate `forms` in an imperative fashion.

  Each expression in `forms` will be evaluated, with the following exceptions:

  * A bare symbol will be bound via (nested) `let` to the next expression.
  * `:mv` will bind the next expression (which must be a list of symbols) to the
    expression after it with `multiple-value-bind`.
  * `:db` will bind the next expression (which must be a valid binding) to the
    expression after it with `destructuring-bind`.

  Example:

      (scratch
        a 10
        b 20
        c (+ a b)
        :mv (d e)   (truncate 100 23)
        :db (f (g)) (list 100 (list 22))
        (+ a (- b c) d e (* f g)))

  

## Package `LOSH.FUNCTIONS`

Utilities for working with higher-order functions.

### `FIXED-POINT` (function)

    (FIXED-POINT FUNCTION DATA &KEY (TEST 'EQL) (LIMIT NIL))

Find a fixed point of `function`, starting with `data`.

  Successive runs of `function` will be compared with `test`.  Once `test`
  returns true the last result will be returned.

  `limit` can be an integer to limit the maximum number of iterations performed.

  A second value is also returned: `t` if a fixed point was found or `nil` if
  the iteration limit was reached.

  

### `JUXT` (function)

    (JUXT &REST FUNCTIONS)

Return a function that will juxtapose the results of `functions`.

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

    (max 1 10 2) ; => 10
    (max)        ; => invalid number of arguments

    (funcall (nullary #'max))          ; => nil
    (funcall (nullary #'max 0))        ; => 0
    (funcall (nullary #'max 0) 1 10 2) ; => 10

    (reduce #'max nil)                  ; => invalid number of arguments
    (reduce (nullary #'max) nil)        ; => nil
    (reduce (nullary #'max :empty) nil) ; => :empty
    (reduce (nullary #'max) '(1 10 2))  ; => 10

  

## Package `LOSH.GNUPLOT`

Utilities for plotting data with gnuplot.

### `GNUPLOT` (function)

    (GNUPLOT DATA COMMANDS)

Graph `data` with gnuplot using `commands`.

  `data` must be an alist of `(identifier . data)` pairs.

  Each `identifier` must be a string of the form `$foo`.  Each `data` must be
  one of the following: a sequence of sequences of data points, an alist of data
  points, or a 2D array of data points.

  `commands` must be a string or a sequence of strings.

  Example:

    (gnuplot `(("$data" . ,foo-data)) "
      @xrfc3339
      set terminal qt
      plot $data using 1:2 with linespoints
      pause mouse close
      ")

  

### `GNUPLOT-COMMAND` (function)

    (GNUPLOT-COMMAND COMMAND &AUX (S (PROCESS-INPUT-STREAM *GNUPLOT-PROCESS*)))

Send the string `command` to the currently-running gnuplot process.

  Must be called from inside `with-gnuplot`.

  

### `GNUPLOT-DATA` (function)

    (GNUPLOT-DATA IDENTIFIER DATA &AUX (S (PROCESS-INPUT-STREAM *GNUPLOT-PROCESS*)))

Bind `identifier` to `data` inside the currently-running gnuplot process.

  `identifier` must be a string of the form `$foo`.

  `data` must be one of the following: a sequence of sequences of data points,
  an alist of data points, or a 2D array of data points.

  Must be called from inside `with-gnuplot`.

  

### `GNUPLOT-FORMAT` (function)

    (GNUPLOT-FORMAT FORMAT-STRING &REST ARGS &AUX (S (PROCESS-INPUT-STREAM *GNUPLOT-PROCESS*)))

Send a `cl:format`ed string to the currently-running gnuplot process.

  Must be called from inside `with-gnuplot`.

  

### `PLOT` (function)

    (PLOT DATA &KEY (STYLE :LINESPOINTS) (FILE plot.pdf) (LOGSCALE NIL))

Plot `data` with gnuplot.

  Convenience wrapper around the gnuplot functions.  This is only intended for
  REPL-driven experimentation — if you want any customization you should use the
  gnuplot interface instead.

  

### `WITH-GNUPLOT` (macro)

    (WITH-GNUPLOT OPTIONS
      &BODY
      BODY)

## Package `LOSH.HASH-SETS`

Simple hash set implementation.

### `COPY-HASH-SET` (function)

    (COPY-HASH-SET HSET)

Create a (shallow) copy of the given hash set.

  Only the storage for the hash set itself will be copied -- the elements
  themselves will not be copied.

  

### `DO-HASH-SET` (macro)

    (DO-HASH-SET (SYMBOL HSET)
      &BODY
      BODY)

Iterate over `hset` with `symbol` bound to successive elements.

### `HASH-SET` (struct)

Slots: `STORAGE`

### `HSET-CLEAR!` (function)

    (HSET-CLEAR! HSET)

Remove all elements from `hset`.

  Returns nothing.

  

### `HSET-CONTAINS-P` (function)

    (HSET-CONTAINS-P HSET ELEMENT)

Return whether `hset` contains `element`.

### `HSET-COUNT` (function)

    (HSET-COUNT HSET)

Return the number of elements in `hset`.

### `HSET-DIFFERENCE` (function)

    (HSET-DIFFERENCE HSET &REST OTHERS)

Return a fresh hash set containing the difference of the given hash sets.

### `HSET-DIFFERENCE!` (function)

    (HSET-DIFFERENCE! HSET &REST OTHERS)

Destructively update `hset` to contain the difference of itself with `others`.

### `HSET-ELEMENTS` (function)

    (HSET-ELEMENTS HSET)

Return a fresh list containing the elements of `hset`.

### `HSET-EMPTY-P` (function)

    (HSET-EMPTY-P HSET)

Return whether `hset` is empty.

### `HSET-FILTER` (function)

    (HSET-FILTER HSET PREDICATE)

Return a fresh hash set containing elements of `hset` satisfying `predicate`.

### `HSET-FILTER!` (function)

    (HSET-FILTER! HSET PREDICATE)

Destructively update `hset` to contain only elements satisfying `predicate`.

### `HSET-INSERT!` (function)

    (HSET-INSERT! HSET &REST ELEMENTS)

Insert each element in `elements` into `hset`.

  Returns nothing.

  

### `HSET-INTERSECTION` (function)

    (HSET-INTERSECTION HSET &REST OTHERS)

Return a fresh hash set containing the intersection of the given hash sets.

### `HSET-INTERSECTION!` (function)

    (HSET-INTERSECTION! HSET &REST OTHERS)

Destructively update `hset` to contain the intersection of itself with `others`.

### `HSET-MAP` (function)

    (HSET-MAP HSET FUNCTION &KEY NEW-TEST)

Return a fresh hash set containing the results of calling `function` on elements of `hset`.

  If `new-test` is given, the new hash set will use this as its `test`.

  

### `HSET-MAP!` (function)

    (HSET-MAP! HSET FUNCTION &KEY NEW-TEST)

Destructively update `hset` by calling `function` on each element.

   If `new-test` is given the hash set's `test` will be updated.

   

### `HSET-POP!` (function)

    (HSET-POP! HSET)

Remove and return an arbitrarily-chosen element from `hset`.

  An error will be signaled if the hash set is empty.

  

### `HSET-REDUCE` (function)

    (HSET-REDUCE HSET FUNCTION &KEY (INITIAL-VALUE NIL IVP))

Reduce `function` over the elements of `hset`.

  The order in which the elements are processed is undefined.

  

### `HSET-REMOVE!` (function)

    (HSET-REMOVE! HSET &REST ELEMENTS)

Remove each element in `elements` from `hset`.

  If an element is not in `hset`, it will be ignored.

  Returns nothing.

  

### `HSET-UNION` (function)

    (HSET-UNION HSET &REST OTHERS)

Return a fresh hash set containing the union of the given hash sets.

### `HSET-UNION!` (function)

    (HSET-UNION! HSET &REST OTHERS)

Destructively update `hset` to contain the union of itself with `others`.

### `HSET=` (function)

    (HSET= HSET &REST OTHERS)

Return whether all the given hash sets contain exactly the same elements.

  All the hash sets are assumed to use the same `test` -- the consequences are
  undefined if this is not the case.

  

### `MAKE-HASH-SET` (function)

    (MAKE-HASH-SET &KEY (TEST 'EQL) (SIZE 16) (INITIAL-CONTENTS 'NIL))

Create a fresh hash set.

  `size` should be a hint as to how many elements this set is expected to
  contain.

  `initial-contents` should be a sequence of initial elements for the set
  (duplicates are fine).

  

## Package `LOSH.HASH-TABLES`

Utilities for operating on hash tables.

### `HASH-TABLE-CONTENTS` (function)

    (HASH-TABLE-CONTENTS HASH-TABLE)

Return a fresh list of `(key value)` elements of `hash-table`.

### `MUTATE-HASH-VALUES` (function)

    (MUTATE-HASH-VALUES FUNCTION HASH-TABLE)

Replace each value in `hash-table` with the result of calling `function` on it.

  Returns the hash table.

  

### `REMHASH-IF` (function)

    (REMHASH-IF TEST HASH-TABLE)

Remove elements which satisfy `(test key value)` from `hash-table`.

  Returns the hash table.

### `REMHASH-IF-KEY` (function)

    (REMHASH-IF-KEY TEST HASH-TABLE)

Remove elements which satisfy `(test key)` from `hash-table`.

  Returns the hash table.

### `REMHASH-IF-NOT` (function)

    (REMHASH-IF-NOT TEST HASH-TABLE)

Remove elements which don't satisfy `(test key value)` from `hash-table`.

  Returns the hash table.

### `REMHASH-IF-NOT-KEY` (function)

    (REMHASH-IF-NOT-KEY TEST HASH-TABLE)

Remove elements which satisfy don't `(test key)` from `hash-table`.

  Returns the hash table.

### `REMHASH-IF-NOT-VALUE` (function)

    (REMHASH-IF-NOT-VALUE TEST HASH-TABLE)

Remove elements which satisfy don't `(test value)` from `hash-table`.

  Returns the hash table.

### `REMHASH-IF-VALUE` (function)

    (REMHASH-IF-VALUE TEST HASH-TABLE)

Remove elements which satisfy `(test value)` from `hash-table`.

  Returns the hash table.

## Package `LOSH.IO`

Utilities for input/output/reading/etc.

### `READ-ALL` (function)

    (READ-ALL STREAM)

Read all forms from `stream` and return them as a fresh list.

### `READ-ALL-FROM-FILE` (function)

    (READ-ALL-FROM-FILE PATH)

Read all forms from the file at `path` and return them as a fresh list.

### `READ-ALL-FROM-STRING` (function)

    (READ-ALL-FROM-STRING STRING)

Read all forms from `string` and return them as a fresh list.

## Package `LOSH.ITERATE`

Custom `iterate` drivers and clauses.

### `IF-FIRST-ITERATION` (macro)

    (IF-FIRST-ITERATION THEN ELSE)

Evaluate `then` if this clause is executed on the first iteration, otherwise `else`.

  This is similar to from iterate's built-in `if-first-time`, but slightly different:

  * `if-first-time` evaluates `then` the first time the clause is evaluated,
    even if that happens on a subsequent iteration.
  * `if-first-iteration` evaluates `then` only if the clause is evaluated on
    the first iteration.

  Example:

    (iterate
      (for i :from 1 :to 4)
      (collect (cons i (when (evenp i)
                         (list
                           (if-first-time :first-time :later-time)
                           (if-first-iteration :first-iter :later-iter))))))
    ; =>
    ; ((1)
    ;  (2 :FIRST-TIME :LATER-ITER)
    ;  (3)
    ;  (4 :LATER-TIME :LATER-ITER))

  in that it will only evaluate `then` on the first iteration of the loop, 

### `MACROEXPAND-ITERATE` (function)

    (MACROEXPAND-ITERATE CLAUSE)

Macroexpand the given iterate clause/driver.

  Example:

    (macroexpand-iterate '(averaging (+ x 10) :into avg))
    =>
    (PROGN
     (FOR #:COUNT630 :FROM 1)
     (SUM (+ X 10) :INTO #:TOTAL631)
     (FOR AVG = (/ #:TOTAL631 #:COUNT630)))

  

### `RETURNING` (macro)

    (RETURNING &REST VALUES)

Return `values` from the iterate clause.

  Equivalent to `(finally (return (values ...)))`.

  

### `UNLESS-FIRST-ITERATION` (macro)

    (UNLESS-FIRST-ITERATION EXPR)

Sugar for `(if-first-iteration nil expr)`.

### `UNLESS-FIRST-TIME` (macro)

    (UNLESS-FIRST-TIME EXPR)

Sugar for `(if-first-time nil expr)`.

### `WHEN-FIRST-ITERATION` (macro)

    (WHEN-FIRST-ITERATION EXPR)

Sugar for `(if-first-iteration expr nil)`.

### `WHEN-FIRST-TIME` (macro)

    (WHEN-FIRST-TIME EXPR)

Sugar for `(if-first-time expr nil)`.

### `WITH-RESULT` (macro)

    (WITH-RESULT SYMBOL = EXPR)

Bind `expr` to symbol using `with`, and return it at the end.

  Equivalent to `(progn (with symbol = expr) (returning expr))`.

  

## Package `LOSH.LISTS`

Utilities for operating on lists.

### `0..` (function)

    (0.. BELOW)

Return a fresh list of the range `[0, below)`.

### `0...` (function)

    (0... TO)

Return a fresh list of the range `[0, to]`.

### `1..` (function)

    (1.. BELOW)

Return a fresh list of the range `[1, below)`.

### `1...` (function)

    (1... TO)

Return a fresh list of the range `[1, to]`.

### `ASSOCAR` (function)

    (ASSOCAR ITEM ALIST &REST ARGS)

Return the `car` of `(apply #'assoc item alist args)`.

### `ASSOCDR` (function)

    (ASSOCDR ITEM ALIST &REST ARGS)

Return the `cdr` of `(apply #'assoc item alist args)`.

### `IRANGE` (function)

    (IRANGE START END &KEY (STEP 1))

Return a fresh list of the range `[start, end]` by `step`.

  `end` can be smaller than `start`, in which case the numbers will be stepped
  down instead of up.

  `step` must always be a positive value, regardless of the direction of the
  range.

  

### `RANGE` (function)

    (RANGE START END &KEY (STEP 1))

Return a fresh list of the range `[start, end)` by `step`.

  `end` can be smaller than `start`, in which case the numbers will be stepped
  down instead of up.

  `step` must always be a positive value, regardless of the direction of the
  range.

  

### `RASSOCAR` (function)

    (RASSOCAR ITEM ALIST &REST ARGS)

Return the `car` of `(apply #'rassoc item alist args)`.

### `RASSOCDR` (function)

    (RASSOCDR ITEM ALIST &REST ARGS)

Return the `cdr` of `(apply #'rassoc item alist args)`.

### `SOMELIST` (function)

    (SOMELIST PREDICATE LIST &REST MORE-LISTS)

Call `predicate` on successive sublists of `list`, returning the first true result.

  `somelist` is to `some` as `maplist` is to `mapcar`.

  

## Package `LOSH.MATH`

Utilities related to math and numbers.

### `1/2TAU` (variable)

### `1/4TAU` (variable)

### `1/8TAU` (variable)

### `2/4TAU` (variable)

### `2/8TAU` (variable)

### `3/4TAU` (variable)

### `3/8TAU` (variable)

### `4/8TAU` (variable)

### `5/8TAU` (variable)

### `6/8TAU` (variable)

### `7/8TAU` (variable)

### `CLAMP` (function)

    (CLAMP FROM TO VALUE)

Clamp `value` between `from` and `to`.

### `DEGREES` (function)

    (DEGREES RADIANS)

Convert `radians` into degrees.

  The result will be the same type as `tau` and `pi`.

  

### `DIGIT` (function)

    (DIGIT POSITION INTEGER &OPTIONAL (BASE 10))

Return the value of the digit at `position` in `integer`.

  Examples:

    (digit 0 135) ; => 5
    (digit 1 135) ; => 3
    (digit 2 135) ; => 1

    (digit 0 #xD4 16) ; => 4
    (digit 1 #xD4 16) ; => 13

  

### `DIVIDESP` (function)

    (DIVIDESP N DIVISOR)

Return whether `n` is evenly divisible by `divisor`.

  The value returned will be the quotient when true, `nil` otherwise.

  

### `IN-RANGE-P` (function)

    (IN-RANGE-P LOW VALUE HIGH)

Return whether `low` <= `value` < `high`.

### `LERP` (function)

    (LERP FROM TO N)

Lerp together `from` and `to` by factor `n`.

  You might want `precise-lerp` instead.

  

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

  

### `RADIANS` (function)

    (RADIANS DEGREES)

Convert `degrees` into radians.

  The result will be the same type as `tau` and `pi`.

  

### `SQUARE` (function)

    (SQUARE X)

### `TAU` (variable)

### `TAU/2` (variable)

### `TAU/4` (variable)

### `TAU/8` (variable)

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

  

### `ENSUREF` (macro)

    (ENSUREF &REST PLACE-EXPR-PAIRS)

Set each `place` that is currently `NIL` to its corresponding `expr`.

  Syntactic sugar where `(ensuref place expr)` expands to something like
  `(or place (setf place expr))` but doesn't multiply-evaluate the place.

  Examples:

    (defparameter *foo* nil)
    *foo* ; => NIL

    (ensuref *foo* (print 'hello)) ; prints HELLO
    *foo* ; => HELLO

    (ensuref *foo* (print 'world))
    *foo* ; => HELLO

  

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

### `TRUNCATEF` (macro)

    (TRUNCATEF PLACE DIVISOR)

Truncate `place` by `divisor` in-place.

### `ZAPF` (macro)

    (ZAPF &REST PLACE-EXPR-PAIRS)

Update each `place` by evaluating `expr` with `%` bound to the current value.

  `zapf` works like `setf`, but when evaluating the value expressions the symbol
  `%` will be bound to the current value of the place.

  Examples:

    (zapf foo (1+ %)
          (car bar) (if (> % 10) :a :b))

  

## Package `LOSH.PRIORITY-QUEUES`

Jankass priority queue implementation.

### `MAKE-PRIORITY-QUEUE` (function)

    (MAKE-PRIORITY-QUEUE &KEY (PRIORITY-PREDICATE #'<) (ELEMENT-TEST #'EQL))

Create and return a fresh priority queue.

  `priority-predicate` is the comparison function used to compare priorities,
  and should be a `<`-like predicate.

  `element-test` should be the equality predicate for elements.

  

### `PQ-DEQUEUE` (function)

    (PQ-DEQUEUE PQ)

Remove and return the element in `pq` with the lowest-numbered priority.

  If `pq` is empty `nil` will be returned.

  A second value is also returned, which will be `t` if an element was present
  or `nil` if the priority queue was empty.

  

### `PQ-ENSURE` (function)

    (PQ-ENSURE PQ ELEMENT PRIORITY)

Ensure `element` is in `pq` with `priority`.

  If `element` is already in `pq` its priority will be set to `priority`.
  Otherwise it will be inserted as if by calling `pq-insert`.

  Returns `pq` (which may have been modified).

  

### `PQ-INSERT` (function)

    (PQ-INSERT PQ ELEMENT PRIORITY)

Insert `element` into `pq` with `priority`.

  Returns `pq` (which has been modified).

  

### `PRIORITY-QUEUE` (struct)

Slots: `CONTENTS`, `PREDICATE`, `TEST`

## Package `LOSH.QUEUES`

A simple queue implementation.

### `DEQUEUE` (function)

    (DEQUEUE QUEUE)

Dequeue an item from `queue` and return it.

### `ENQUEUE` (function)

    (ENQUEUE ITEM QUEUE)

Enqueue `item` in `queue`, returning the new size of the queue.

### `MAKE-QUEUE` (function)

    (MAKE-QUEUE &KEY INITIAL-CONTENTS)

Allocate and return a fresh queue.

### `QUEUE` (struct)

Slots: `CONTENTS`, `LAST`, `SIZE`

### `QUEUE-APPEND` (function)

    (QUEUE-APPEND QUEUE LIST)

Enqueue each element of `list` in `queue` and return the queue's final size.

### `QUEUE-EMPTY-P` (function)

    (QUEUE-EMPTY-P QUEUE)

Return whether `queue` is empty.

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

    (RANDOM-AROUND VALUE SPREAD &OPTIONAL (GENERATOR #'RANDOM))

Return a random number within `spread` of `value` (inclusive).

### `RANDOM-ELT` (function)

    (RANDOM-ELT SEQ &OPTIONAL (GENERATOR #'RANDOM))

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

    (RANDOM-GAUSSIAN MEAN STANDARD-DEVIATION &OPTIONAL (GENERATOR #'RANDOM))

Return a random float from a gaussian distribution.  NOT THREAD-SAFE (yet)!

### `RANDOM-GAUSSIAN-INTEGER` (function)

    (RANDOM-GAUSSIAN-INTEGER MEAN STANDARD-DEVIATION &OPTIONAL (GENERATOR #'RANDOM))

Return a random integer from a gaussian distribution.  NOT THREAD-SAFE (yet)!

### `RANDOM-RANGE` (function)

    (RANDOM-RANGE MIN MAX &OPTIONAL (GENERATOR #'RANDOM))

Return a random number in [`min`, `max`).

### `RANDOM-RANGE-EXCLUSIVE` (function)

    (RANDOM-RANGE-EXCLUSIVE MIN MAX &OPTIONAL (GENERATOR #'RANDOM))

Return a random number in (`min`, `max`).

### `RANDOM-RANGE-INCLUSIVE` (function)

    (RANDOM-RANGE-INCLUSIVE MIN MAX &OPTIONAL (GENERATOR #'RANDOM))

Return a random number in [`min`, `max`].

### `RANDOMP` (function)

    (RANDOMP &OPTIONAL (CHANCE 0.5) (GENERATOR #'RANDOM))

Return a random boolean with `chance` probability of `t`.

## Package `LOSH.READTABLE`

Custom readtable.

## Package `LOSH.REGEX`

Utilities related to regular expressions.

### `RECASE` (macro)

    (RECASE (TARGET-STRING &OPTIONAL)
      &BODY
      CLAUSES)

Match a target string against regexes, also binding variables.

  Each clause is of the form:

    (condition &rest body)

  Where `condition` is a list (or just the regex if no variables are required):

    (regex &rest ppcre-var-list)

  The target string will be matched against `regex` and `ppcre-var-list` bound
  with `ppcre:register-groups-bind`.  If it matches, `body` will be executed and
  its value returned, otherwise execution continues to later clauses.

  A final condition of `t` can be used as a fallback.

  Declarations are supported.

  Example:

    (recase (string)
      (("([0-9]{4})-([0-9]{2})-([0-9]{2})" (#'parse-integer year month day))
       (declare (ignore month day))
       (format t "~S was a good year for PLs." year))
      (("([A-Z][a-z]+) ([0-9]{1,2}), ([0-9]{4})" month (#'parse-integer day year))
       (declare (ignore year day))
       (format t "~A was a good month for Lisp." month))
      (t "Programming is hard."))

  

## Package `LOSH.RING-BUFFERS`

Simple ring buffer implementation.

### `DO-RING-BUFFER` (macro)

    (DO-RING-BUFFER (SYMBOL RING-BUFFER)
      &BODY
      BODY)

Iterate over `ring-buffer`, executing `body` with `symbol` bound to each element.

  Elements are walked oldest to newest.

  

### `MAKE-RING-BUFFER` (function)

    (MAKE-RING-BUFFER &KEY (SIZE 64))

Create and return a fresh ring buffer able to hold `(1- size)` elements.

### `RB-CLEAR` (function)

    (RB-CLEAR RING-BUFFER)

Clear the contents of `ring-buffer`.

### `RB-CONTENTS` (function)

    (RB-CONTENTS RING-BUFFER &KEY (RESULT-TYPE 'LIST))

Return a fresh sequence of the contents of `ring-buffer` (oldest to newest).

  `result-type` can be `list` or `vector`.

  

### `RB-COUNT` (function)

    (RB-COUNT RING-BUFFER)

Return the number of elements currently stored in `ring-buffer`.

### `RB-EMPTY-P` (function)

    (RB-EMPTY-P RING-BUFFER)

Return whether `ring-buffer` is empty.

### `RB-FULL-P` (function)

    (RB-FULL-P RING-BUFFER)

Return whether `ring-buffer` is full.

### `RB-POP` (function)

    (RB-POP RING-BUFFER)

Remove and return the oldest element of `ring-buffer`, or signal an error if it is empty.

### `RB-PUSH` (function)

    (RB-PUSH RING-BUFFER OBJECT)

Push `object` into `ring-buffer` and return `object`.

  If `ring-buffer` is full, its oldest element will be silently dropped.  If you
  want an error to be signaled instead, use `rb-safe-push`.

  

### `RB-REF` (function)

    (RB-REF RING-BUFFER INDEX)

Return the element of `ring-buffer` at `index`.

  Elements are indexed oldest to newest: element 0 is the oldest element in the
  ring buffer, element 1 is the second oldest, and so on.

  Negative indices are supported: element -1 is the newest element, element -2
  the second newest, and so on.

  An error will be signaled if `index` is out of range.

  

### `RB-SAFE-PUSH` (function)

    (RB-SAFE-PUSH RING-BUFFER OBJECT)

Push `object` into `ring-buffer`, or signal an error if it is already full.

### `RB-SIZE` (function)

    (RB-SIZE RING-BUFFER)

Return the size of `ring-buffer`.

  A ring buffer can hold at most `(1- (rb-size ring-buffer))` elements.

  

### `RING-BUFFER` (struct)

Slots: `DATA`, `R`, `W`

## Package `LOSH.SEQUENCES`

Utilities for operating on sequences.

### `CHUNK` (function)

    (CHUNK SEQUENCE CHUNK-SIZE)

Split `sequence` into a list of subsequences of size `chunk-size`.

  The final chunk may be smaller than `chunk-size` if the length of `sequence`
  is not evenly divisible by `chunk-size`.

  

### `DEFINE-SORTING-PREDICATE` (macro)

    (DEFINE-SORTING-PREDICATE NAME PREDICATE-SPEC &REST MORE-PREDICATE-SPECS)

Define `name` as a predicate that composes the given predicates.

  This function takes one or more predicates and composes them into a single
  predicate suitable for passing to `sort`.  Earlier predicates will take
  precedence over later ones — later predicates will only be called to break
  ties for earlier predicates.  This is useful if you want to do something like
  "sort customers by last name, then by first name, then by ID number".

  `predicate-spec` can be one of:

  * A quoted symbol.
  * `(function ...)`
  * `(lambda ...)`
  * A list of `(predicate &key key)`, where `predicate` is any of the above.

  If a `key` is specified, it will be called on arguments before passing them to
  `predicate`.  Note that the `key` only affects the predicate it's consed to,
  not later predicates.

  See `make-sorting-predicate` for a functional version.

  Examples:

    ;; Sort shorter strings first, breaking ties lexicographically:
    (define-sorting-predicate fancy<
      (#'< :key #'length)
      #'string<)

    (sort (list "zz" "abc" "yy") #'fancy<)
    ; => ("yy" "zz" "abc")

    ;; Sort customers by last name, then first name, then ID number:
    (define-sorting-predicate customer<
       (#'string< :key #'last-name)
       (#'string< :key #'first-name)
       (#'< :key #'id))

    (sort (find-customers) #'customer<)

  

### `DOSEQ` (macro)

    (DOSEQ (VAR SEQUENCE)
      &BODY
      BODY)

Perform `body` with `var` bound to each element in `sequence` in turn.

  It's like `cl:dolist`, but for all sequences.

  

### `DROP` (function)

    (DROP N SEQ)

Return a fresh copy of the `seq` without the first `n` elements.

  The result will be of the same type as `seq`.

  If `seq` is shorter than `n` an empty sequence will be returned.

  Example:

    (drop 2 '(a b c))
    => (c)

    (drop 4 #(1))
    => #()

  From Serapeum.

  

### `DROP-WHILE` (function)

    (DROP-WHILE PREDICATE SEQ)

Drop elements from `seq` as long as `predicate` remains true.

  The result will be a fresh sequence of the same type as `seq`.

  Example:

    (drop-while #'evenp '(2 4 5 6 7 8))
    ; => (5 6 7 8)

    (drop-while #'evenp #(2))
    ; => #(2)

  

### `ENUMERATE` (function)

    (ENUMERATE SEQUENCE &KEY (START 0) (STEP 1) KEY)

Return an alist of `(n . element)` for each element of `sequence`.

  `start` and `step` control the values generated for `n`, NOT which elements of
  the sequence are enumerated.

  Examples:

    (enumerate '(a b c))
    ; => ((0 . A) (1 . B) (2 . C))

    (enumerate '(a b c) :start 1)
    ; => ((1 . A) (2 . B) (3 . C))

    (enumerate '(a b c) :key #'alexandria:make-keyword)
    ; => ((0 . :A) (1 . :B) (2 . :C))

  

### `EXTREMA` (function)

    (EXTREMA PREDICATE SEQUENCE &KEY (KEY #'IDENTITY))

Return the smallest and largest elements of `sequence` according to `predicate`.

  `predicate` should be a strict ordering predicate (e.g. `<`).

  Returns the smallest and largest elements in the sequence as two values,
  respectively.

  

### `FREQUENCIES` (function)

    (FREQUENCIES SEQUENCE &KEY (TEST #'EQL) KEY)

Return a hash table containing the frequencies of the elements of `sequence`.

  When `key` is given, it will be called on the elements first before they are
  counted.

  Uses `test` for the `:test` of the hash table.

  Example:

    (frequencies '(foo foo bar))
    => {foo 2
        bar 1}

  

### `GROUP-BY` (function)

    (GROUP-BY FUNCTION SEQUENCE &KEY (TEST #'EQL) (KEY #'IDENTITY) (MAP #'IDENTITY))

Return a hash table of the elements of `sequence` grouped by `function`.

  This function groups the elements of `sequence` into buckets.  The bucket for
  an element is determined by calling `function` on it.

  The result is a hash table (with test `test`) whose keys are the bucket
  identifiers and whose values are lists of the elements in each bucket.  The
  order of these lists is unspecified.

  If `key` is given it will be called on each element before passing it to
  `function` to produce the bucket identifier.  This does not effect what is
  stored in the lists.

  If `map` is given it will be called on each element before storing it in the
  hash table.

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

  

### `MAKE-SORTING-PREDICATE` (function)

    (MAKE-SORTING-PREDICATE PREDICATE-SPEC &REST MORE-PREDICATE-SPECS)

Compose the given predicates into a single predicate and return it.

  This function takes one or more predicates and composes them into a single
  predicate suitable for passing to `sort`.  Earlier predicates will take
  precedence over later ones — later predicates will only be called to break
  ties for earlier predicates.  This is useful if you want to do something like
  "sort customers by last name, then by first name, then by ID number".

  `predicate-spec` can be either a function or a cons of `(predicate . key)`,
  in which case the key will be called on arguments before passing them to
  `predicate`.  Note that the `key` only affects the predicate it's consed to,
  not later predicates.

  See `define-sorting-predicate` for a convenient way to define named sorting
  predicates.

  Examples:

    ;; Trivial example:
    (sort (list "zz" "abc")
          (make-sorting-predicate #'string<))
    ; => ("abc" "zz")

    ;; Sort shorter strings first, breaking ties lexicographically:
    (sort (list "zz" "abc" "yy")
          (make-sorting-predicate (cons #'< #'length) #'string<))
    ; => ("yy" "zz" "abc")

    ;; Sort customers by last name, then first name, then ID number:
    (sort (find-customers)
          (make-sorting-predicate
            (cons #'string< #'last-name)
            (cons #'string< #'first-name)
            (cons #'< #'id)))

  

### `NGRAMS` (function)

    (NGRAMS N SEQUENCE)

Return a list of the `n`grams of `sequence`.

  The length of `sequence` must be at least `n`.

  

### `PREFIX-SUMS` (function)

    (PREFIX-SUMS SEQUENCE &KEY KEY (RESULT-TYPE 'LIST))

Return the prefix sums of the elements of `sequence`.

  If `key` is given, it will be called on the elements before summing.
  `result-type` must be a type suitable for passing to `map`.

  Example:

    (prefix-sums '(10 10 10 0 1))
    ; => (10 20 30 30 31)

    (prefix-sums "ABCD" :key #'char-code :result-type '(vector fixnum))
    ; => #(65 131 198 266)

  

### `PRODUCT` (function)

    (PRODUCT SEQUENCE &KEY KEY (INITIAL-VALUE 1) MODULO)

Return the product of all elements of `sequence`.

  If `key` is given, it will be called on each element to compute the
  multiplicand.

  If `initial-value` is given, it will be used instead of 1 to seed the
  multiplication.

  If `modulo` is given the successive products will be modulo'ed by it along the
  way, which can prevent the need for bignums if you don't need the full result.

  Examples:

    (product #(1 2 3))
    ; => 6

    (product #(1 2 3) :modulo 5)
    ; => 1

    (product #(1 2 3) :modulo 5 :initial-value 2)
    ; => 2

    (product '("1" "2" "3") :key #'parse-integer)
    ; => 6

    (product '("1" "2" "3") :key #'length)
    ; => 1

  

### `PROPORTIONS` (function)

    (PROPORTIONS SEQUENCE &KEY (TEST 'EQL) (FLOAT T) KEY)

Return a hash table containing the proportions of the items in `sequence`.

  When `key` is given, it will be called on the elements first before they are
  counted.

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

  

### `REDUCTIONS` (function)

    (REDUCTIONS FUNCTION SEQUENCE &KEY KEY FROM-END START END (INITIAL-VALUE NIL IV?)
                (RESULT-TYPE 'LIST))

Return a list of intermediate values of `reduce`ing `function` over `sequence`.

  If `initial-value` is provided it will be included as the first element in the
  results.

  If `from-end` is true the sequence will be walked in reverse order, but the
  order of the *results* will still be in the order they were produced (with the
  `initial-value` first, if one is provided).

  Like `reduce`, `key` is only called on the elements of `sequence`, *not* on
  `initial-value` if one is provided.

  *Unlike* `reduce`, if the (sub)sequence is empty (and no `initial-value` is
  provided) an empty list will be returned, instead of calling `function` with
  no arguments.

  `result-type` must be a subtype of `list` or `vector`.

  Examples:

    (reductions #'+ '(0 1 2 3))
    ; => (0 1 3 6)

    (reductions #'+ '(0 1 2 3) :from-end t)
    ; => (3 5 6 6)

    (reductions #'+ '(10 20 30) :initial-value 100)
    ; => (100 110 120 130)

    (reductions #'+ '((10) (20) (30)) :initial-value 100 :key #'car)
    ; => (100 110 120 130)

    (reductions #'+ '(10 20 30) :start 1 :end 1)
    ; => ()

    (reductions #'+ '(10 20 30) :start 1 :end 1 :initial-value 111)
    ; => (111)

  

### `STRING-JOIN` (function)

    (STRING-JOIN SEPARATOR SEQUENCE)

Join a `sequence` of objects into a string, separated by `separator`.

  All objects in `sequence` (and `separator`) will be `princ`ed before joining.

  

### `SUMMATION` (function)

    (SUMMATION SEQUENCE &KEY KEY (INITIAL-VALUE 0) MODULO)

Return the sum of all elements of `sequence`.

  If `key` is given, it will be called on each element to compute the addend.

  If `initial-value` is given, it will be used instead of 0 to seed the addition.

  If `modulo` is given the successive sums will be modulo'ed by it along the
  way, which can prevent the need for bignums if you don't need the full result.

  This function's ugly name was chosen so it wouldn't clash with iterate's `sum`
  symbol.  Sorry.

  Examples:

    (summation #(1 2 3))
    ; => 6

    (summation '("1" "2" "3") :key #'parse-integer)
    ; => 6

    (summation '("1" "2" "3") :key #'length)
    ; => 3

  

### `TAKE` (function)

    (TAKE N SEQ)

Return a fresh sequence of the first `n` elements of `seq`.

  The result will be of the same type as `seq`.

  If `seq` is shorter than `n` a shorter result will be returned.

  Example:

    (take 2 '(a b c))
    => (a b)

    (take 4 #(1))
    => #(1)

  From Serapeum.

  

### `TAKE-WHILE` (function)

    (TAKE-WHILE PREDICATE SEQ)

Take elements from `seq` as long as `predicate` remains true.

  The result will be a fresh sequence of the same type as `seq`.

  Example:

    (take-while #'evenp '(2 4 5 6 7 8))
    ; => (2 4)

    (take-while #'evenp #(1))
    ; => #()

  

## Package `LOSH.SHELL`

Utilities for interacting with external programs.

### `*PBCOPY-COMMAND*` (variable)

The shell command to use for `pbcopy`.  When run, this command should set the clipboard contents to its standard input.

### `*PBPASTE-COMMAND*` (variable)

The shell command to use for `pbpaste`.  When run, this command should print the clipboard contents on standard output.

### `PBCOPY` (function)

    (PBCOPY &OPTIONAL (OBJECT *))

`pbcopy` the `aesthetic-string` of `object`.

### `PBPASTE` (function)

    (PBPASTE)

`pbpaste` the current clipboard as a string.

### `RSCRIPT` (function)

    (RSCRIPT CODE &REST ARGS)

Invoke `Rscript` on the given `code` and `args`.

  `code` must be a string of R code and will be piped into `Rscript` over
  `stdin`.  Use `rscript-file` if you have a file of R code to run.

  `args` will be passed as command line arguments and can be retrieved on the
  R side with e.g.:

    args <- commandArgs(trailingOnly=TRUE)

  

### `RSCRIPT-FILE` (function)

    (RSCRIPT-FILE PATH &REST ARGS)

Invoke `Rscript` on the given `path` and `args`.

  `path` must be a path to a file R code.  Use `rscript` if you have a string of
  R code to run.

  `args` will be passed as command line arguments and can be retrieved on the
  R side with e.g.:

    args <- commandArgs(trailingOnly=TRUE)

  

### `SH` (function)

    (SH COMMAND &KEY INPUT (WAIT T) (RESULT-TYPE 'NULL))

Run `command`, piping `input` to it, optionally returning its output.

  `command` must be either a string (the program), or a list of the program and
  its arguments.

  `wait` must be a boolean.  If true, this function will block until the command
  completes.  If false, it will return immediately and allow the program to run
  asynchronously.

  `input` must be a character input stream, a string, a list of strings, or
  `nil`.  If non-`nil` its contents will be sent to the program as its standard
  input.  A list of strings will be sent separated by newlines.

  `result-type` must be one of:

  * `null`: output will be sent to `/dev/null` and `nil` returned.
  * `stream`: output will be returned as a character stream.
  * `string`: all output will be gathered up and returned as a single string.
  * `list`: all output will be gathered up and returned as a list of lines.
  * `vector`: all output will be gathered up and returned as a vector of octets.

  If `wait` is `nil`, the only acceptable values for `result-type` are `null`
  and `stream`.

  

## Package `LOSH.STREAMS`

Utilities related to strings, reading, and/or printing.

### `WITH-EOF-HANDLED` (macro)

    (WITH-EOF-HANDLED (STREAM EOF-ERROR-P EOF-VALUE)
      &BODY
      BODY)

## Package `LOSH.WEIGHTLISTS`

A simple data structure for choosing random items with weighted probabilities.

### `MAKE-WEIGHTLIST` (function)

    (MAKE-WEIGHTLIST WEIGHTS-AND-ITEMS)

Make a weightlist of the given items and weights.

  `weights-and-items` should be an alist of `(weight . item)` pairs.

  Weights can be any `real` numbers.  Weights of zero are fine, as long as at
  least one of the weights is nonzero (otherwise there's nothing to choose).

  

### `WEIGHTLIST` (struct)

Slots: `WEIGHTS`, `SUMS`, `ITEMS`, `TOTAL`

### `WEIGHTLIST-RANDOM` (function)

    (WEIGHTLIST-RANDOM WEIGHTLIST)

Return a random item from the weightlist, taking the weights into account.

