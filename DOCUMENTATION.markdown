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

### `BISECT-LEFT` (function)

    (BISECT-LEFT PREDICATE VECTOR TARGET)

Bisect `vector` based on `(predicate el target)` and return the LEFT element

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

  

### `BISECT-RIGHT` (function)

    (BISECT-RIGHT PREDICATE VECTOR TARGET)

Bisect `vector` based on `(predicate el target)` and return the RIGHT element

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

  

## Package `LOSH.CONTROL-FLOW`

Utilities for managing control flow.

### `-<>` (macro)

    (-<> &REST FORMS)

Thread the given forms, with `<>` as a placeholder.

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

    (WHEN-FOUND VAR
        LOOKUP-EXPR
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

    (when-found val (gethash :foo hash)
      body)

  

### `WHEN-LET*` (macro)

    (WHEN-LET* BINDING-FORMS
      &BODY
      BODY)

Bind the forms in `binding-forms` in order, short-circuiting on `nil`.

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

  

## Package `LOSH.DEBUGGING`

Utilities for figuring out what the hell is going on.

### `AESTHETIC-STRING` (function)

    (AESTHETIC-STRING THING)

Return the string used to represent `thing` when printing aesthetically.

### `BITS` (function)

    (BITS N SIZE &OPTIONAL (STREAM T))

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

  

### `PR` (function)

    (PR &REST ARGS)

Print `args` readably, separated by spaces and followed by a newline.

  Returns the first argument, so you can just wrap it around a form without
  interfering with the rest of the program.

  This is what `print` should have been.

  

### `PRINT-HASH-TABLE` (function)

    (PRINT-HASH-TABLE HASH-TABLE &OPTIONAL (STREAM T))

Print a pretty representation of `hash-table` to `stream.`

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

  

### `SHUT-UP` (macro)

    (SHUT-UP
      &BODY
      BODY)

Run `body` with stdout and stderr redirected to the void.

### `START-PROFILING` (function)

    (START-PROFILING &OPTIONAL CALL-COUNT-PACKAGES)

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

  

### `DLAMBDA` (macro)

    (DLAMBDA &REST CLAUSES)

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

    (GNUPLOT DATA &REST ARGS &KEY (X #'CAR) (Y #'CDR) &ALLOW-OTHER-KEYS)

Plot `data` to `filename` with gnuplot.

  This will (silently) quickload the `external-program` system to handle the
  communication with gnuplot.

  `data` should be a sequence of data points to plot.

  `x` should be a function to pull the x-values from each item in data.

  `y` should be a function to pull the y-values from each item in data.

  See the docstring of `gnuplot-args` for other keyword arguments.

  

### `GNUPLOT-ARGS` (function)

    (GNUPLOT-ARGS &KEY (OUTPUT :QT) (FILENAME plot.png) (STYLE :LINES)
                  (SIZE-X 1200) (SIZE-Y 800) (LABEL-X) (LABEL-Y)
                  (LINE-TITLE 'DATA) (LINE-WIDTH 4) (AXIS-X NIL) (AXIS-Y NIL)
                  (GRAPH-TITLE) (LOGSCALE-X NIL) (LOGSCALE-Y NIL)
                  &ALLOW-OTHER-KEYS)

Return the formatted command line arguments for the given gnuplot arguments.

  You shouldn't call this function directly — it's exposed just so you can see
  the list of possible gnuplot arguments all in one place.

  

### `GNUPLOT-EXPR` (macro)

    (GNUPLOT-EXPR EXPR &REST ARGS)

Plot `expr` (an expression involving `x`) with gnuplot.

  See the docstring of `gnuplot-args` for other keyword arguments.

  

### `GNUPLOT-FUNCTION` (function)

    (GNUPLOT-FUNCTION FUNCTION &REST ARGS &KEY (START 0.0) (END 1.0) (STEP 0.1)
                      (INCLUDE-END NIL) &ALLOW-OTHER-KEYS)

Plot `function` over [`start`, `end`) by `step` with gnuplot.

  If `include-end` is `t` the `end` value will also be plotted.

  See the docstring of `gnuplot-args` for other keyword arguments.

  

## Package `LOSH.HASH-SETS`

Simple hash set implementation.

### `COPY-HASH-SET` (function)

    (COPY-HASH-SET INSTANCE)

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

### `MUTATE-HASH-VALUES` (function)

    (MUTATE-HASH-VALUES FUNCTION HASH-TABLE)

Replace each value in `hash-table` with the result of calling `function` on it.

  Returns the hash table.

  

## Package `LOSH.ITERATE`

Custom `iterate` drivers and clauses.

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

  

## Package `LOSH.LICENSING`

Utilities related to open source licenses.

### `PRINT-LICENSES` (function)

    (PRINT-LICENSES QUICKLISP-PROJECT-DESIGNATOR)

Print the licenses used by the given project and its dependencies.

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

Return a random number within `spread` of `value` (inclusive).

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

Return a random number in [`min`, `max`).

### `RANDOM-RANGE-EXCLUSIVE` (function)

    (RANDOM-RANGE-EXCLUSIVE MIN MAX)

Return a random number in (`min`, `max`).

### `RANDOM-RANGE-INCLUSIVE` (function)

    (RANDOM-RANGE-INCLUSIVE MIN MAX)

Return a random number in [`min`, `max`].

### `RANDOMP` (function)

    (RANDOMP &OPTIONAL (CHANCE 0.5))

Return a random boolean with `chance` probability of `t`.

## Package `LOSH.SEQUENCES`

Utilities for operating on sequences.

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

    (enumerate '(a b c) :key #'ensure-keyword)
    ; => ((0 . :A) (1 . :B) (2 . :C))

  

### `EXTREMA` (function)

    (EXTREMA PREDICATE SEQUENCE)

Return the smallest and largest elements of `sequence` according to `predicate`.

  `predicate` should be a strict ordering predicate (e.g. `<`).

  Returns the smallest and largest elements in the sequence as two values,
  respectively.

  

### `FREQUENCIES` (function)

    (FREQUENCIES SEQUENCE &KEY (TEST 'EQL))

Return a hash table containing the frequencies of the items in `sequence`.

  Uses `test` for the `:test` of the hash table.

  Example:

    (frequencies '(foo foo bar))
    => {foo 2
        bar 1}

  

### `GROUP-BY` (function)

    (GROUP-BY FUNCTION SEQUENCE &KEY (TEST #'EQL) (KEY #'IDENTITY))

Return a hash table of the elements of `sequence` grouped by `function`.

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

  

### `PREFIX-SUMS` (function)

    (PREFIX-SUMS SEQUENCE)

Return a list of the prefix sums of the numbers in `sequence`.

  Example:

    (prefix-sums '(10 10 10 0 1))
    => (10 20 30 30 31)

  

### `PROPORTIONS` (function)

    (PROPORTIONS SEQUENCE &KEY (TEST 'EQL) (FLOAT T))

Return a hash table containing the proportions of the items in `sequence`.

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

