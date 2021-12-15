(in-package :losh.iterate)

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
                  PER-ITERATION-INTO per-var
                  SECONDS seconds?)
  "Time [real/run]-time into variables.

  `time-type` should be either the symbol `run-time` or `real-time`, depending
  on which kind of time you want to track.  Times are reported in internal time
  units, unless `seconds?` is given, in which case they will be converted to
  a `single-float` by dividing by `internal-time-units-per-second`.

  If `since-var` is given, on each iteration it will be bound to the amount of
  time that has passed since the beginning of the loop.

  If `per-var` is given, on each iteration it will be bound to the amount of
  time that has passed since the last time it was set.  On the first iteration
  it will be bound to the amount of time since the loop started.

  If neither var is given, it is as if `since-var` were given and returned as
  the value of the `iterate` statement.

  `seconds?` is checked at compile time, not runtime.

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
             (timing real-time :since-start-into s :per-iteration-into p :seconds t)
             (sleep 1.0)
             (collect (list s p)))
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
    (flet ((convert (val)
             (if seconds?
               `(/ ,val internal-time-units-per-second 1.0f0)
               val)))
      (with-gensyms (start-time current-time previous-time)
        `(progn
           (with ,start-time = (,timing-function))
           (for ,current-time = (,timing-function))
           ,@(when since-var
               `((for ,since-var = ,(convert `(- ,current-time ,start-time)))))
           ,@(when per-var
               `((for ,previous-time :previous ,current-time :initially ,start-time)
                 (for ,per-var = ,(convert `(- ,current-time ,previous-time))))))))))


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
        (generate ,seq :in-whatever (remove-if #'alexandria:emptyp ,seqs))
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


(defmacro-driver (FOR var AROUND seq)
  "Iterate cyclically around items in the given sequence.

  The results are undefined if the sequence is empty.

  "
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (is-list original source i len)
      `(progn
         (with ,original = ,seq)
         (with ,source = ,original)
         (with ,is-list = (typep ,source 'list))
         (with ,len = (if ,is-list -1 (length ,source)))
         (with ,i = -1)
         (,kwd ,var :next (if ,is-list
                            (progn (unless ,source (setf ,source ,original))
                                   (pop ,source))
                            (progn (incf ,i)
                                   (when (= ,i ,len)
                                     (setf ,i 0))
                                   (elt ,source ,i))))))))


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
                  SKIP-ORIGIN should-skip-origin
                  ORIGIN origin)
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
  ;; TODO rewrite this as bare `for`s without all the generator cruft to avoid
  ;; the bullshit SBCL `deleting unreachable code` garbage we get every time
  ;; skip-origin is true.
  (let* ((delta-vars (ensure-list delta-vars))
         (origin-vars (mapcar (lambda (dv) (gensym (mkstr 'origin- dv)))
                              delta-vars))
         (origin-vals (if (null origin)
                        (mapcar (constantly 0) delta-vars)
                        origin)))
    (with-gensyms (r control skip)
      `(progn
         (with ,r = ,radius)
         (with ,skip = ,should-skip-origin)
         ,@(mapcar (lambda (ovar oval)
                     `(with ,ovar = ,oval))
                   origin-vars origin-vals)
         (generate-nested ,(iterate (for var :in delta-vars)
                                    (for orig :in origin-vars)
                                    (collect `(,var :from (- ,orig ,r) :to (+ ,orig ,r))))
                          :control-var ,control)
         (next ,control)
         (when (and ,skip
                    ,@(iterate (for var :in (ensure-list delta-vars))
                               (for ovar :in origin-vars)
                               (collect `(= ,ovar ,var))))
           (next ,control))))))


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

(defmacro-clause (COLLECT-SET element &optional
                  INTO var
                  TEST (test '#'eql))
  "Collect elements into a hash set at `var`.

  If `var` is omitted the hash set will be returned instead.

  `test` specifies the test used for the hash set.

  Example:

    (iterate (for y :in '(a b a))
             (collect-set y))
    ; => {a b}

  "
  (let ((hash-set (or var iterate::*result-var*)))
    `(progn
       (with ,hash-set = (make-hash-set :test ,test))
       (hset-insert! ,hash-set ,element))))

(defmacro-clause (COLLECT-FREQUENCIES expr &optional
                  INTO var
                  TEST (test '#'eql))
  "Collect frequencies of `expr` values into a hash table at `var`.

  If `var` is omitted the hash table will be returned instead.

  `test` specifies the test used for the hash table.

  Example:

    (iterate (for x :in '(b a n a n a s))
             (collect-frequencies x))
    ; => {b 1
    ;     a 3
    ;     n 2
    ;     s 1}

  "
  (let ((hash-table (or var iterate::*result-var*)))
    `(progn
       (with ,hash-table = (make-hash-table :test ,test))
       (incf (gethash ,expr ,hash-table 0)))))

(defmacro-clause (CONCATENATING expr &optional INTO var SEPARATOR separator)
  "Concatenate the string `expr` into `var`.

  If `var` is not given, `expr` will be accumulated into a string output stream
  and the result returned.

  If `var` is given, `expr` will be concatenated onto it.  Whether `var` is
  a fresh string each time or whether an adjustable string is mutated is
  implementation defined.

  If `separator` is not `nil` it must be a string designator, and it will be
  evaluated once at the beginning of the iterate form.

  Examples:

    (iterate (for s :in '(\"foo\" \"bar\" \"baz\"))
             (concatenating s))
    ; => \"foobarbaz\"

    (iterate (for s :in '(\"foo\" \"bar\" \"baz\"))
             (concatenating s :separator #\,))
    ; => \"foo,bar,baz\"

    (iterate (for s :in '(\"foo\" \"bar\" \"baz\"))
             (concatenating s :separator \", \"))
    ; => \"foo, bar, baz\"

    (iterate (for s :in '(\"foo\" \"bar\" \"baz\"))
             (concatenating s :separator #\, :into v)
             (format t \"> ~A~%\" v)
             (collect v))
    ; => > foo
    ; => > foo,bar
    ; => > foo,bar,baz
    ;
    ; Implementation defined result, might be one of:
    ; => (\"foo\" \"foo,bar\" \"foo,bar,baz\")             ; 3 fresh strings
    ; => (\"foo,bar,baz\" \"foo,bar,baz\" \"foo,bar,baz\") ; same string

  "
  (if var
    (let ((separator% (gensym "SEPARATOR"))
          (sep (gensym "SEP")))
      `(progn
         (with ,separator% = ,separator)
         (with ,sep = (if ,separator% (string ,separator%) ""))
         (reducing ,expr
                   :by (lambda (a b)
                         (if (null a)
                           (copy-seq b)
                           (concatenate 'string a ,sep b)))
                   :into ,var
                   :initial-value nil)))
    (let ((separator% (gensym "SEPARATOR"))
          (sos (gensym "SOS"))
          (sep (gensym "SEP")))
      `(progn
         (with ,separator% = ,separator)
         (with ,sos = nil)
         (with ,sep = (if (or (null ,separator%) (equal ,separator% ""))
                        nil
                        (string ,separator%)))
         (if (null ,sos)
           (setf ,sos (make-string-output-stream))
           (when ,sep
             (write-string ,sep ,sos)))
         (write-string ,expr ,sos)
         (finally (return (get-output-stream-string ,sos)))))))


(defmacro-clause (ORING expr &optional INTO var)
  (let ((result (or var iterate::*result-var*)))
    `(reducing ,expr :by #'or :into ,result :initial-value nil)))

(defmacro-clause (ANDING expr &optional INTO var)
  (let ((result (or var iterate::*result-var*)))
    `(reducing ,expr :by #'and :into ,result :initial-value t)))


(defun keywordize-clause (clause)
  (iterate
    (for (k v . nil) :on clause :by #'cddr)
    (collect (alexandria:make-keyword k))
    (collect v)))

(defun keywordize-some-of-clause (clause)
  ; please kill me
  (append (list (first clause) (second clause))
          (keywordize-clause (nthcdr 2 clause))))

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
    `(,kwd (,var) :in-hashtable (losh.hash-sets::hash-set-storage ,hset))))


(defmacro-driver (FOR var IN-RING-BUFFER ring-buffer)
  "Iterate over the elements of `ring-buffer`, oldest to newest.

  Does not modify the ring buffer.

  "
  (let ((kwd (if generate 'generate 'for)))
    (with-gensyms (rb r w d s)
      `(progn
         (with ,rb = ,ring-buffer)
         (with ,r = (losh.ring-buffers::r ,rb))
         (with ,w = (losh.ring-buffers::w ,rb))
         (with ,d = (losh.ring-buffers::data ,rb))
         (with ,s = (losh.ring-buffers::size ,rb))
         (,kwd ,var :next (if (= ,r ,w)
                            (terminate)
                            (prog1 (svref ,d ,r)
                              (incf ,r)
                              (when (= ,r ,s)
                                (setf ,r 0)))))))))


(defmacro-driver (FOR var SEED seed THEN then)
  "Bind `var` to `seed` initially, then to `then` on every iteration.

  This differs from `(FOR … FIRST … THEN …)` and `(FOR … INITIALLY … THEN …)`
  because `then` is evaluated on every iteration, *including* the first.

  Example:

    (iterate
      (repeat 3)
      (for x :first     0 :then (1+ x))
      (for y :initially 0 :then (1+ y))
      (for z :seed      0 :then (1+ z))
      (collect (list x y z)))
    ; =>
    ((0 0 1)
     (1 1 2)
     (2 2 3))

  "
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (,kwd ,var :next ,then)
       (initially (setf ,var ,seed)))))


(deftype sharp-quoted-function ()
  '(cons (eql function)
         (cons t null)))

(defmacro-clause (FINDING-ALL expr MINIMIZING m-expr &optional INTO var)
  "Collect all `expr`s minimizing `m-expr` into a list at `var`.

  The partial list at `var` is available for inspection at any point in the loop.

  If `m-expr` is a sharp-quoted function, then it is called on `expr` instead of
  being evaluated and compared itself.

  "
  ;; TODO: result-type
  (with-gensyms (min value m-value tail)
    (let ((result (or var iterate::*result-var*)))
      `(progn
         (with ,result = '())
         (with ,tail = nil)
         (with ,min = nil)
         ,(typecase m-expr
            (sharp-quoted-function
             `(progn
                (for ,value = ,expr)
                (for ,m-value = (funcall ,m-expr ,value))
                (cond
                  ((or (null ,min)
                       (< ,m-value ,min)) (setf ,result (list ,value)
                                                ,tail ,result
                                                ,min ,m-value))
                  ((= ,m-value ,min) (setf (cdr ,tail) (cons ,value nil)
                                           ,tail (cdr ,tail))))))
            (t `(progn
                  (for ,m-value = ,m-expr)
                  (cond
                    ((or (null ,min)
                         (< ,m-value ,min)) (setf ,result (list ,expr)
                                                  ,tail ,result
                                                  ,min ,m-value))
                    ((= ,m-value ,min) (setf (cdr ,tail) (cons ,expr nil)
                                             ,tail (cdr ,tail)))))))))))

(defmacro-clause (FINDING-ALL expr MAXIMIZING m-expr &optional INTO var)
  "Collect all `expr`s maximizing `m-expr` into a list at `var`.

  The partial list at `var` is available for inspection at any point in the loop.

  If `m-expr` is a sharp-quoted function, then it is called on `expr` instead of
  being evaluated and compared itself.

  "
  ;; TODO: result-type
  (with-gensyms (max value m-value tail)
    (let ((result (or var iterate::*result-var*)))
      `(progn
         (with ,result = '())
         (with ,tail = nil)
         (with ,max = nil)
         ,(typecase m-expr
            (sharp-quoted-function
             `(progn
                (for ,value = ,expr)
                (for ,m-value = (funcall ,m-expr ,value))
                (cond
                  ((or (null ,max)
                       (> ,m-value ,max)) (setf ,result (list ,value)
                                                ,tail ,result
                                                ,max ,m-value))
                  ((= ,m-value ,max) (setf (cdr ,tail) (cons ,value nil)
                                           ,tail (cdr ,tail))))))
            (t `(progn
                  (for ,m-value = ,m-expr)
                  (cond
                    ((or (null ,max)
                         (> ,m-value ,max)) (setf ,result (list ,expr)
                                                  ,tail ,result
                                                  ,max ,m-value))
                    ((= ,m-value ,max) (setf (cdr ,tail) (cons ,expr nil)
                                             ,tail (cdr ,tail)))))))))))

(defmacro-clause (FINDING-ALL expr SUCH-THAT test &optional INTO var RESULT-TYPE result-type)
  "Collect all `expr`s for which `test` is true.

  If `test` is a sharp-quoted function, then it is called on `expr` instead of
  being evaluated and compared itself.

  "
  (let ((result (or var iterate::*result-var*)))
    (typecase test
      (sharp-quoted-function
        (with-gensyms (value)
          `(progn
             (for ,value = ,expr)
             (when (funcall ,test ,value)
               (collect ,value :into ,result
                        ,@(when result-type `(:result-type ,result-type)))))))
      (t `(when ,test
            (collect ,expr :into ,result
                     ,@(when result-type `(:result-type ,result-type))))))))

(defmacro-clause (FINDING-FIRST expr SUCH-THAT test &optional INTO var)
  "Collect the first `expr` for which `test` is true.

  Unlike vanilla `finding`, it does not block further iteration.

  If `test` is a sharp-quoted function, then it is called on `expr` instead of
  being evaluated and compared itself.

  "
  (with-gensyms (value found)
    (let ((result (or var iterate::*result-var*)))
      `(progn
         (with ,found)
         ,@(when var (list `(with ,var)))
         ,(typecase test
            (sharp-quoted-function
              `(unless ,found
                 (for ,value = ,expr)
                 (when (funcall ,test ,value)
                   (setf ,found t ,result ,value))))
            (t `(unless ,found
                  (when ,test
                    (setf ,found t ,result ,expr)))))))))


(defmacro returning (&rest values)
  "Return `values` from the iterate clause.

  Equivalent to `(finally (return (values ...)))`.

  "
  `(finally (return (values ,@values))))

(defmacro with-result (symbol = expr)
  "Bind `expr` to symbol using `with`, and return it at the end.

  Equivalent to `(progn (with symbol = expr) (returning expr))`.

  "
  (assert (eql = '=))
  `(progn (with ,symbol = ,expr)
     (returning ,symbol)))

(defmacro-driver (FOR var-or-vars MATCHING regex AGAINST string &optional OVERLAP overlap? START start END end)
  "Iterate over the matches of `regex` in `string`, binding `var-or-vars`.

  `regex` must be a suitable argument for passing to `ppcre:create-scanner`.
  Note that `ppcre:create-scanner` accepts already-created scanners and returns
  them unchanged, so you can provide an existing scanner if you wish.

  `var-or-vars` will be bound to the successive matches.  If it is a symbol, it
  will be bound to the entire match.  If it is a list of variables, they will be
  bound to the register groups as if by `ppcre:register-groups-bind`.

  If `overlap?` is true, after finding a match, the next match will be searched
  for from the next character, instead of skipping past the entire previous
  match.

  `generate` is supported.

  Examples:

    (iterate (for word :matching \"\\\\w+\" :against \"foo bar baz\")
             (collect word))
    ; =>
    (\"foo\" \"bar\" \"baz\")

    (iterate (for x :matching \"\\\\w\\\\w\" :against \"abcde\")
             (collect x))
    ; =>
    (\"ab\" \"cd\")

    (iterate (for x :matching \"\\\\w\\\\w\" :against \"abcde\" :overlap t)
             (collect x))
    ; =>
    (\"ab\" \"bc\" \"cd\" \"de\")

    (iterate (for ((#'string-upcase name) (#'parse-integer year month day))
                  :matching \"(\\\\w+)? (\\\\d+)-(\\\\d+)-(\\\\d+)\"
                  :against \"foo 2019-12-06 / 2010-11-14\")
             (collect (list name year month day)))
    ; =>
    ((\"FOO\" 2019 12 6) (NIL 2010 11 14))

    (iterate (for x :matching (ppcre:create-scanner \"foo+\" :case-insensitive-mode t)
                    :against \"FOOOOD\")
             (collect x))
    ; =>
    (\"FOOOO\")

  "
  (let* ((kwd (if generate 'generate 'for))
         (single (symbolp var-or-vars))
         (var (if single var-or-vars nil))
         (vars (unless single
                 (iterate
                   (for spec :in var-or-vars)
                   (etypecase spec
                     (cons (destructuring-bind (function &rest vars) spec
                             (appending (mapcar (curry #'cons function) vars))))
                     (symbol (appending (list `(nil . ,spec)))))))))
    (with-gensyms (scanner% pos% start% end% string% reg-start% reg-end% limit%)
      `(progn
         (with ,pos% = ,(or start 0))
         (with ,string% = ,string)
         (with ,limit% = ,(or end `(length ,string%)))
         (with ,scanner% = (ppcre:create-scanner ,regex))
         (,kwd ,(if single
                  var
                  `(values ,@(mapcar #'cdr vars)))
          :next
          (multiple-value-bind (,start% ,end% ,@(unless single `(,reg-start% ,reg-end%)))
              (ppcre:scan ,scanner% ,string% :start ,pos% :end ,limit%)
            (declare (ignorable ,end%))
            (if (null ,start%)
              (terminate)
              (progn (setf ,pos% ,(if overlap? `(1+ ,start%) end%))
                     ,(if single
                        `(subseq ,string% ,start% ,end%)
                        `(values
                           ,@(iterate
                               (for i :from 0)
                               (for (function . nil) :in vars)
                               (collect
                                 `(when (aref ,reg-start% ,i)
                                    (,@(if function `(funcall ,function) `(progn))
                                     (subseq ,string%
                                             (aref ,reg-start% ,i)
                                             (aref ,reg-end% ,i))))))))))))))))


(defmacro WHEN-FIRST-TIME (expr)
  "Sugar for `(if-first-time expr nil)`."
  `(if-first-time ,expr nil))

(defmacro UNLESS-FIRST-TIME (expr)
  "Sugar for `(if-first-time nil expr)`."
  `(if-first-time nil ,expr))


(defmacro IF-FIRST-ITERATION (then else)
  "Evaluate `then` if this clause is executed on the first iteration, otherwise `else`.

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

  in that it will only evaluate `then` on the first iteration of the loop, "
  (with-gensyms (first-iteration)
    `(progn
       (with ,first-iteration = t)
       (after-each (setf ,first-iteration nil))
       (if ,first-iteration
         ,then
         ,else))))

(defmacro WHEN-FIRST-ITERATION (expr)
  "Sugar for `(if-first-iteration expr nil)`."
  `(if-first-iteration ,expr nil))

(defmacro UNLESS-FIRST-ITERATION (expr)
  "Sugar for `(if-first-iteration nil expr)`."
  `(if-first-iteration nil ,expr))


(defmacro-clause (FOR vars WINDOW size ON sequence-or-length
                  &optional START start END end)
  "Iterate a window over a sequence.

  The exact nature of the iteration depends on the form of `vars`:

  If `vars` is a symbol, or a list of a single symbol, it will be bound to
  a `size`-element `subseq` of `sequence-or-length` on each iteration.  In this
  case, `sequence-or-length` must be a sequence.

  If `vars` is a list of two symbols `(start end)`, they will be bound to the
  start and end bounding indices of a a `size`-element window of
  `sequence-or-length` on each iteration.  In this case, `sequence-or-length`
  can be a sequence (in which case its `length` is used) or an integer.

  If `vars` is a list of three symbols `(subseq start end)`, both of the above
  bindings will happen.  In this case, `sequence-or-length` must be a sequence.

  If `start` or `end` are given, they are used to restrict the range of the
  sequence being iterated over.

  `generate` is not supported at this time.

  Examples:

    (iterate (with string = \"abcdefg\")
             (for x :window 2 :on string)
             (collect x))
    ; => (\"ab\" \"bc\" \"cd\" \"de\" \"ef\" \"fg\")

    (iterate (with string = \"abcdefg\")
             (for (start end) :window 2 :on 5)
             (collect (subseq string start end)))
    ; => (\"ab\" \"bc\" \"cd\" \"de\")

    (iterate (with string = \"abcdefg\")
             (for (x start end) :window 2 :on string)
             (collect (list x start end)))
    ; => ((\"ab\" 0 2) (\"bc\" 1 3) (\"cd\" 2 4) (\"de\" 3 5) (\"ef\" 4 6) (\"fg\" 5 7))

    (iterate (with string = \"abcdefg\")
             (for (x start end) :window 2 :on string :start 1 :end 5)
             (collect (list x start end)))
    ; => ((\"bc\" 1 3) (\"cd\" 2 4) (\"de\" 3 5))

  "
  (setf vars (ensure-list vars))
  (alexandria:with-gensyms (n seq s)
    (let (subseq% start% end%)
      (ecase (length vars)
        (1 (setf subseq% (first vars)
                 start% (gensym "START")
                 end% (gensym "END")))
        (2 (setf subseq% nil
                 start% (first vars)
                 end% (second vars)))
        (3 (setf subseq% (first vars)
                 start% (second vars)
                 end% (third vars))))
      `(progn
         (with ,s = ,(or start 0))
         (with ,n = ,size)
         (with ,seq = ,sequence-or-length)
         (for ,start% :from ,s)
         (for ,end% :from (+ ,n ,s) :to
              ,(cond
                 (end end)
                 (subseq% `(length ,seq))
                 (t `(etypecase ,seq
                       (integer ,seq)
                       (sequence (length ,seq))))))
         ,@(when subseq%
             `((for ,subseq% = (subseq ,seq ,start% ,end%))))))))
