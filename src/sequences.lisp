(in-package :losh.sequences)

(deftype array-index (&optional (length (1- array-dimension-limit)))
  "An integer in the range `[0, length)`.

  From Alexandria.

  "
  `(integer 0 (,length)))


(defun prefix-sums (sequence &key key (result-type 'list))
  "Return the prefix sums of the elements of `sequence`.

  If `key` is given, it will be called on the elements before summing.
  `result-type` must be a type suitable for passing to `map`.

  Example:

    (prefix-sums '(10 10 10 0 1))
    ; => (10 20 30 30 31)

    (prefix-sums \"ABCD\" :key #'char-code :result-type '(vector fixnum))
    ; => #(65 131 198 266)

  "
  (let ((sum 0))
    (map result-type (lambda (x) (incf sum (if key (funcall key x) x)))
         sequence)))

(defun frequencies (sequence &key (test #'eql) key)
  "Return a hash table containing the frequencies of the elements of `sequence`.

  When `key` is given, it will be called on the elements first before they are
  counted.

  Uses `test` for the `:test` of the hash table.

  Example:

    (frequencies '(foo foo bar))
    => {foo 2
        bar 1}

  "
  (if key
    (iterate (for i :in-whatever sequence)
             (collect-frequencies (funcall key i) :test test))
    (iterate (for i :in-whatever sequence)
             (collect-frequencies i :test test))))

(defun proportions (sequence &key (test 'eql) (float t) key)
  "Return a hash table containing the proportions of the items in `sequence`.

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

  "
  (let* ((freqs (frequencies sequence :test test :key key))
         (total (reduce #'+ (alexandria:hash-table-values freqs)
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


(defun-inline chunk-list (list chunk-size)
  ;; Since lists have O(N) access time, we iterate through manually,
  ;; collecting each chunk as we pass through it. Using SUBSEQ would
  ;; be O(N^2).
  (loop :while list
        :collect (loop :repeat chunk-size :while list :collect (pop list))))

(defun-inline chunk-sequence (sequence chunk-size)
  ;; For other sequences like strings or arrays, we can simply chunk
  ;; by repeated SUBSEQs.
  (loop :with len := (length sequence)
        :for i :below len :by chunk-size
        :collect (subseq sequence i (min len (+ chunk-size i)))))

(defun chunk (sequence chunk-size)
  "Split `sequence` into a list of subsequences of size `chunk-size`.

  The final chunk may be smaller than `chunk-size` if the length of `sequence`
  is not evenly divisible by `chunk-size`.

  "
  ;; Based on `subdivide` from http://quickutil.org/
  (check-type sequence sequence)
  (check-type chunk-size (integer 1))
  (etypecase sequence
    (list (chunk-list sequence chunk-size))
    (sequence (chunk-sequence sequence chunk-size))))


(defun-inline ngrams-list (n list)
  (loop :repeat (1+ (- (length list) n))
        :for l :on list
        :collect (take-list n l)))

(defun-inline ngrams-sequence (n sequence)
  (loop :for i :to (- (length sequence) n)
        :collect (subseq sequence i (+ i n))))

(defun ngrams (n sequence)
  "Return a list of the `n`grams of `sequence`.

  The length of `sequence` must be at least `n`.

  "
  ;; Based on `n-grams` from http://quickutil.org/
  (check-type sequence sequence)
  (check-type n (integer 1))
  (assert (<= n (length sequence)))
  (etypecase sequence
    (list (ngrams-list n sequence))
    (sequence (ngrams-sequence n sequence))))


(defun extrema (predicate sequence &key (key #'identity))
  "Return the smallest and largest elements of `sequence` according to `predicate`.

  `predicate` should be a strict ordering predicate (e.g. `<`).

  Returns the smallest and largest elements in the sequence as two values,
  respectively.

  "
  (iterate
    (with min = (elt sequence 0))
    (with min-val = (funcall key min))
    (with max = (elt sequence 0))
    (with max-val = (funcall key max))
    (for el :in-whatever sequence)
    (for val = (funcall key el))
    (when (funcall predicate val min-val) (setf min el min-val val))
    (when (funcall predicate max-val val) (setf max el max-val val))
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

    (enumerate '(a b c) :key #'alexandria:make-keyword)
    ; => ((0 . :A) (1 . :B) (2 . :C))

  "
  (iterate (for el :in-whatever sequence)
           (for n :from start :by step)
           (collect (cons n (if key
                              (funcall key el)
                              el)))))


(defun reductions/list% (function sequence key from-end start end initial-value iv?)
  (let ((result (list)))
    (labels ((f (&optional (a nil a?) b)
               ;; The only time the reducing function is called with zero
               ;; arguments is if we have an empty (sub)seq.  If that's the case
               ;; we can just bail immediately.
               (when (not a?)
                 (return-from reductions/list% (list)))
               ;; Otherwise push the current value (we'll handle the final one at
               ;; the end) and return the next.
               (push (if from-end b a) result)
               (funcall function a b)))
      (let ((final (if iv?
                     (reduce #'f sequence
                             :key key
                             :from-end from-end
                             :start (or start 0)
                             :end end
                             :initial-value initial-value)
                     ;; We have to specifically NOT pass :initial-value if it's
                     ;; omitted.  We could apply (when …), but that's ugly.
                     (reduce #'f sequence
                             :key key
                             :from-end from-end
                             :start (or start 0)
                             :end end))))
        (push final result)
        (nreverse result)))))

(defun reductions/vector% (function sequence key from-end start end initial-value iv? result-type)
  (let* ((end (or end (length sequence)))
         (start (or start 0))
         (result (make-sequence result-type (+ (- end start) (if iv? 1 0))))
         (i -1))
    (labels ((collect (value)
               (setf (aref result (incf i)) value))
             (f (&optional (a nil a?) b)
               (when (not a?)
                 (return-from reductions/vector% result))
               (collect (if from-end b a))
               (funcall function a b)))
      (let ((final (if iv?
                     (reduce #'f sequence
                             :key key
                             :from-end from-end
                             :start start
                             :end end
                             :initial-value initial-value)
                     (reduce #'f sequence
                             :key key
                             :from-end from-end
                             :start start
                             :end end))))
        (collect final)
        result))))

(defun reductions (function sequence &key
                   key from-end start end
                   (initial-value nil iv?)
                   (result-type 'list))
  "Return a list of intermediate values of `reduce`ing `function` over `sequence`.

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

  "
  (cond ((subtypep result-type 'vector)
         (reductions/vector% function sequence key from-end start end initial-value iv? result-type))
        ((subtypep result-type 'list)
         (reductions/list%   function sequence key from-end start end initial-value iv?))
        (t (error "Bad result-type ~S: must be a subtype of list or vector." result-type))))

(defmacro doseq ((var sequence) &body body)
  "Perform `body` with `var` bound to each element in `sequence` in turn.

  It's like `cl:dolist`, but for all sequences.

  "
  `(map nil (lambda (,var) ,@body) ,sequence))


(defun-inlineable summation (sequence &key key (initial-value 0) modulo)
  "Return the sum of all elements of `sequence`.

  If `key` is given, it will be called on each element to compute the addend.

  If `initial-value` is given, it will be used instead of 0 to seed the addition.

  If `modulo` is given the successive sums will be modulo'ed by it along the
  way, which can prevent the need for bignums if you don't need the full result.

  This function's ugly name was chosen so it wouldn't clash with iterate's `sum`
  symbol.  Sorry.

  Examples:

    (summation #(1 2 3))
    ; => 6

    (summation '(\"1\" \"2\" \"3\") :key #'parse-integer)
    ; => 6

    (summation '(\"1\" \"2\" \"3\") :key #'length)
    ; => 3

  "
  (let ((result initial-value))
    (when modulo (modf result modulo))
    (if modulo
      (if key
        (doseq (n sequence) (setf result (mod (+ result (funcall key n)) modulo)))
        (doseq (n sequence) (setf result (mod (+ result n) modulo))))
      (if key
        (doseq (n sequence) (setf result (+ result (funcall key n))))
        (doseq (n sequence) (setf result (+ result n)))))
    result))

(defun-inlineable product (sequence &key key (initial-value 1) modulo)
  "Return the product of all elements of `sequence`.

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

    (product '(\"1\" \"2\" \"3\") :key #'parse-integer)
    ; => 6

    (product '(\"1\" \"2\" \"3\") :key #'length)
    ; => 1

  "
  (let ((result initial-value))
    (when modulo (modf result modulo))
    (if modulo
      (if key
        (doseq (n sequence) (setf result (mod (* result (funcall key n)) modulo)))
        (doseq (n sequence) (setf result (mod (* result n) modulo))))
      (if key
        (doseq (n sequence) (setf result (* result (funcall key n))))
        (doseq (n sequence) (setf result (* result n)))))
    result))


(defun string-join (separator sequence)
  "Join a `sequence` of objects into a string, separated by `separator`.

  All objects in `sequence` (and `separator`) will be `princ`ed before joining.

  "
  (unless (stringp separator)
    (callf separator #'princ-to-string))
  (with-output-to-string (s)
    (let ((first t))
      (map nil (lambda (el)
                 (if first
                   (setf first nil)
                   (write-string separator s))
                 (princ el s))
           sequence))))


(defun make-sorting-predicate (predicate-spec &rest more-predicate-specs)
  "Compose the given predicates into a single predicate and return it.

  This function takes one or more predicates and composes them into a single
  predicate suitable for passing to `sort`.  Earlier predicates will take
  precedence over later ones — later predicates will only be called to break
  ties for earlier predicates.  This is useful if you want to do something like
  \"sort customers by last name, then by first name, then by ID number\".

  `predicate-spec` can be either a function or a cons of `(predicate . key)`,
  in which case the key will be called on arguments before passing them to
  `predicate`.  Note that the `key` only affects the predicate it's consed to,
  not later predicates.

  See `define-sorting-predicate` for a convenient way to define named sorting
  predicates.

  Examples:

    ;; Trivial example:
    (sort (list \"zz\" \"abc\")
          (make-sorting-predicate #'string<))
    ; => (\"abc\" \"zz\")

    ;; Sort shorter strings first, breaking ties lexicographically:
    (sort (list \"zz\" \"abc\" \"yy\")
          (make-sorting-predicate (cons #'< #'length) #'string<))
    ; => (\"yy\" \"zz\" \"abc\")

    ;; Sort customers by last name, then first name, then ID number:
    (sort (find-customers)
          (make-sorting-predicate
            (cons #'string< #'last-name)
            (cons #'string< #'first-name)
            (cons #'< #'id)))

  "
  (let (predicate key)
    (if (consp predicate-spec)
      (setf predicate (car predicate-spec)
            key (cdr predicate-spec))
      (setf predicate predicate-spec
            key nil))
    (if (null more-predicate-specs)
      (if key
        (lambda (x y)
          (funcall predicate (funcall key x) (funcall key y)))
        predicate)
      (let ((next (apply #'make-sorting-predicate more-predicate-specs)))
        (if key
          (lambda (x y)
            (let ((kx (funcall key x))
                  (ky (funcall key y)))
              (cond
                ((funcall predicate kx ky) t)
                ((funcall predicate ky kx) nil)
                (t (funcall next x y)))))
          (lambda (x y)
            (cond
              ((funcall predicate x y) t)
              ((funcall predicate y x) nil)
              (t (funcall next x y)))))))))

(defmacro define-sorting-predicate (name predicate-spec &rest more-predicate-specs)
  "Define `name` as a predicate that composes the given predicates.

  This function takes one or more predicates and composes them into a single
  predicate suitable for passing to `sort`.  Earlier predicates will take
  precedence over later ones — later predicates will only be called to break
  ties for earlier predicates.  This is useful if you want to do something like
  \"sort customers by last name, then by first name, then by ID number\".

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

    (sort (list \"zz\" \"abc\" \"yy\") #'fancy<)
    ; => (\"yy\" \"zz\" \"abc\")

    ;; Sort customers by last name, then first name, then ID number:
    (define-sorting-predicate customer<
       (#'string< :key #'last-name)
       (#'string< :key #'first-name)
       (#'< :key #'id))

    (sort (find-customers) #'customer<)

  "
  (with-gensyms (x y)
    (labels ((parse-spec (spec)
               "Parse `spec` and return the predicate and key as values."
               (if (consp spec)
                 (if (member (first spec) '(function lambda quote))
                   (values spec '#'identity)
                   (destructuring-bind (predicate &key (key '#'identity)) spec
                     (values predicate key)))
                 (values spec '#'identity)))
             (expand (specs)
               "Expand `specs` into the body code of the new predicate."
               (destructuring-bind (spec . remaining) specs
                 (multiple-value-bind (predicate key) (parse-spec spec)
                   (once-only (predicate key)
                     (with-gensyms (kx ky)
                       `(let ((,kx (funcall ,key ,x))
                              (,ky (funcall ,key ,y)))
                          ,(if (null remaining)
                             `(if (funcall ,predicate ,kx ,ky)
                                t
                                nil)
                             `(cond
                                ((funcall ,predicate ,kx ,ky) t)
                                ((funcall ,predicate ,ky ,kx) nil)
                                (t ,(expand remaining)))))))))))
      `(defun ,name (,x ,y)
         ,(expand (cons predicate-spec more-predicate-specs))))))


