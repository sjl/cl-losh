(in-package :losh.sequences)

(deftype array-index (&optional (length (1- array-dimension-limit)))
  "An integer in the range `[0, length)`.

  From Alexandria.

  "
  `(integer 0 (,length)))


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


(defun-inlineable summation (sequence &key key)
  "Return the sum of all elements of `sequence`.

  If `key` is given, it will be called on each element to compute the addend.

  This function's ugly name was chosen so it wouldn't clash with iterate's `sum`
  symbol.  Sorry.

  Examples:

    (sum #(1 2 3))
    ; => 6

    (sum '(\"1\" \"2\" \"3\") :key #'parse-integer)
    ; => 6

    (sum '(\"1\" \"2\" \"3\") :key #'length)
    ; => 3

  "
  (if key
    (iterate (for n :in-whatever sequence)
             (sum (funcall key n)))
    (iterate (for n :in-whatever sequence)
             (sum n))))

(defun-inlineable product (sequence &key key)
  "Return the product of all elements of `sequence`.

  If `key` is given, it will be called on each element to compute the
  multiplicand.

  Examples:

    (product #(1 2 3))
    ; => 6

    (product '(\"1\" \"2\" \"3\") :key #'parse-integer)
    ; => 6

    (product '(\"1\" \"2\" \"3\") :key #'length)
    ; => 1

  "
  (if key
    (iterate (for n :in-whatever sequence)
             (multiplying (funcall key n)))
    (iterate (for n :in-whatever sequence)
             (multiplying n))))


(defmacro doseq ((var sequence) &body body)
  "Perform `body` with `var` bound to each element in `sequence` in turn.

  It's like `cl:dolist`, but for all sequences.

  "
  `(map nil (lambda (,var) ,@body) ,sequence))


