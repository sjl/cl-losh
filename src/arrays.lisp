(in-package :losh.arrays)

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


(defun-inlineable bisect-left (predicate vector target &key
                               (key #'identity)
                               (start 0)
                               (end (length vector)))
  "Bisect `vector` with `predicate` and return the LEFT element.

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

  "
  (if (>= start end)
    (values nil nil)
    (iterate
      (for index = (truncate (+ start end) 2))
      (for value = (aref vector index))
      (for result = (funcall predicate (funcall key value) target))
      (if (= start index)
        (return (if result
                  (values value index)
                  (values nil nil)))
        (if result
          (setf start index)
          (setf end index))))))

(defun-inlineable bisect-right (predicate vector target &key
                                (key #'identity)
                                (start 0)
                                (end (length vector)))
  "Bisect `vector` with `predicate` and return the RIGHT element.

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

  "
  (if (>= start end)
    (values nil nil)
    (iterate
      (with bottom = (1- start))
      (with top = (1- end))
      (for index = (ceiling (+ bottom top) 2))
      (for value = (aref vector index))
      (for result = (funcall predicate (funcall key value) target))
      (if (= top index)
        (return (if result
                  (values nil nil)
                  (values value index)))
        (if result
          (setf bottom index)
          (setf top index))))))


(defun vector-last (vector)
  "Return the last element of `vector`, or `nil` if it is empty.

  A second value is returned, which will be `t` if the vector was not empty and
  `nil` if it was.

  The vector's fill-pointer will be respected.

  "
  (let ((length (length vector)))
    (if (zerop length)
      (values nil nil)
      (values (aref vector (1- length)) t))))
