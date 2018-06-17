(in-package :losh.random)

(defun-inline epsilon (val)
  (etypecase val
    (integer 1)
    (short-float short-float-epsilon)
    (long-float long-float-epsilon)
    (single-float single-float-epsilon)
    (double-float double-float-epsilon)))


(defun-inlineable randomp (&optional (chance 0.5) (generator #'random))
  "Return a random boolean with `chance` probability of `t`."
  (< (funcall generator 1.0) chance))

(defun random-elt (seq &optional (generator #'random))
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
      (values (elt seq (funcall generator length)) t))))

(defun-inlineable random-range (min max &optional (generator #'random))
  "Return a random number in [`min`, `max`)."
  (+ min (funcall generator (- max min))))

(defun-inlineable random-range-inclusive (min max &optional (generator #'random))
  "Return a random number in [`min`, `max`]."
  (+ min (funcall generator (+ (- max min) (epsilon min)))))

(defun-inlineable random-range-exclusive (min max  &optional (generator #'random))
  "Return a random number in (`min`, `max`)."
  (+ (epsilon min) min (funcall generator (- max min (epsilon min)))))

(defun-inlineable random-around (value spread &optional (generator #'random))
  "Return a random number within `spread` of `value` (inclusive)."
  (random-range-inclusive (- value spread)
                          (+ value spread)
                          generator))


(let (spare)
  (defun clear-gaussian-spare ()
    (setf spare nil))
  (defun random-gaussian (mean standard-deviation &optional (generator #'random))
    "Return a random float from a gaussian distribution.  NOT THREAD-SAFE (yet)!"
    ;; https://en.wikipedia.org/wiki/Marsaglia_polar_method
    (declare (optimize (speed 3))
             (inline random-range))
    (let ((mean (coerce mean 'single-float))
          (standard-deviation (coerce standard-deviation 'single-float)))
      (flet ((scale (n)
               (+ mean (* n standard-deviation))))
        (if spare
          (prog1
              (scale (the single-float spare))
            (setf spare nil))
          (loop
            :for u :of-type single-float = (+ -1.0 (the single-float (funcall generator 2.0)))
            :for v :of-type single-float = (+ -1.0 (the single-float (funcall generator 2.0)))
            :for s :of-type single-float = (+ (square u) (square v))
            :while (or (>= s 1.0) (= s 0.0))
            :finally
            (setf s (sqrt (/ (* -2.0 (the (single-float * (0.0)) (log s)))
                             s))
                  spare (* v s))
            (return (scale (* u s)))))))))

(defun random-gaussian-integer (mean standard-deviation &optional (generator #'random))
  "Return a random integer from a gaussian distribution.  NOT THREAD-SAFE (yet)!"
  (values (round (random-gaussian mean standard-deviation generator))))


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

