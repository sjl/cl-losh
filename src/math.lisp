(in-package :losh.math)


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
  "Return whether `n` is evenly divisible by `divisor`.

  The value returned will be the quotient when true, `nil` otherwise.

  "
  (multiple-value-bind (quotient remainder) (floor n divisor)
    (when (zerop remainder)
      quotient)))


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
  (_ integer
    (floor _ (expt base position))
    (mod _ base)))

