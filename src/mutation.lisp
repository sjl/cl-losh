(in-package :losh.mutation)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun build-zap (place expr env)
    (multiple-value-bind (temps exprs stores store-expr access-expr)
        (get-setf-expansion place env)
      `(let* (,@(mapcar #'list temps exprs)
              (,(car stores) (symbol-macrolet ((% ,access-expr))
                               ,expr)))
         ,store-expr))))

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
    ,@(loop :for (place expr . nil) :on place-expr-pairs :by #'cddr
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
     ,@(loop :for (place function . nil) :on place-function-pairs :by #'cddr
             :collect `(%callf ,place ,function))))

