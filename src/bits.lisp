(in-package :losh.bits)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *signed-add-docstring-template*
    "Perform ~D-bit signed addition of `x` and `y`.

  Returns two values: the result and a boolean specifying whether
  underflow/overflow occurred.

  ")

  (defparameter *signed-sub-docstring-template*
    "Perform ~D-bit signed subtraction of `x` and `y`.

  Returns two values: the result and a boolean specifying whether
  underflow/overflow occurred.

  "))

(macrolet
    ((define-ops (size)
       (let ((overflow (symb 'overflow- size)))
         `(progn
           (defun-inline ,overflow (value)
             (cond ((> value (1- (expt 2 ,(1- size))))
                    (values (- value (expt 2 ,size)) t))
                   ((< value (- (expt 2 ,(1- size))))
                    (values (+ value (expt 2 ,size)) t))
                   (t (values value nil))))

           (defun-inlineable ,(symb '+/ size) (x y)
             ,(format nil *signed-add-docstring-template* size)
             (declare (optimize speed)
                      (type (signed-byte ,size) x y))
             (,overflow (+ x y)))

           (defun-inlineable ,(symb '-/ size) (x y)
             ,(format nil *signed-sub-docstring-template* size)
             (declare (optimize speed)
                      (type (signed-byte ,size) x y))
             (,overflow (- x y)))))))
  (define-ops 8)
  (define-ops 16)
  (define-ops 32)
  (define-ops 64))

