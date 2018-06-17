(in-package :losh.chili-dogs)

(defmacro defun-inlineable (name &body body)
  "Like `defun-inline`, but declaims `name` to be `notinline` afterword.

  This is useful when you don't want to inline a function everywhere, but *do*
  want to have the ability to inline it on demand with (declare (inline ...)).

  "
  `(progn
     (declaim (inline ,name))
     (defun ,name ,@body)
     (declaim (notinline ,name))
     ',name))

(defmacro defun-inline (name &body body)
  "Like `defun`, but declaims `name` to be `inline`."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,@body)
     ',name))

