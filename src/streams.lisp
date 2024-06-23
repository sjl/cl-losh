(in-package :losh.streams)

(defmacro with-eof-handled ((stream eof-error-p eof-value) &body body)
  (alexandria:once-only (stream eof-error-p eof-value)
    `(if (null (peek-char nil ,stream nil))
       (if ,eof-error-p
         (error 'end-of-file)
         ,eof-value)
       (progn ,@body))))
