(in-package :losh.test)


(defmacro define-test (name &body body)
  `(test ,(intern (concatenate 'string (symbol-name 'test-) (symbol-name name)))
    (let ((*package* ,*package*))
      ,@body)))

(defun run-tests ()
  (1am:run))

