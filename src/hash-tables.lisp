(in-package :losh.hash-tables)

(defun mutate-hash-values (function hash-table)
  "Replace each value in `hash-table` with the result of calling `function` on it.

  Returns the hash table.

  "
  (iterate (for (key value) :in-hashtable hash-table)
           (setf (gethash key hash-table)
                 (funcall function value)))
  hash-table)

(defun hash-table-contents (hash-table)
  "Return a fresh list of `(key value)` elements of `hash-table`."
  (gathering (maphash (compose #'gather #'list) hash-table)))

(defun remhash-if (test hash-table)
  "Remove elements which satisfy `(test key value)` from `hash-table`.

  Returns the hash table."
  (maphash (lambda (k v)
             (when (funcall test k v)
               (remhash k hash-table)))
           hash-table)
  hash-table)

(defun remhash-if-not (test hash-table)
  "Remove elements which don't satisfy `(test key value)` from `hash-table`.

  Returns the hash table."
  (maphash (lambda (k v)
             (unless (funcall test k v)
               (remhash k hash-table)))
           hash-table)
  hash-table)

(defun remhash-if-key (test hash-table)
  "Remove elements which satisfy `(test key)` from `hash-table`.

  Returns the hash table."
  (maphash (lambda (k v)
             (declare (ignore v))
             (when (funcall test k)
               (remhash k hash-table)))
           hash-table)
  hash-table)

(defun remhash-if-not-key (test hash-table)
  "Remove elements which satisfy don't `(test key)` from `hash-table`.

  Returns the hash table."
  (maphash (lambda (k v)
             (declare (ignore v))
             (unless (funcall test k)
               (remhash k hash-table)))
           hash-table)
  hash-table)

(defun remhash-if-value (test hash-table)
  "Remove elements which satisfy `(test value)` from `hash-table`.

  Returns the hash table."
  (maphash (lambda (k v)
             (when (funcall test v)
               (remhash k hash-table)))
           hash-table)
  hash-table)

(defun remhash-if-not-value (test hash-table)
  "Remove elements which satisfy don't `(test value)` from `hash-table`.

  Returns the hash table."
  (maphash (lambda (k v)
             (unless (funcall test v)
               (remhash k hash-table)))
           hash-table)
  hash-table)
