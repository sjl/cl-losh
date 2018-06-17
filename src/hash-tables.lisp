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

