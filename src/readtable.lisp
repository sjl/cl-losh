(in-package :losh.readtable)


(defun sharp-semicolon-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (loop :while (read-line stream nil nil))
  (values))

(defun shebang-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (read-line stream)
  (values))

(named-readtables:defreadtable losh
  (:merge :standard losh.hash-tables::hash-table-constructor-syntax)
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# #\; #'sharp-semicolon-reader)
  (:dispatch-macro-char #\# #\! #'shebang-reader))
