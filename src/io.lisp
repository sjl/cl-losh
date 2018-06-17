(in-package :losh.io)

(defun read-all-from-string (string)
  "Read all forms from `string` and return them as a fresh list."
  (iterate
    (with done = (gensym))
    (with start = 0)
    (for (values form pos) = (read-from-string string nil done
                                               :start start))
    (while (not (eq form done)))
    (collect form)
    (setf start pos)))

(defun read-all-from-file (path)
  "Read all forms from the file at `path` and return them as a fresh list."
  (with-open-file (file path :direction :input)
    (iterate
      (with done = (gensym))
      (for form = (read file nil done))
      (while (not (eq form done)))
      (collect form))))


