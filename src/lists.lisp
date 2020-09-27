(in-package :losh.lists)

(defun somelist (predicate list &rest more-lists)
  "Call `predicate` on successive sublists of `list`, returning the first true result.

  `somelist` is to `some` as `maplist` is to `mapcar`.

  "
  (if more-lists
    (iterate
      (for lists :first (cons list more-lists) :then (mapcar #'cdr lists))
      (until (some #'null lists))
      (thereis (apply predicate lists)))
    (iterate (for l :on list)
             (thereis (funcall predicate l)))))


(defun 0.. (below)
  "Return a fresh list of the range `[0, below)`."
  (loop :for i :from 0 :below below :collect i))

(defun 1.. (below)
  "Return a fresh list of the range `[1, below)`."
  (loop :for i :from 1 :below below :collect i))

(defun n.. (from below)
  "Return a fresh list of the range `[from, below)`."
  (loop :for i :from from :below below :collect i))

(defun 0... (to)
  "Return a fresh list of the range `[0, to]`."
  (loop :for i :from 0 :to to :collect i))

(defun 1... (to)
  "Return a fresh list of the range `[1, to]`."
  (loop :for i :from 1 :to to :collect i))

(defun n... (from to)
  "Return a fresh list of the range `[from, to]`."
  (loop :for i :from from :to to :collect i))
