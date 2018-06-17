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

