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


(declaim (inline assocar assocdr rassocar rassocdr
                 (setf assocar) (setf assocdr) (setf rassocar) (setf rassocdr)))


(defun assocar (item alist &rest args)
  "Return the `car` of `(apply #'assoc item alist args)`."
  (car (apply #'assoc item alist args)))

(defun assocdr (item alist &rest args)
  "Return the `cdr` of `(apply #'assoc item alist args)`."
  (cdr (apply #'assoc item alist args)))

(defun rassocar (item alist &rest args)
  "Return the `car` of `(apply #'rassoc item alist args)`."
  (car (apply #'rassoc item alist args)))

(defun rassocdr (item alist &rest args)
  "Return the `cdr` of `(apply #'rassoc item alist args)`."
  (cdr (apply #'rassoc item alist args)))


(defun (setf assocar) (new-value item alist &rest args)
  "Set the `car` of `(apply #'assoc item alist args)` to `new-value`."
  (setf (car (apply #'assoc item alist args)) new-value))

(defun (setf assocdr) (new-value item alist &rest args)
  "Set the `cdr` of `(apply #'assoc item alist args)` to `new-value`."
  (setf (cdr (apply #'assoc item alist args)) new-value))

(defun (setf rassocar) (new-value item alist &rest args)
  "Set the `car` of `(apply #'rassoc item alist args)` to `new-value`."
  (setf (car (apply #'rassoc item alist args)) new-value))

(defun (setf rassocdr) (new-value item alist &rest args)
  "Set the `cdr` of `(apply #'rassoc item alist args)` to `new-value`."
  (setf (cdr (apply #'rassoc item alist args)) new-value))
