(in-package :losh.weightlists)

(defstruct (weightlist (:constructor %make-weightlist))
  weights sums items total)

(defun make-weightlist (weights-and-items)
  "Make a weightlist of the given items and weights.

  `weights-and-items` should be an alist of `(weight . item)` pairs.

  Weights can be any `real` numbers.  Weights of zero are fine, as long as at
  least one of the weights is nonzero (otherwise there's nothing to choose).

  "
  (let ((weights (mapcar #'car weights-and-items))
        (items (mapcar #'cdr weights-and-items)))
    (%make-weightlist
      :items items
      :weights weights
      :sums (prefix-sums weights)
      :total (apply #'+ weights))))

(defun weightlist-random (weightlist)
  "Return a random item from the weightlist, taking the weights into account."
  (iterate
    (with n = (random (weightlist-total weightlist)))
    (for item :in (weightlist-items weightlist))
    (for weight :in (weightlist-sums weightlist))
    (finding item :such-that (< n weight))))


