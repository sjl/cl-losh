(in-package :losh.priority-queues)

(defstruct (priority-queue (:conc-name pq-)
                           (:constructor make-priority-queue%))
  (contents nil)
  (predicate #'<)
  (test #'eql))


(defun make-priority-queue (&key (priority-predicate #'<) (element-test #'eql))
  "Create and return a fresh priority queue.

  `priority-predicate` is the comparison function used to compare priorities,
  and should be a `<`-like predicate.

  `element-test` should be the equality predicate for elements.

  "
  (make-priority-queue% :predicate priority-predicate :test element-test))


(defmethod print-object ((object priority-queue) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (prin1 (pq-contents object) stream)))


(defun pqn-priority (node)
  (first node))

(defun pqn-element (node)
  (second node))

(defun pq-resort (pq)
  (zapf (pq-contents pq)
        (sort % (pq-predicate pq) :key #'pqn-priority))
  pq)

(defun pq-lookup (pq element)
  (find element (pq-contents pq)
        :key #'pqn-element
        :test (pq-test pq)))


(defun pq-insert (pq element priority)
  "Insert `element` into `pq` with `priority`.

  Returns `pq` (which has been modified).

  "
  (zapf (pq-contents pq)
        (merge 'list `((,priority ,element)) % (pq-predicate pq)
               :key #'pqn-priority))
  pq)

(defun pq-ensure (pq element priority)
  "Ensure `element` is in `pq` with `priority`.

  If `element` is already in `pq` its priority will be set to `priority`.
  Otherwise it will be inserted as if by calling `pq-insert`.

  Returns `pq` (which may have been modified).

  "
  (let ((existing (pq-lookup pq element)))
    (if existing
      (progn (setf (car existing) priority)
             (pq-resort pq))
      (pq-insert pq element priority)))
  pq)


(defun pq-dequeue (pq)
  "Remove and return the element in `pq` with the lowest-numbered priority.

  If `pq` is empty `nil` will be returned.

  A second value is also returned, which will be `t` if an element was present
  or `nil` if the priority queue was empty.

  "
  (if (pq-contents pq)
    (values (pqn-element (pop (pq-contents pq))) t)
    (values nil nil)))


