(in-package :losh.queues)

;;; Based on the PAIP queues (thanks, Norvig), but beefed up a little bit to add
;;; tracking of the queue size.

(defstruct (queue (:constructor make-queue%))
  (contents nil :type list)
  (last nil :type list)
  (size 0 :type fixnum))


(declaim
  (ftype (function (&key (:initial-contents list))
                   (values queue &optional))
         make-queue)
  (ftype (function (queue)
                   (values boolean &optional))
         queue-empty-p)
  (ftype (function (t queue)
                   (values fixnum &optional))
         enqueue)
  (ftype (function (queue)
                   (values t &optional))
         dequeue)
  (ftype (function (queue list)
                   (values fixnum &optional))
         queue-append))


(defun-inlineable make-queue (&key initial-contents)
  "Allocate and return a fresh queue."
  (let ((queue (make-queue%)))
    (when initial-contents
      (queue-append queue initial-contents))
    queue))

(defun-inlineable queue-empty-p (queue)
  "Return whether `queue` is empty."
  (zerop (queue-size queue)))

(defun-inlineable enqueue (item queue)
  "Enqueue `item` in `queue`, returning the new size of the queue."
  (let ((cell (cons item nil)))
    (if (queue-empty-p queue)
      (setf (queue-contents queue) cell)
      (setf (cdr (queue-last queue)) cell))
    (setf (queue-last queue) cell))
  (incf (queue-size queue)))

(defun-inlineable dequeue (queue)
  "Dequeue an item from `queue` and return it."
  (when (zerop (decf (queue-size queue)))
    (setf (queue-last queue) nil))
  (pop (queue-contents queue)))

(defun-inlineable queue-append (queue list)
  "Enqueue each element of `list` in `queue` and return the queue's final size."
  (loop :for item :in list
        :for size = (enqueue item queue)
        :finally (return size)))

