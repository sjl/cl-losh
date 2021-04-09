(in-package :losh.ring-buffers)

;;;; Data Structure -----------------------------------------------------------
(defstruct (ring-buffer (:constructor make-ring-buffer%)
                        (:conc-name nil))
  (data (error "Data is required.") :type simple-vector)
  (r 0 :type fixnum)
  (w 0 :type fixnum))

(defun make-ring-buffer (&key (size 64))
  "Create and return a fresh ring buffer able to hold `(1- size)` elements."
  (check-type size (and (integer 2) fixnum))
  (make-ring-buffer% :data (make-array (list size) :initial-element nil)))


;;;; Operations ---------------------------------------------------------------
(defun-inline size (ring-buffer)
  (length (data ring-buffer)))

(defun rb-size (ring-buffer)
  "Return the size of `ring-buffer`.

  A ring buffer can hold at most `(1- (rb-size ring-buffer))` elements.

  "
  (size ring-buffer))

(defun rb-count (ring-buffer)
  "Return the number of elements currently stored in `ring-buffer`."
  (mod (- (w ring-buffer) (r ring-buffer))
       (size ring-buffer)))


(defmacro 1+mod ((field ring-buffer))
  (once-only (ring-buffer)
    (with-gensyms (result)
      `(let ((,result (1+ (,field ,ring-buffer))))
         (if (= (size ,ring-buffer) ,result)
           0
           ,result)))))

(defmacro 1+modf ((field ring-buffer))
  (once-only (ring-buffer)
    `(setf (,field ,ring-buffer) (1+mod (,field ,ring-buffer)))))


(defun rb-full-p (ring-buffer)
  "Return whether `ring-buffer` is full."
  (= (1+mod (w ring-buffer))
     (r ring-buffer)))

(defun rb-empty-p (ring-buffer)
  "Return whether `ring-buffer` is empty."
  (= (w ring-buffer) (r ring-buffer)))


(defun rb-push (ring-buffer object)
  "Push `object` into `ring-buffer`.

  If `ring-buffer` is full, its oldest element will be silently dropped.  If you
  want an error to be signaled instead, use `rb-safe-push`.

  "
  (setf (svref (data ring-buffer) (w ring-buffer)) object)
  (let ((w (1+mod (w ring-buffer))))
    (setf (w ring-buffer) w)
    (when (= w (r ring-buffer))
      (setf (svref (data ring-buffer) w) nil)
      (1+modf (r ring-buffer))))
  object)

(defun rb-safe-push (ring-buffer object)
  "Push `object` into `ring-buffer`, or signal an error if it is already full."
  (assert (not (rb-full-p ring-buffer)) ()
    "Cannot safely push ~S to a full ring buffer ~S." object ring-buffer)
  (setf (svref (data ring-buffer) (w ring-buffer)) object)
  (1+modf (w ring-buffer))
  object)


(defun-inline pop% (vector index)
  (prog1 (svref vector index)
    (setf (svref vector index) nil)))

(defun rb-pop (ring-buffer)
  "Remove and return the oldest element of `ring-buffer`, or signal an error if it is empty."
  (if (rb-empty-p ring-buffer)
    (error "Cannot pop from empty ring buffer ~S." ring-buffer)
    (prog1 (pop% (data ring-buffer) (r ring-buffer))
      (1+modf (r ring-buffer)))))


(defun-inline bad-index (ring-buffer index)
  (error "Invalid index ~D for ring buffer with ~D element~:P."
         index (rb-count ring-buffer)))

(defun-inline compute-index (ring-buffer index)
  (if (minusp index)
    (if (< index (- (rb-count ring-buffer)))
      (bad-index ring-buffer index)
      (mod (+ index (w ring-buffer)) (size ring-buffer)))
    (if (>= index (rb-count ring-buffer))
      (bad-index ring-buffer index)
      (mod (+ index (r ring-buffer)) (size ring-buffer)))))

(defun rb-ref (ring-buffer index)
  "Return the element of `ring-buffer` at `index`.

  Elements are indexed oldest to newest: element 0 is the oldest element in the
  ring buffer, element 1 is the second oldest, and so on.

  Negative indices are supported: element -1 is the newest element, element -2
  the second newest, and so on.

  An error will be signaled if `index` is out of range.

  "
  (svref (data ring-buffer) (compute-index ring-buffer index)))


;;;; Iteration ----------------------------------------------------------------
(defmacro do-ring-buffer ((symbol ring-buffer) &body body)
  "Iterate over `ring-buffer`, executing `body` with `symbol` bound to each element.

  Elements are walked oldest to newest.

  "
  (with-gensyms (r w d s)
    (once-only (ring-buffer)
      `(do ((,r (r ,ring-buffer))
            (,w (w ,ring-buffer))
            (,d (data ,ring-buffer))
            (,s (size ,ring-buffer)))
         ((= ,r ,w))
         (let ((,symbol (svref ,d ,r)))
           ,@body)
         (incf ,r)
         (when (= ,r ,s) (setf ,r 0))))))

(defun rb-contents (ring-buffer &key (result-type 'list))
  "Return a fresh sequence of the contents of `ring-buffer` (oldest to newest).

  `result-type` can currently only be `list`.  TODO: add `vector`.

  "
  (ecase result-type
    (list (loop :with r = (r ring-buffer)
                :with w = (w ring-buffer)
                :with d = (data ring-buffer)
                :with s = (size ring-buffer)
                :until (= r w)
                :collect (svref d r)
                :do (incf r)
                :when (= r s) :do (setf r 0)))))


;;;; Printing -----------------------------------------------------------------
(defvar *debug-ring-buffers* nil)

(defmethod print-object ((o ring-buffer) s)
  (print-unreadable-object (o s :type t :identity t)
    (if *debug-ring-buffers*
      (format s "~D/~D contents ~:S array [~A]"
              (rb-count o) (size o) (rb-contents o)
              (with-output-to-string (s)
                (loop :with r = (r o)
                      :with w = (w o)
                      :for i :from 0
                      :for el :across (data o)
                      :unless (zerop i) :do (princ #\space s)
                      :do (format s (cond
                                      ((= i r w) "{R+W ~A}")
                                      ((= i r) "{R ~A}")
                                      ((= i w) "{W ~A}")
                                      (t "~A"))
                                  el))))
      (format s "~D/~D" (rb-count o) (size o)))))
