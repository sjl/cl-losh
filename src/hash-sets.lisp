(in-package :losh.hash-sets)

(defstruct (hash-set (:constructor make-hash-set%)
                     (:copier nil))
  (storage (error "Required") :type hash-table))

(defmethod print-object ((hset hash-set) stream)
  (print-unreadable-object (hset stream :type t :identity t)
    (format stream "~:S" (hset-elements hset))))


(defun make-hash-set (&key (test 'eql) (size 16) (initial-contents '()))
  "Create a fresh hash set.

  `size` should be a hint as to how many elements this set is expected to
  contain.

  `initial-contents` should be a sequence of initial elements for the set
  (duplicates are fine).

  "
  (let* ((result (make-hash-set% :storage (make-hash-table :test test
                                                           :size size))))
    (map nil (curry #'hset-insert! result) initial-contents)
    result))

(defun copy-hash-set (hset)
  "Create a (shallow) copy of the given hash set.

  Only the storage for the hash set itself will be copied -- the elements
  themselves will not be copied.

  "
  (make-hash-set% :storage (copy-hash-table (hash-set-storage hset))))


(defmacro define-hset-op (name arglist &body body)
  (let* ((has-docstring (stringp (first body)))
         (docstring (if has-docstring
                      (first body)
                      ""))
         (body (if has-docstring
                 (rest body)
                 body)))
    `(defun ,name ,arglist
       ,docstring
       (symbol-macrolet ((storage (hash-set-storage ,(first arglist))))
        ,@body))))


(define-hset-op hset-empty-p (hset)
  "Return whether `hset` is empty."
  (zerop (hash-table-count storage)))

(define-hset-op hset-contains-p (hset element)
  "Return whether `hset` contains `element`."
  (values (gethash element storage)))

(define-hset-op hset-count (hset)
  "Return the number of elements in `hset`."
  (hash-table-count storage))

(define-hset-op hset-insert! (hset &rest elements)
  "Insert each element in `elements` into `hset`.

  Returns nothing.

  "
  (dolist (element elements)
    (setf (gethash element storage) t))
  (values))

(define-hset-op hset-remove! (hset &rest elements)
  "Remove each element in `elements` from `hset`.

  If an element is not in `hset`, it will be ignored.

  Returns nothing.

  "
  (dolist (element elements)
    (remhash element storage))
  (values))

(define-hset-op hset-pop! (hset)
  "Remove and return an arbitrarily-chosen element from `hset`.

  An error will be signaled if the hash set is empty.

  "
  (assert (not (hset-empty-p hset))
      (hset)
    "Cannot pop from empty hash set ~S"
    hset)
  (iterate (for (k nil) :in-hashtable storage)
           (remhash k storage)
           (return k)))

(define-hset-op hset-clear! (hset)
  "Remove all elements from `hset`.

  Returns nothing.

  "
  (clrhash storage)
  (values))


(define-hset-op hset=% (hset other)
  (iterate (for (k nil) :in-hashtable storage)
           (when (not (hset-contains-p other k))
             (return nil))
           (finally (return t))))

(define-hset-op hset= (hset &rest others)
  "Return whether all the given hash sets contain exactly the same elements.

  All the hash sets are assumed to use the same `test` -- the consequences are
  undefined if this is not the case.

  "
  (if (apply #'/= (hset-count hset) (mapcar #'hset-count others))
    nil
    (iterate (for other :in others)
             (when (not (hset=% hset other))
               (return nil))
             (finally (return t)))))


(define-hset-op hset-union!% (hset other)
  (iterate (for (k nil) :in-hashtable (hash-set-storage other))
           (hset-insert! hset k))
  hset)

(define-hset-op hset-union! (hset &rest others)
  "Destructively update `hset` to contain the union of itself with `others`."
  (reduce #'hset-union!% others :initial-value hset))

(define-hset-op hset-union (hset &rest others)
  "Return a fresh hash set containing the union of the given hash sets."
  (apply #'hset-union! (copy-hash-set hset) others))


(define-hset-op hset-intersection!% (hset other)
  (iterate (for (k nil) :in-hashtable storage)
           (when (not (hset-contains-p other k))
             (remhash k storage)))
  hset)

(define-hset-op hset-intersection! (hset &rest others)
  "Destructively update `hset` to contain the intersection of itself with `others`."
  (reduce #'hset-intersection!% others :initial-value hset))

(define-hset-op hset-intersection (hset &rest others)
  "Return a fresh hash set containing the intersection of the given hash sets."
  (apply #'hset-intersection! (copy-hash-set hset) others))


(define-hset-op hset-difference!% (hset other)
  (iterate (for (k nil) :in-hashtable (hash-set-storage other))
           (remhash k storage))
  hset)

(define-hset-op hset-difference! (hset &rest others)
  "Destructively update `hset` to contain the difference of itself with `others`."
  (reduce #'hset-difference!% others :initial-value hset))

(define-hset-op hset-difference (hset &rest others)
  "Return a fresh hash set containing the difference of the given hash sets."
  (apply #'hset-difference! (copy-hash-set hset) others))


(define-hset-op hset-filter! (hset predicate)
  "Destructively update `hset` to contain only elements satisfying `predicate`."
  (iterate (for (k nil) :in-hashtable storage)
           (when (funcall predicate k)
             (remhash k storage))))

(define-hset-op hset-filter (hset predicate)
  "Return a fresh hash set containing elements of `hset` satisfying `predicate`."
  (let ((new (copy-hash-set hset)))
    (hset-filter! new predicate)
    new))


(define-hset-op hset-map! (hset function &key new-test)
  "Destructively update `hset` by calling `function` on each element.

   If `new-test` is given the hash set's `test` will be updated.

   "
  (let ((results (iterate (for (k nil) :in-hashtable storage)
                          (collect (funcall function k)))))
    (if new-test
      ;; Rebuild the underlying hash table if we have a new test.
      (setf storage (make-hash-table :test new-test
                                     :size (hash-table-count storage)))
      ;; Otherwise just clear and reuse the existing one.
      (clrhash storage))
    (dolist (k results)
      (hset-insert! hset k))
    nil))

(define-hset-op hset-map (hset function &key new-test)
  "Return a fresh hash set containing the results of calling `function` on elements of `hset`.

  If `new-test` is given, the new hash set will use this as its `test`.

  "
  (let ((new (copy-hash-set hset)))
    (hset-map! new function :new-test new-test)
    new))


(define-hset-op hset-reduce (hset function &key (initial-value nil ivp))
  "Reduce `function` over the elements of `hset`.

  The order in which the elements are processed is undefined.

  "
  (if ivp
    (iterate (for (n nil) :in-hashtable storage)
             (reducing n by function :initial-value initial-value))
    (iterate (for (n nil) :in-hashtable storage)
             (reducing n by function))))

(define-hset-op hset-elements (hset)
  "Return a fresh list containing the elements of `hset`."
  (hash-table-keys storage))


