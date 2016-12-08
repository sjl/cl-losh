(defpackage :losh.internal
  (:use :cl))

(in-package :losh.internal)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun external-symbols (package)
    (let ((symbols nil))
      (do-external-symbols (s (find-package package) symbols)
        (push s symbols)))))

(defmacro defpackage-inheriting (name parent-packages &rest args)
  `(defpackage ,name
     ,@args
     ,@(loop :for parent-package :in parent-packages
             :collect
             `(:use ,parent-package)
             :collect
             `(:export ,@(external-symbols parent-package)))))


(defpackage :losh.math
  (:documentation "Utilities related to math and numbers.")
  (:export
    :tau
    :tau/2
    :1/2tau
    :tau/4
    :1/4tau
    :2/4tau
    :3/4tau
    :tau/8
    :1/8tau
    :2/8tau
    :3/8tau
    :4/8tau
    :5/8tau
    :6/8tau
    :7/8tau

    :radians
    :degrees
    :square
    :dividesp
    :norm
    :lerp
    :precise-lerp
    :map-range
    :clamp))

(defpackage :losh.random
  (:documentation "Utilities related to randomness.")
  (:export
    :randomp
    :random-elt
    :random-range
    :random-range-exclusive
    :random-around
    :random-gaussian
    :random-gaussian-integer
    :d))

(defpackage :losh.functions
  (:documentation "Utilities for working with higher-order functions.")
  (:export
    :juxt
    :nullary
    :fixed-point))

(defpackage :losh.control-flow
  (:documentation "Utilities for managing control flow.")
  (:export
    :recursively
    :recur
    :when-found
    :if-found
    :gathering
    :gather
    :when-let*))

(defpackage :losh.mutation
  (:documentation "Utilities for mutating places in-place.")
  (:export
    :zapf
    :%
    :mulf
    :divf
    :modf
    :remainderf
    :clampf
    :negatef
    :notf
    :callf))

(defpackage :losh.lists
  (:documentation "Utilities related to lists.")
  (:export
    :take))

(defpackage :losh.arrays
  (:documentation "Utilities related to arrays.")
  (:export
    :do-array
    :fill-multidimensional-array
    :fill-multidimensional-array-t
    :fill-multidimensional-array-fixnum
    :fill-multidimensional-array-single-float))

(defpackage :losh.queues
  (:documentation "A simple queue implementation.")
  (:export
    :queue
    :make-queue
    :queue-contents
    :queue-size
    :queue-empty-p
    :enqueue
    :dequeue
    :queue-append))

(defpackage :losh.iterate
  (:use :iterate) ; need this for iterate's `for` symbol fuckery
  (:documentation "Custom `iterate` drivers and clauses.")
  (:export
    :across-flat-array
    :averaging
    :cycling
    :every-nth
    :for-nested
    :in-array
    :in-lists
    :in-sequences
    :in-whatever
    :index-of-flat-array
    :into
    :macroexpand-iterate
    :modulo
    :pairs-of-list
    :per-iteration-into
    :real-time
    :run-time
    :since-start-into
    :skip-origin
    :timing
    :within-radius
    ))

(defpackage :losh.hash-tables
  (:documentation "Utilities for operating on hash tables.")
  (:export
    :mutate-hash-values))

(defpackage :losh.hash-sets
  (:documentation "Simple hash set implementation.")
  (:export
    :hash-set
    :make-hash-set
    :copy-hash-set

    :hset-empty-p
    :hset-contains-p
    :hset-elements
    :hset-count

    :hset-insert!
    :hset-remove!
    :hset-pop!
    :hset-clear!

    :hset=

    :hset-union
    :hset-union!
    :hset-intersection
    :hset-intersection!
    :hset-difference
    :hset-difference!
    :hset-filter
    :hset-filter!
    :hset-map
    :hset-map!))

(defpackage :losh.sequences
  (:documentation "Utilities for operating on sequences.")
  (:export
    :prefix-sums
    :frequencies
    :proportions
    :group-by))

(defpackage :losh.debugging
  (:documentation "Utilities for figuring out what the hell is going on.")
  (:export
    :pr
    :prl
    :bits
    :shut-up
    :dis
    :comment
    :aesthetic-string
    :structural-string
    #+sbcl :start-profiling
    #+sbcl :stop-profiling
    :print-table
    :print-hash-table))

(defpackage :losh.weightlists
  (:documentation
    "A simple data structure for choosing random items with weighted probabilities.")
  (:export
    :weightlist
    :weightlist-weights
    :weightlist-items
    :make-weightlist
    :weightlist-random))

(defpackage :losh.licensing
  (:documentation "Utilities related to open source licenses.")
  (:export
    :print-licenses))

(defpackage :losh.eldritch-horrors
  (:documentation "Abandon all hope, ye who enter here.")
  (:export
    :dlambda
    :define-with-macro))


(defpackage-inheriting :losh
  (:losh.arrays

   :losh.control-flow
   :losh.debugging
   :losh.eldritch-horrors
   :losh.functions
   :losh.hash-sets
   :losh.hash-tables
   :losh.iterate
   :losh.licensing
   :losh.lists
   :losh.math
   :losh.mutation
   :losh.queues
   :losh.random
   :losh.sequences
   :losh.weightlists

   )
  (:use
    :cl
    :iterate
    :losh.quickutils)
  (:documentation
    "This package exports all of the symbols in the other packages.

  If you just want to get everything you can `:use` this one and be done with
  it.  Otherwise you can `:use` only the ones you need.

  "))


;;;; Remember to add it to the docs!
