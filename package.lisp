(defpackage #:losh.internal
  (:use #:cl))

(in-package #:losh.internal)

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




(defpackage #:losh.math
  (:documentation "Utilities related to math and numbers.")
  (:export
    #:tau
    #:square
    #:dividesp
    #:norm
    #:lerp
    #:precise-lerp
    #:map-range
    #:clamp))

(defpackage #:losh.random
  (:documentation "Utilities related to randomness.")
  (:export
    #:randomp
    #:random-elt
    #:random-range
    #:random-range-exclusive
    #:random-around
    #:random-gaussian
    #:random-gaussian-integer
    #:d))

(defpackage #:losh.functions
  (:documentation "Utilities for working with higher-order functions.")
  (:export
    #:juxt
    #:nullary))

(defpackage #:losh.control-flow
  (:documentation "Utilities for managing control flow.")
  (:export
    #:recursively
    #:recur
    #:when-found
    #:if-found
    #:gathering
    #:gather))

(defpackage #:losh.mutation
  (:documentation "Utilities for mutating places in-place.")
  (:export
    #:zapf
    #:%
    #:mulf
    #:divf
    #:modf
    #:remainderf
    #:clampf
    #:negatef
    #:notf
    #:callf))

(defpackage #:losh.lists
  (:documentation "Utilities related to lists.")
  (:export
    #:take))

(defpackage #:losh.arrays
  (:documentation "Utilities related to arrays.")
  (:export
    #:do-array
    #:fill-multidimensional-array
    #:fill-multidimensional-array-t
    #:fill-multidimensional-array-fixnum
    #:fill-multidimensional-array-single-float))

(defpackage #:losh.queues
  (:documentation "A simple queue implementation.")
  (:export
    #:queue
    #:make-queue
    #:queue-contents
    #:queue-size
    #:queue-empty-p
    #:enqueue
    #:dequeue
    #:queue-append))

(defpackage #:losh.iterate
  (:use #:iterate) ; need this for iterate's `for` symbol fuckery
  (:documentation "Custom `iterate` drivers and clauses.")
  (:export
    #:pairs-of-list
    #:averaging
    #:into
    #:timing
    #:since-start-into
    #:per-iteration-into
    #:real-time
    #:run-time
    #:in-lists
    #:in-sequences
    #:in-whatever
    #:in-array
    #:across-flat-array
    #:index-of-flat-array
    #:cycling
    #:for-nested
    #:within-radius
    #:skip-origin))

(defpackage #:losh.distributions
  (:documentation "Utilities for calculating statistical... things.")
  (:export
    #:prefix-sums
    #:frequencies))

(defpackage #:losh.hash-sets
  (:documentation "A simple hash set implementation.")
  (:export
    #:hash-set
    #:make-set
    #:set-contains-p
    #:set-empty-p
    #:set-add
    #:set-add-all
    #:set-remove
    #:set-remove-all
    #:set-clear
    #:set-random
    #:set-pop))

(defpackage #:losh.debugging
  (:documentation "Utilities for figuring out what the hell is going on.")
  (:export
    #:pr
    #:bits
    #:dis))

(defpackage #:losh.weightlists
  (:documentation
    "A simple data structure for choosing random items with weighted probabilities.")
  (:export
    #:weightlist
    #:weightlist-weights
    #:weightlist-items
    #:make-weightlist
    #:weightlist-random))

(defpackage #:losh.eldritch-horrors
  (:documentation "Abandon all hope, ye who enter here.")
  (:export
    #:dlambda
    #:define-with-macro))


(defpackage-inheriting #:losh
  (#:losh.arrays
   #:losh.control-flow
   #:losh.debugging
   #:losh.distributions
   #:losh.eldritch-horrors
   #:losh.functions
   #:losh.hash-sets
   #:losh.iterate
   #:losh.lists
   #:losh.math
   #:losh.mutation
   #:losh.queues
   #:losh.random
   #:losh.weightlists)
  (:use
    #:cl
    #:iterate
    #:losh.quickutils)
  (:documentation
    "This package exports all of the symbols in the other packages.

  If you just want to get everything you can `:use` this one and be done with
  it.  Otherwise you can `:use` only the ones you need.

  "))


;;;; Remember to add it to the docs!
