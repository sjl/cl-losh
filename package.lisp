(defpackage #:losh.internal
  (:use #:cl))

(in-package #:losh.internal)

(defun reexport (source-package &optional (target-package '#:losh))
  (do-external-symbols (s (find-package source-package))
    (import s (find-package target-package))
    (export s (find-package target-package))))

(defmacro defsubpackage (name &rest args)
  `(progn
    (defpackage ,name ,@args)
    (reexport ',name)))


(defpackage #:losh
  (:use
    #:cl
    #:iterate
    #:losh.quickutils)
  (:documentation
    "This package exports all of the symbols in the other packages.

  If you just want to get everything you can `:use` this one and be done with
  it.  Otherwise you can `:use` only the ones you need.

  "))


(defsubpackage #:losh.symbols
  (:documentation "Utilities related to symbols.")
  (:export
    #:symbolize))

(defsubpackage #:losh.math
  (:documentation "Utilities related to math and numbers.")
  (:export
    #:tau
    #:square
    #:dividesp
    #:norm
    #:lerp
    #:precide-lerp
    #:map-range
    #:clamp))

(defsubpackage #:losh.random
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

(defsubpackage #:losh.functions
  (:documentation "Utilities for working with higher-order functions.")
  (:export
    #:juxt
    #:nullary))

(defsubpackage #:losh.control-flow
  (:documentation "Utilities for managing control flow.")
  (:export
    #:recursively
    #:recur))

(defsubpackage #:losh.mutation
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
    #:callf))

(defsubpackage #:losh.lists
  (:documentation "Utilities related to lists.")
  (:export
    #:take))

(defsubpackage #:losh.arrays
  (:documentation "Utilities related to arrays.")
  (:export
    #:do-array))

(defsubpackage #:losh.hash-tables
  (:documentation "Utilities related to hash tables.")
  (:export
    #:gethash-or-init))

(defsubpackage #:losh.queues
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

(defsubpackage #:losh.iterate
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
    #:for-nested))

(defsubpackage #:losh.distributions
  (:documentation "Utilities for calculating statistical... things.")
  (:export
    #:prefix-sums
    #:frequencies))

(defsubpackage #:losh.hash-sets
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

(defsubpackage #:losh.debugging
  (:documentation "Utilities for figuring out what the hell is going on.")
  (:export
    #:pr
    #:bits
    #:dis))

(defsubpackage #:losh.file-io
  (:documentation "Utilities for reading from and writing to files.")
  (:export
    #:slurp
    #:spit))

(defsubpackage #:losh.eldritch-horrors
  (:documentation "Abandon all hope, ye who enter here.")
  (:export
    #:dlambda
    #:define-with-macro))


;;;; Remember to add it to the docs!
