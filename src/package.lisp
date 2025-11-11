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
             :collect `(:use ,parent-package)
             :collect `(:export ,@(external-symbols parent-package)))))


(defpackage :losh.base
  (:use :cl)
  (:documentation "A few utilities re-exported from Alexandria, plus some other basic stuff.")
  (:import-from :alexandria
    :compose :curry :rcurry
    :with-gensyms :once-only
    :ensure-list
    )
  (:export
    :losh

    :compose :curry :rcurry
    :with-gensyms :once-only
    :ensure-list

    :timing ; both profiling and iterate use this symbol

    :symb :mkstr))


(defpackage :losh.chili-dogs
  (:use :cl :iterate :losh.base)
  (:documentation "Gotta go FAST.")
  (:export
    :defun-inline
    :defun-inlineable))

(defpackage :losh.clos
  (:use :cl :iterate :losh.base)
  (:documentation "Utilities for working with CLOS.")
  (:export
    :defclass*
    :define-condition*
    :slot-value-or
    :ensure-slot-value))

(defpackage :losh.eldritch-horrors
  (:use :cl :iterate :losh.base)
  (:documentation "Abandon all hope, ye who enter here.")
  (:export
    :eval-dammit
    :define-with-macro
    :scratch))

(defpackage :losh.functions
  (:use :cl :iterate :losh.base)
  (:documentation "Utilities for working with higher-order functions.")
  (:export
    :juxt
    :nullary
    :fixed-point))

(defpackage :losh.hash-sets
  (:use :cl :iterate :losh.base)
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
    :hset-map!
    :hset-reduce

    :do-hash-set))

(defpackage :losh.regex
  (:use :cl :iterate :losh.base)
  (:documentation "Utilities related to regular expressions.")
  (:export
    :recase))

(defpackage :losh.streams
  (:use :cl :iterate :losh.base)
  (:documentation "Utilities related to strings, reading, and/or printing.")
  (:export
    :with-eof-handled))


(defpackage :losh.io
  (:use :cl :iterate :losh.base)
  (:documentation "Utilities for input/output/reading/etc.")
  (:export
    :read-all
    :read-all-from-file
    :read-all-from-string))

(defpackage :losh.lists
  (:use :cl :iterate :losh.base)
  (:documentation "Utilities for operating on lists.")
  (:export
    :0..   :1..   :range
    :0...  :1...  :irange
    :somelist
    :assocar  :assocdr
    :rassocar :rassocdr))

(defpackage :losh.mutation
  (:use :cl :iterate :losh.base)
  (:documentation "Utilities for mutating places in-place.")
  (:export
    :zapf
    :%
    :mulf
    :divf
    :modf
    :remainderf
    :truncatef
    :clampf
    :negatef
    :notf
    :callf
    :ensuref))

(defpackage :losh.shell
  (:use :cl :iterate :losh.base)
  (:documentation "Utilities for interacting with external programs.")
  (:export
    :sh
    :pbcopy
    :pbpaste
    :*pbcopy-command*
    :*pbpaste-command*
    :rscript
    :rscript-file))


(defpackage :losh.arrays
  (:use :cl :iterate :losh.base
    :losh.chili-dogs)
  (:documentation "Utilities related to arrays.")
  (:export
    :do-array
    :bisect-left
    :bisect-right
    :fill-multidimensional-array
    :fill-multidimensional-array-t
    :fill-multidimensional-array-fixnum
    :fill-multidimensional-array-single-float
    :vector-last))

(defpackage :losh.bits
  (:use :cl :iterate :losh.base
    :losh.chili-dogs)
  (:documentation "Utilities for low-level bit stuff.")
  (:export
    :+/8
    :-/8
    :+/16
    :-/16
    :+/32
    :-/32
    :+/64
    :-/64))

(defpackage :losh.queues
  (:use :cl :iterate :losh.base
    :losh.chili-dogs)
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

(defpackage :losh.priority-queues
  (:use :cl :iterate :losh.base
    :losh.mutation)
  (:documentation "Jankass priority queue implementation.")
  (:export
    :priority-queue
    :make-priority-queue

    :pq-insert
    :pq-ensure
    :pq-dequeue))

(defpackage :losh.ring-buffers
  (:use :cl :iterate :losh.base
    :losh.chili-dogs
    :losh.eldritch-horrors
    :losh.mutation)
  (:documentation "Simple ring buffer implementation.")
  (:export

    :do-ring-buffer
    :make-ring-buffer
    :rb-clear
    :rb-contents
    :rb-count
    :rb-empty-p
    :rb-full-p
    :rb-pop
    :rb-push
    :rb-ref
    :rb-safe-push
    :rb-size
    :ring-buffer

    ))


(defpackage :losh.control-flow
  (:use :cl :iterate :losh.base
    :losh.queues)
  (:documentation "Utilities for managing control flow.")
  (:export
    :_
    :nest
    :recursively
    :recur
    :when-found
    :if-found
    :gathering
    :gathering-vector
    :gather
    :if-let
    :if-let*
    :when-let
    :when-let*
    :multiple-value-bind*
    :do-repeat
    :do-range
    :do-irange
    :do-file
    :do-vector))


(defpackage :losh.astar
  (:use :cl :iterate :losh.base
    :losh.chili-dogs
    :losh.control-flow)
  (:documentation "A★ search in a handy package.")
  (:export
    :astar))

(defpackage :losh.math
  (:use :cl :iterate :losh.base
    :losh.chili-dogs
    :losh.control-flow)
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

    :clamp
    :degrees
    :dividesp
    :in-range-p
    :lerp
    :map-range
    :norm
    :precise-lerp
    :radians
    :square
    :digit))

(defpackage :losh.hash-tables
  (:use :cl :iterate :losh.base
    :losh.control-flow)
  (:documentation "Utilities for operating on hash tables.")
  (:export
    :hash-table-contents
    :mutate-hash-values
    :remhash-if
    :remhash-if-not
    :remhash-if-key
    :remhash-if-not-key
    :remhash-if-value
    :remhash-if-not-value))

(defpackage :losh.iterate
  (:use :cl :iterate :losh.base
    :losh.hash-sets
    :losh.ring-buffers)
  (:documentation "Custom `iterate` drivers and clauses.")
  (:export

    :across-flat-array
    :against
    :anding
    :averaging
    :collect-frequencies
    :collect-hash
    :collect-set
    :concatenating
    :cycling
    :end
    :every-nth
    :finding-all
    :finding-first
    :for-nested
    :if-first-iteration
    :in-array
    :in-hashset
    :in-lists
    :in-ring-buffer
    :in-sequences
    :in-whatever
    :index-of-flat-array
    :initially
    :into
    :macroexpand-iterate
    :matching
    :modulo
    :oring
    :overlap
    :pairs-of-list
    :per-iteration-into
    :real-time
    :returning
    :run-time
    :seed
    :since-start-into
    :skip-origin
    :start
    :test
    :then
    :timing
    :unless-first-iteration
    :unless-first-time
    :when-first-iteration
    :when-first-time
    :window
    :with-result
    :within-radius

    ))


(defpackage :losh.readtable
  (:use :cl :losh.base)
  (:documentation "Custom readtable.")
  (:export :losh))


(defpackage :losh.random
  (:use :cl :iterate :losh.base
    :losh.chili-dogs
    :losh.math)
  (:documentation "Utilities related to randomness.")
  (:export
    :randomp
    :random-elt
    :random-range

    :random-range-exclusive
    :random-range-inclusive
    :random-around
    :random-gaussian
    :random-gaussian-integer
    :d))

(defpackage :losh.sequences
  (:use :cl :iterate :losh.base
    :losh.chili-dogs
    :losh.functions
    :losh.hash-tables
    :losh.iterate
    :losh.mutation)
  (:documentation "Utilities for operating on sequences.")
  (:export
    :extrema
    :enumerate
    :prefix-sums
    :frequencies
    :proportions
    :group-by
    :take
    :take-while
    :drop
    :drop-while
    :chunk
    :ngrams
    :summation
    :product
    :doseq
    :string-join
    :reductions
    :define-sorting-predicate
    :make-sorting-predicate))

(defpackage :losh.debugging
  (:use :cl :iterate :losh.base
    :losh.math
    :losh.control-flow
    :losh.hash-tables)
  (:documentation "Utilities for figuring out what the hell is going on.")
  (:export

    #+sbcl :profile
    #+sbcl :profile-when
    #+sbcl :start-profiling
    #+sbcl :stop-profiling
    :aesthetic-string
    :bits
    :comment
    :dis
    :gimme
    :hex
    :phr
    :pr
    :pretty-print-hash-table
    :print-table
    :prl
    :shut-up
    :structural-string
    :timing

    ))


(defpackage :losh.gnuplot
  (:use :cl :iterate :losh.base
    :losh.control-flow
    :losh.debugging
    :losh.lists
    :losh.sequences)
  (:documentation "Utilities for plotting data with gnuplot.")
  (:export
    :gnuplot
    :with-gnuplot
    :gnuplot-data
    :gnuplot-command
    :gnuplot-format
    :plot))

(defpackage :losh.weightlists
  (:use :cl :iterate :losh.base
    :losh.sequences)
  (:documentation
    "A simple data structure for choosing random items with weighted probabilities.")
  (:export
    :weightlist
    :weightlist-weights
    :weightlist-items
    :make-weightlist
    :weightlist-random))


(defpackage-inheriting :losh
  (

   :losh.arrays
   :losh.astar
   :losh.base
   :losh.bits
   :losh.chili-dogs
   :losh.clos
   :losh.control-flow
   :losh.debugging
   :losh.eldritch-horrors
   :losh.functions
   :losh.gnuplot
   :losh.hash-sets
   :losh.hash-tables
   :losh.io
   :losh.iterate
   :losh.lists
   :losh.math
   :losh.mutation
   :losh.priority-queues
   :losh.queues
   :losh.random
   :losh.readtable
   :losh.regex
   :losh.ring-buffers
   :losh.sequences
   :losh.shell
   :losh.streams
   :losh.weightlists

   )
  (:use :cl :iterate :losh.base)
  (:documentation
    "This package exports all of the symbols in the other packages.

  If you just want to get everything you can `:use` this one and be done with
  it.  Otherwise you can `:use` only the ones you need.

  "))

(defpackage :losh-user
  (:use :cl :iterate :losh))

;;;; Remember to add it to the docs!
