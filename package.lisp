(defpackage #:losh
  (:use
    #:cl
    #:iterate
    #:losh.quickutils)
  (:export
    #:symbolize

    #:tau
    #:square
    #:dividesp
    #:norm
    #:lerp
    #:precide-lerp
    #:map-range
    #:clamp

    #:randomp
    #:random-elt
    #:random-range
    #:random-range-exclusive
    #:random-around
    #:random-gaussian
    #:random-gaussian-integer
    #:d

    #:juxt

    #:recursively
    #:recur

    #:zapf
    #:%
    #:mulf
    #:divf
    #:modf
    #:remainderf
    #:clampf
    #:callf

    #:take

    #:do-array

    #:gethash-or-init

    #:queue
    #:make-queue
    #:queue-contents
    #:queue-size
    #:queue-empty-p
    #:enqueue
    #:dequeue
    #:queue-append

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

    #:prefix-sums
    #:frequencies

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
    #:set-pop

    #:pr
    #:bits
    #:dis

    #:slurp
    #:spit

    #:dlambda

    #:define-with-macro

    ))
