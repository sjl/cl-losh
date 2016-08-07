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
    #:d

    #:juxt

    #:recursively
    #:recur

    #:zap%
    #:%
    #:mulf
    #:zapf

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

    ))
