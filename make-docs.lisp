(ql:quickload "cl-d-api")

(defparameter *document-packages*
  (list "LOSH"

        "LOSH.ASTAR"
        "LOSH.ARRAYS"
        "LOSH.BASE"
        "LOSH.BITS"
        "LOSH.CHILI-DOGS"
        "LOSH.CLOS"
        "LOSH.CONTROL-FLOW"
        "LOSH.DEBUGGING"
        "LOSH.ELDRITCH-HORRORS"
        "LOSH.FUNCTIONS"
        "LOSH.GNUPLOT"
        "LOSH.HASH-SETS"
        "LOSH.HASH-TABLES"
        "LOSH.IO"
        "LOSH.ITERATE"
        "LOSH.LISTS"
        "LOSH.MATH"
        "LOSH.MUTATION"
        "LOSH.PRIORITY-QUEUES"
        "LOSH.QUEUES"
        "LOSH.RANDOM"
        "LOSH.READTABLE"
        "LOSH.RING-BUFFERS"
        "LOSH.SEQUENCES"
        "LOSH.SHELL"
        "LOSH.WEIGHTLISTS"

        ))

(defparameter *output-path*
  #p"DOCUMENTATION.markdown" )

(defparameter *header*
  "This library is my own personal utility belt.

  Everything I write in here is MIT licensed, so you're free to use it if
  you want.  But I make no guarantees about backwards compatibility -- I might
  change and break things at any time.  Use this at your own risk.
  

  ")

(d-api:generate-documentation
  :losh
  *output-path*
  *document-packages*
  *header*
  :title "Documentation for `cl-losh`")
