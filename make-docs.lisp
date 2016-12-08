(ql:quickload "cl-d-api")

(defparameter *document-packages*
  (list "LOSH"

        "LOSH.ARRAYS"
        "LOSH.CONTROL-FLOW"
        "LOSH.DEBUGGING"
        "LOSH.ELDRITCH-HORRORS"
        "LOSH.FUNCTIONS"
        "LOSH.HASH-SETS"
        "LOSH.HASH-TABLES"
        "LOSH.ITERATE"
        "LOSH.LICENSING"
        "LOSH.LISTS"
        "LOSH.MATH"
        "LOSH.MUTATION"
        "LOSH.QUEUES"
        "LOSH.RANDOM"
        "LOSH.SEQUENCES"
        "LOSH.WEIGHTLISTS"

        ))

(defparameter *output-path*
  #p"DOCUMENTATION.markdown" )

(defparameter *header*
  "This library is my own personal utility belt.

  Everything I write in here is MIT/X11 licensed, so you're free to use it if
  you want.  But I make no guarantees about backwards compatibility -- I might
  change and break things at any time.  Use this at your own risk.
  

  ")

(d-api:generate-documentation
  :losh
  *output-path*
  *document-packages*
  *header*
  :title "Documentation for `cl-losh`")
