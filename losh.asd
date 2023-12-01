(asdf:defsystem :losh
  :name "losh"
  :description "My personal utility belt library."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT"
  :version "0.0.1"

  :in-order-to ((asdf:test-op (asdf:test-op :losh/test)))

  :depends-on (

               #+sbcl :sb-sprof
               :alexandria
               :cl-ppcre
               :external-program
               :flexi-streams
               :iterate
               :pileup
               :named-readtables

               )

  :components
  ((:module "src"
    :components (
                 ;; -1 --------------------------------------------------------
                 (:file "package")
                 (:file "base" :depends-on ("package"))

                 ;; 0 ---------------------------------------------------------
                 (:file "chili-dogs" :depends-on ("base"))
                 (:file "clos" :depends-on ("base"))
                 (:file "eldritch-horrors" :depends-on ("base"))
                 (:file "functions" :depends-on ("base"))
                 (:file "hash-sets" :depends-on ("base"))
                 (:file "io" :depends-on ("base"))
                 (:file "lists" :depends-on ("base"))
                 (:file "mutation" :depends-on ("base"))
                 (:file "shell" :depends-on ("base"))

                 ;; 1 ---------------------------------------------------------
                 (:file "arrays" :depends-on ("chili-dogs"))
                 (:file "bits" :depends-on ("chili-dogs"))
                 (:file "queues" :depends-on ("chili-dogs"))
                 (:file "priority-queues" :depends-on ("mutation"))
                 (:file "ring-buffers" :depends-on ("chili-dogs"
                                                    "eldritch-horrors"
                                                    "mutation"))

                 ;; 2 ---------------------------------------------------------
                 (:file "control-flow" :depends-on ("queues"))

                 ;; 3 ---------------------------------------------------------
                 (:file "astar" :depends-on ("control-flow"
                                             "chili-dogs"))
                 (:file "iterate" :depends-on ("control-flow"
                                               "hash-sets"))
                 (:file "math" :depends-on ("control-flow"
                                            "chili-dogs"))
                 (:file "hash-tables" :depends-on ("control-flow"))


                 ;; 4 ---------------------------------------------------------
                 (:file "random" :depends-on ("math"
                                              "chili-dogs"))
                 (:file "readtable" :depends-on ("hash-tables"))
                 (:file "sequences" :depends-on ("chili-dogs"
                                                 "hash-tables"
                                                 "functions"
                                                 "iterate"
                                                 "mutation"))
                 (:file "debugging" :depends-on ("control-flow"
                                                 "math"
                                                 "hash-tables"))

                 ;; 5 ---------------------------------------------------------
                 (:file "weightlists" :depends-on ("sequences"))
                 (:file "gnuplot" :depends-on ("control-flow"
                                               "iterate"
                                               "debugging"
                                               "lists"
                                               "sequences"))

                 ))))

(asdf:defsystem :losh/test
  :description "Test suite for losh."

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :depends-on (:losh :1am)

  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "base")
                             (:file "arrays")
                             (:file "lists")
                             (:file "iterate")
                             (:file "sequences")
                             (:file "control-flow")
                             (:file "ring-buffers"))))

  :perform (asdf:test-op (op system)
             (funcall (read-from-string "losh.test:run-tests"))))
