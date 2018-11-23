(asdf:defsystem :losh
  :name "losh"
  :description "My personal utility belt library."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT"
  :version "0.0.1"

  :in-order-to ((asdf:test-op (asdf:test-op :losh/test)))

  :depends-on (:iterate
               #+sbcl :sb-sprof
               )

  :serial t
  :components
  ((:module "vendor"
    :serial t
    :components ((:file "quickutils")))
   (:file "package")
   (:module "src"
    :components (

                 (:file "chili-dogs")
                 (:file "clos")
                 (:file "eldritch-horrors")
                 (:file "functions")
                 (:file "hash-sets")
                 (:file "io")
                 (:file "iterate-pre")
                 (:file "lists")
                 (:file "mutation")

                 (:file "arrays" :depends-on ("chili-dogs"))
                 (:file "bits" :depends-on ("chili-dogs"))
                 (:file "queues" :depends-on ("chili-dogs"))
                 (:file "priority-queues" :depends-on ("mutation"))

                 (:file "control-flow" :depends-on ("queues"))

                 (:file "math" :depends-on ("control-flow"
                                            "chili-dogs"))
                 (:file "hash-tables" :depends-on ("control-flow"))

                 (:file "random" :depends-on ("math"
                                              "chili-dogs"))
                 (:file "sequences" :depends-on ("chili-dogs"
                                                 "hash-tables"
                                                 "functions"
                                                 "mutation"))
                 (:file "debugging" :depends-on ("control-flow"
                                                 "math"
                                                 "hash-tables"))

                 (:file "iterate" :depends-on ("control-flow"
                                               "sequences"))
                 (:file "gnuplot" :depends-on ("control-flow"
                                               "debugging"
                                               "sequences"))
                 (:file "weightlists" :depends-on ("sequences"))

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
                             (:file "control-flow"))))

  :perform (asdf:test-op (op system)
             (funcall (read-from-string "losh.test:run-tests"))))
