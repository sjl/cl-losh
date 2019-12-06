(asdf:defsystem :losh
  :name "losh"
  :description "My personal utility belt library."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT"
  :version "0.0.1"

  :in-order-to ((asdf:test-op (asdf:test-op :losh/test)))

  :depends-on (:iterate
               :external-program
               #+sbcl :sb-sprof)

  :serial t
  :components
  ((:module "vendor"
    :serial t
    :components ((:file "quickutils")))
   (:file "package")
   (:module "src"
    :components (

                 ;; 0 ---------------------------------------------------------
                 (:file "chili-dogs")
                 (:file "clos")
                 (:file "eldritch-horrors")
                 (:file "functions")
                 (:file "hash-sets")
                 (:file "io")
                 (:file "lists")
                 (:file "mutation")
                 (:file "shell")

                 ;; 1 ---------------------------------------------------------
                 (:file "arrays" :depends-on ("chili-dogs"))
                 (:file "bits" :depends-on ("chili-dogs"))
                 (:file "queues" :depends-on ("chili-dogs"))
                 (:file "priority-queues" :depends-on ("mutation"))

                 ;; 2 ---------------------------------------------------------
                 (:file "control-flow" :depends-on ("queues"))

                 ;; 3 ---------------------------------------------------------
                 (:file "iterate" :depends-on ("control-flow"
                                               "hash-sets"))
                 (:file "math" :depends-on ("control-flow"
                                            "chili-dogs"))
                 (:file "hash-tables" :depends-on ("control-flow"))


                 ;; 4 ---------------------------------------------------------
                 (:file "random" :depends-on ("math"
                                              "chili-dogs"))
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
                             (:file "control-flow"))))

  :perform (asdf:test-op (op system)
             (funcall (read-from-string "losh.test:run-tests"))))
