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
   (:file "losh")))

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
                             (:file "control-flow"))))

  :perform (asdf:test-op (op system)
             (funcall (read-from-string "losh.test:run-tests"))))
