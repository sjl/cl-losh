(asdf:defsystem #:losh
  :name "losh"
  :description "My personal utility belt library."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (#:iterate)

  :serial t
  :components
  ((:module "vendor"
    :serial t
    :components ((:file "quickutils")))
   (:file "package")
   (:file "losh")))
