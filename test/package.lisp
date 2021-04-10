(defpackage :losh.test
  (:use :cl :1am :losh :iterate)
  (:shadowing-import-from :1am :test)
  (:export :run-tests))
