(defpackage :losh.test
  (:use :cl :1am :losh)
  (:shadowing-import-from :1am :test)
  (:export :run-tests))
