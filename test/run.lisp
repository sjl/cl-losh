#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(ql:quickload :losh)
(time (asdf:test-system :losh))
(quit)
