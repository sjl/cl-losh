(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(:curry
               :rcurry
               :emptyp
               :ensure-list
               :with-gensyms
               :once-only)
  :package "LOSH.QUICKUTILS")
