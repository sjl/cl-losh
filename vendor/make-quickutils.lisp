(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :emptyp
               :ensure-keyword
               :ensure-list
               :mkstr
               :once-only
               :rcurry
               :symb
               :with-gensyms

               )
  :package "LOSH.QUICKUTILS")
