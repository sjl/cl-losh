(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :emptyp
               :ensure-keyword
               :ensure-list
               :flatten
               :map-tree
               :mkstr
               :once-only
               :rcurry
               :symb
               :weave
               :with-gensyms

               )
  :package "LOSH.QUICKUTILS")
