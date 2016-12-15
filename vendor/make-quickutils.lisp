(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :copy-hash-table
               :curry
               :emptyp
               :ensure-keyword
               :ensure-list
               :flatten
               :hash-table-keys
               :hash-table-values
               :map-tree
               :mkstr
               :once-only
               :range
               :rcurry
               :symb
               :weave
               :with-gensyms

               )
  :package "LOSH.QUICKUTILS")
