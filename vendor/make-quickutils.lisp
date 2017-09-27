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
               :hash-table-alist
               :hash-table-keys
               :hash-table-values
               :make-gensym
               :map-tree
               :mkstr
               :once-only
               :parse-body
               :range
               :rcurry
               :symb
               :weave
               :with-gensyms

               )
  :package "LOSH.QUICKUTILS")
