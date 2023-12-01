(in-package :losh.readtable)

(named-readtables:defreadtable losh
  (:merge :standard losh.hash-tables::hash-table-constructor-syntax))
