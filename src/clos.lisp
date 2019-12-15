(in-package :losh.clos)

(defun build-slot-definition (conc-name slot-spec)
  (destructuring-bind (name &key
                            (type nil type?)
                            (documentation nil documentation?)
                            (initform nil initform?)
                            (allocation nil allocation?)
                            (accessor (if conc-name
                                        (symb conc-name name)
                                        name))
                            (initarg (ensure-keyword name)))
      (ensure-list slot-spec)
    `(,name
      :initarg ,initarg
      :accessor ,accessor
      ,@(when initform? `(:initform ,initform))
      ,@(when allocation? `(:allocation ,allocation))
      ,@(when type? `(:type ,type))
      ,@(when documentation? `(:documentation ,documentation)))))

(defmacro defclass* (name-and-options direct-superclasses slots &rest options)
  "`defclass` without the tedium.

  This is like `defclass`, but the `:initarg` and `:accessor` slot options will
  automatically be filled in with sane values if they aren't given.

  `name-and-options` can be a symbol or a list, which will be destructured
  against `(name &key conc-name)`.

  "
  (destructuring-bind (name &key conc-name)
      (ensure-list name-and-options)
    `(defclass ,name ,direct-superclasses
       ,(mapcar (curry #'build-slot-definition conc-name) slots)
       ,@options)))

(defmacro define-condition* (name-and-options direct-superclasses slots &rest options)
  "`define-condition` without the tedium.

  This is like `define-condition`, but the `:initarg` and `:accessor` slot
  options will automatically be filled in with sane values if they aren't given.

  `name-and-options` can be a symbol or a list, which will be destructured
  against `(name &key conc-name)`.

  "
  (destructuring-bind (name &key conc-name)
      (ensure-list name-and-options)
    `(define-condition ,name ,direct-superclasses
       ,(mapcar (curry #'build-slot-definition conc-name) slots)
       ,@options)))

