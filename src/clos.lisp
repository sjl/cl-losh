(in-package :losh.clos)

(defun build-slot-definition (conc-name slot-spec)
  (destructuring-bind (name &rest slot-options) (ensure-list slot-spec)
    `(,name
      ,@(unless (getf slot-options :initarg)
          `(:initarg ,(alexandria:make-keyword name)))
      ,@(unless (or (getf slot-options :reader)
                    (getf slot-options :writer)
                    (getf slot-options :accessor))
          `(:accessor ,(if conc-name
                         (symb conc-name name)
                         name)))
      ,@slot-options)))

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



(defmacro ensure-slot-value (object slot &optional default)
  "Return the `slot-value` of `slot` in `object`, setting it to `default` if unbound."
  (alexandria:once-only (object slot)
    `(if (slot-boundp ,object ,slot)
       (slot-value ,object ,slot)
       (setf (slot-value ,object ,slot) ,default))))
