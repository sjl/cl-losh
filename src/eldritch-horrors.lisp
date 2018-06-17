(in-package :losh.eldritch-horrors)

(defmacro with-flexible-accessors (slot-entries instance-form &rest body)
  (with-gensyms (instance)
    `(let ((,instance ,instance-form))
      (declare (ignorable ,instance))
      (symbol-macrolet
          ,(iterate (for (symbol accessor) :in slot-entries)
                    (collect `(,symbol (,accessor ,instance))))
        ,@body))))

(defmacro define-with-macro (type-and-options &rest slots)
  "Define a with-`type` macro for the given `type` and `slots`.

  This new macro wraps `with-accessors` so you don't have to type `type-`
  a billion times.

  The given `type` must be a symbol naming a struct or class.  It must have the
  appropriate accessors with names exactly of the form `type`-`slot`.

  The defined macro will look something like this:

    (define-with-macro foo a b)
    =>
    (defmacro with-foo ((foo &optional (a-symbol 'a) (b-symbol 'b))
                        &body body)
      `(with-accessors ((,a-symbol foo-a) (,b-symbol foo-b))
           ,foo
         ,@body))

  There's a lot of magic here, but it cuts down on boilerplate for simple things
  quite a lot.

  Example:

    (defstruct foo x y)
    (define-with-macro foo x y)

    (defparameter *f* (make-foo :x 10 :y 20))
    (defparameter *g* (make-foo :x 555 :y 999))

    (with-foo (*f*)
      (with-foo (*g* gx gy)
        (print (list x y gx gy))))
    =>
    (10 20 555 999)

  "
  (destructuring-bind (type &key (conc-name (symb type '-)))
      (ensure-list type-and-options)
    (let* ((accessors (loop :for slot :in slots
                            :collect (symb conc-name slot)))
           (symbol-args (loop :for slot :in slots
                              :collect (symb slot '-symbol)))
           (macro-name (symb 'with- type))
           (macro-arglist `((,type &optional
                             ,@(loop :for slot :in slots
                                     :for arg :in symbol-args
                                     :collect `(,arg ',slot)))
                            &body body))
           (accessor-binding-list (loop :for arg :in symbol-args
                                        :for accessor :in accessors
                                        :collect ``(,,arg ,',accessor))))
      `(defmacro ,macro-name ,macro-arglist
        `(with-flexible-accessors ,,`(list ,@accessor-binding-list)
          ,,type
          ,@body)))))


(defmacro eval-dammit (&body body)
  "Just evaluate `body` all the time, jesus christ lisp."
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))

