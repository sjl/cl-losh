(in-package :losh.debugging)

(defun pr (&rest args)
  "Print `args` readably, separated by spaces and followed by a newline.

  Returns the first argument, so you can just wrap it around a form without
  interfering with the rest of the program.

  This is what `print` should have been.

  "
  (format t "~{~S~^ ~}~%" args)
  (finish-output)
  (first args))

(defmacro prl (&rest args)
  "Print `args` labeled and readably.

  Each argument form will be printed, then evaluated and the result printed.
  One final newline will be printed after everything.

  Returns the last result.

  Examples:

    (let ((i 1)
          (l (list 1 2 3)))
      (prl i (second l)))
    ; =>
    i 1
    (second l) 2

  "
  `(prog1
    (progn ,@(mapcar (lambda (arg) `(pr ',arg ,arg)) args))
    (terpri)
    (finish-output)))


(defun bits (&optional (n *) (size 8) (stream t))
  "Print the bits of the `size`-bit two's complement integer `n` to `stream`.

  Examples:

    (bits 5 10)
    => 0000000101

    (bits -5 10)
    => 1111111011

  "
  ;; http://blog.chaitanyagupta.com/2013/10/print-bit-representation-of-signed.html
  (format stream (format nil "~~~D,'0B" size) (ldb (byte size 0) n)))

(defun hex (&optional (thing *) (stream t))
  "Print the `thing` to `stream` with numbers in base 16.

  Examples:

    (hex 255)
    => FF

    (hex #(0 128))
    => #(0 80)

  "
  (let ((*print-base* 16))
    (case stream
      ((nil) (prin1-to-string thing))
      ((t) (prin1 thing stream) (terpri stream) nil)
      (otherwise (prin1 thing stream) (terpri stream) nil))))

(defmacro shut-up (&body body)
  "Run `body` with stdout and stderr redirected to the void."
  `(let ((*standard-output* (make-broadcast-stream))
         (*error-output* (make-broadcast-stream)))
    ,@body))

(defmacro dis (&body body)
  "Disassemble the code generated for a `lambda` with `arglist` and `body`.

  It will also spew compiler notes so you can see why the garbage box isn't
  doing what you think it should be doing.

  "
  (let ((%disassemble #+sbcl 'sb-disassem:disassemble-code-component
                      #-sbcl 'disassemble))
    (destructuring-bind (arglist &body body)
        (iterate (for b :first body :then (cdr b))
                 (while (not (listp (car b))))
                 (finally (return b)))
      `(,%disassemble (compile nil '(lambda ,arglist
                                     (declare (optimize speed))
                                     ,@body))))))

(defmacro comment (&body body)
  "Do nothing with a bunch of forms.

  Handy for block-commenting multiple expressions.

  "
  (declare (ignore body))
  nil)


(defun aesthetic-string (thing)
  "Return the string used to represent `thing` when printing aesthetically."
  (format nil "~A" thing))

(defun structural-string (thing)
  "Return the string used to represent `thing` when printing structurally."
  (format nil "~S" thing))

(defun print-table (rows)
  "Print `rows` as a nicely-formatted table.

  Each row should have the same number of colums.

  Columns will be justified properly to fit the longest item in each one.

  Example:

    (print-table '((1 :red something)
                   (2 :green more)))
    =>
    1 | RED   | SOMETHING
    2 | GREEN | MORE

  "
  (when rows
    (iterate
      (with column-sizes =
            (reduce (curry #'mapcar #'max)
                    (mapcar (curry #'mapcar (compose #'length #'aesthetic-string))
                            rows))) ; lol
      (for row :in rows)
      (format t "~{~vA~^ | ~}~%" (weave column-sizes row))))
  (values))


(defun pretty-print-hash-table (*standard-output* ht)
  (pprint-logical-block
      (*standard-output* (hash-table-contents ht) :prefix "{" :suffix "}")
    (pprint-exit-if-list-exhausted)
    (loop (destructuring-bind (k v) (pprint-pop)
            (write k)
            (write-string ": ")
            (write v)
            (pprint-exit-if-list-exhausted)
            (write-string ", ")
            (pprint-newline :linear)))))


(set-pprint-dispatch 'hash-table 'pretty-print-hash-table)


#+sbcl
(defun dump-profile (filename)
  (with-open-file (*standard-output* filename
                                     :direction :output
                                     :if-exists :supersede)
    (sb-sprof:report :type :graph
                     :sort-by :cumulative-samples
                     :sort-order :ascending)
    (sb-sprof:report :type :flat
                     :min-percent 0.5)))

#+sbcl
(defun start-profiling (&key call-count-packages (mode :cpu))
  "Start profiling performance.  SBCL only.

  `call-count-packages` should be a list of package designators.  Functions in
  these packages will have their call counts recorded via
  `sb-sprof::profile-call-counts`.

  "
  (sb-sprof::reset)
  (-<> call-count-packages
    (mapcar #'mkstr <>)
    (mapcar #'string-upcase <>)
    (mapc #'sb-sprof::profile-call-counts <>))
  (sb-sprof::start-profiling :max-samples 50000
                             :mode mode
                             ; :mode :time
                             :sample-interval 0.01
                             :threads :all))

#+sbcl
(defun stop-profiling (&optional (filename "lisp.prof"))
  "Stop profiling performance and dump a report to `filename`.  SBCL only."
  (sb-sprof::stop-profiling)
  (dump-profile filename))

#+sbcl
(defmacro profile (form &key (mode :cpu))
  "Profile `form` and dump the report to `lisp.prof`."
  `(progn
     (start-profiling :mode ,mode)
     (unwind-protect
         (time ,form)
       (stop-profiling))))


(defmacro gimme (n &body body)
  `(iterate (repeat ,n)
     (collect (progn ,@body))))


