(in-package :losh.gnuplot)

;;; This very spartan gnuplot interface is inspired by the advice in Gnuplot in
;;; Action (second edition) specifically the section "Thought for the design of
;;; a gnuplot access layer" on page 253.

(defparameter *gnuplot-path* "gnuplot")
(defparameter *gnuplot-process* nil)

(defmacro with-gnuplot (&body body)
  `(let ((*gnuplot-process*
           (external-program:start *gnuplot-path* '() :input :stream :output t)))
     (unwind-protect (progn ,@body *gnuplot-process*)
       (close (external-program:process-input-stream *gnuplot-process*)))))


(defun gnuplot-data-sequence% (sequence s)
  (map nil (lambda (row)
             (map nil (lambda (val)
                        (princ val s)
                        (princ #\tab s))
                  row)
             (terpri s))
       sequence))

(defun gnuplot-data-matrix% (matrix s)
  (destructuring-bind (rows cols) (array-dimensions matrix)
    (dotimes (r rows)
      (dotimes (c cols)
        (princ (aref matrix r c) s)
        (princ #\tab s))
      (terpri s))))

(defun gnuplot-data (identifier data &aux (s (external-program:process-input-stream *gnuplot-process*)))
  "Bind `identifier` to `data` inside the currently-running gnuplot process.

  `identifier` must be a string of the form `$foo`.

  `data` must be a sequence of sequences of data or a 2D array of data.

  Must be called from inside `with-gnuplot`.

  "
  (assert (not (null *gnuplot-process*)) () "~A must be called inside ~S" 'gnuplot-data 'with-gnuplot)
  (check-type identifier string)
  (assert (char= #\$ (char identifier 0)))
  (format s "~A << EOD~%" identifier)
  (etypecase data
    ((array * (* *)) (gnuplot-data-matrix% data s))
    (sequence (gnuplot-data-sequence% data s)))
  (format s "EOD~%"))

(defun gnuplot-format (format-string &rest args &aux (s (external-program:process-input-stream *gnuplot-process*)))
  "Send a `cl:format`ed string to the currently-running gnuplot process.

  Must be called from inside `with-gnuplot`.

  "
  (assert (not (null *gnuplot-process*)) () "~A must be called inside ~S" 'gnuplot-format 'with-gnuplot)
  (apply #'format s format-string args)
  (terpri s))

(defun gnuplot-command (command &aux (s (external-program:process-input-stream *gnuplot-process*)))
  "Send the string `command` to the currently-running gnuplot process.

  Must be called from inside `with-gnuplot`.

  "
  (assert (not (null *gnuplot-process*)) () "~A must be called inside ~S" 'gnuplot-command 'with-gnuplot)
  (write-line command s))

(defun gnuplot (data commands)
  "Graph `data` with gnuplot using `commands`.

  `data` must be an alist of `(identifier . data)` pairs.  `identifier` must be
  a string of the form `$foo`.  `data` must be a sequence of sequences of data
  or a 2D array of data.

  `commands` must be a string or a sequence of strings.

  "
  (with-gnuplot
    (dolist (d data)
      (gnuplot-data (car d) (cdr d)))
    (etypecase commands
      (string (gnuplot-command commands))
      (sequence (map nil #'gnuplot-command commands)))))

