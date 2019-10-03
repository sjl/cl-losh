(in-package :losh.shell)

(defun sh (command &key input output (wait t))
  "Run `command`, piping `input` to it, optionally returning its output.

  `command` must be either a string (the program), or a list of the program and
  its arguments.

  `wait` must be a boolean.  If true, this function will block until the command
  completes.  If false, it will return immediately and allow the program to run
  asynchronously.

  `input` must be a character input stream, a string, or `nil`.  If non-`nil`
  its contents will be sent to the program as its standard input.

  `output` must be one of `:string`, `:stream`, or `nil`.  `:string` cannot be
  used if `:wait` is `nil`.

  "
  (ctypecase command
    (string (setf command (list command)))
    ((cons string list)))
  (ctypecase input
    (string (setf input (make-string-input-stream input)))
    (stream)
    (null))
  (let ((result (funcall (if wait #'uiop:run-program #'uiop:launch-program)
                         command
                         :output (when output
                                   (if wait
                                     (ccase output
                                       (:string :string)
                                       (:stream :string)) ; hack because uiop doesn't support this
                                     (ccase output
                                       (:string (error "`output` cannot be `:string` when not `wait`ing."))
                                       (:stream :stream))))
                         :input input)))
    (ecase output
      ((nil) (values))
      (:stream (if wait
                 (make-string-input-stream result)
                 (uiop:process-info-output result)))
      (:string result))))


(defun pbcopy (object)
  "`pbcopy` the `aesthetic-string` of `object`."
  (sh "pbcopy" :input (format nil "~A" object) :wait nil)
  (values))

(defun pbpaste ()
  "`pbpaste` the current clipboard as a string."
  (values (sh "pbpaste" :output :string)))
