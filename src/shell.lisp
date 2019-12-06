(in-package :losh.shell)

(defun sh (command &key input (wait t) (result-type 'null))
  "Run `command`, piping `input` to it, optionally returning its output.

  `command` must be either a string (the program), or a list of the program and
  its arguments.

  `wait` must be a boolean.  If true, this function will block until the command
  completes.  If false, it will return immediately and allow the program to run
  asynchronously.

  `input` must be a character input stream, a string, or `nil`.  If non-`nil`
  its contents will be sent to the program as its standard input.

  `result-type` must be one of:

  * `null`: output will be sent to `/dev/null` and `nil` returned.
  * `stream`: output will be returned as a character stream.
  * `string`: all output will be gathered up and returned as a single string.
  * `list`: all output will be gathered up and returned as a list of lines.

  If `wait` is `nil`, the only acceptable values for `result-type` are `null`
  and `stream`.

  "
  (ctypecase command
    (string (setf command (list command)))
    ((cons string list)))
  (ctypecase input
    (string (setf input (make-string-input-stream input)))
    (stream)
    (null))
  (when (not wait)
    (assert (member result-type '(null stream)) ()
      "`result-type` must be `stream` or `null` when not `wait`ing."))
  (let* ((out (if wait ; why is every external programming running facility a goddamn mess?
                (ecase result-type
                  ((string stream list) (make-string-output-stream))
                  (null nil))
                (ecase result-type
                  ((string list) (make-string-output-stream))
                  (stream :stream)
                  (null nil))))
         (result (multiple-value-list
                   (funcall (if wait #'external-program:run #'external-program:start)
                            (first command) (rest command)
                            :output out
                            :input input))))
    (flet ((output-stream () ; jesus christ
             (if wait
               (make-string-input-stream (get-output-stream-string out))
               (external-program:process-output-stream (first result)))))
      (values-list
        (cons (ecase result-type
                (null nil)
                (stream (output-stream))
                (string (get-output-stream-string out))
                (list (iterate (for line :in-stream (output-stream) :using #'read-line)
                               (collect line))))
              result)))))


(defun pbcopy (object)
  "`pbcopy` the `aesthetic-string` of `object`."
  (sh "pbcopy" :input (format nil "~A" object) :wait nil)
  (values))

(defun pbpaste ()
  "`pbpaste` the current clipboard as a string."
  (values (sh "pbpaste" :result-type 'string)))
