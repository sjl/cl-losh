(in-package :losh.regex)

;; Nerd-sniped by https://news.ycombinator.com/item?id=43468972, sigh

(defmacro recase ((target-string &optional) &body clauses)
  "Match a target string against regexes, also binding variables.

  Each clause is of the form:

    (condition &rest body)

  Where `condition` is a list (or just the regex if no variables are required):

    (regex &rest ppcre-var-list)

  The target string will be matched against `regex` and `ppcre-var-list` bound
  with `ppcre:register-groups-bind`.  If it matches, `body` will be executed and
  its value returned, otherwise execution continues to later clauses.

  A final condition of `t` can be used as a fallback.

  Declarations are supported.

  Example:

    (recase (string)
      ((\"([0-9]{4})-([0-9]{2})-([0-9]{2})\" (#'parse-integer year month day))
       (declare (ignore month day))
       (format t \"~S was a good year for PLs.\" year))
      ((\"([A-Z][a-z]+) ([0-9]{1,2}), ([0-9]{4})\" month (#'parse-integer day year))
       (declare (ignore year day))
       (format t \"~A was a good month for Lisp.\" month))
      (t \"Programming is hard.\"))

  "
  (with-gensyms (block-name)
    (once-only (target-string)
      (flet ((parse-clause (clause)
               (destructuring-bind (condition &rest body) clause
                 (multiple-value-bind
                     (body declarations)
                     (alexandria:parse-body body)
                   (if (eql t condition)
                     `(let ()
                        ,@declarations
                        (return-from ,block-name (progn ,@body)))
                     (destructuring-bind (regex &rest vars) (ensure-list condition)
                       `(ppcre:register-groups-bind (,@vars)
                          (,regex ,target-string)
                          ,@declarations
                          (return-from ,block-name (progn ,@body)))))))))
        `(block ,block-name
           ,@(mapcar #'parse-clause clauses))))))
