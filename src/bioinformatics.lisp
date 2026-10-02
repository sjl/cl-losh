(in-package :losh.bioinformatics)


(defun nxx (data n key)
  (if (alexandria:emptyp data)
    0
    (let* ((lengths (sort (map 'vector key data) #'>))
           (total (reduce #'+ lengths))
           (target (* total n)))
      (loop :with x = 0
            :for l :across lengths
            :do (incf x l)
            :when (>= x target) :do (return l)))))

(defun n50 (data &key (key #'identity))
  "Return the N50 statistic of `data`.

  `key` will be called on each element of `data` and should return the length
  of each datum.

  An empty `data` will return an N50 of `0`.

  Examples:

    (n50 (list 2 3 4 5 6 7 8 9 10))
    ;; => 8

    (n50 (vector \"ACTACCAT\"
                 \"CAGAC\"
                 \"GCTT\"
                 \"CCCCCCC\"
                 \"CCAACCAAA\"
                 \"CA\")
         :key #'length)
    ;; => 7

  "
  (nxx data 0.5d0 key))

(defun n90 (data &key (key #'identity))
  "Return the N90 statistic of `data`.

  `key` will be called on each element of `data` and should return the length
  of each datum.

  An empty `data` will return an N90 of `0`.

  Examples:

    (n90 (list 2 3 4 5 6 7 8 9 10))
    ;; => 4

    (n90 (vector \"ACTACCAT\"
                 \"CAGAC\"
                 \"GCTT\"
                 \"CCCCCCC\"
                 \"CCAACCAAA\"
                 \"CA\")
         :key #'length)
    ;; => 4

  "
  (nxx data 0.9d0 key))


(defparameter *x*
  (list
    990 990 990
    2500
    10000 12000 12000 14000
    )
  )

(defparameter *y*
  (list
    23 10 99
    7938
    6000 6000 12000 21400
    )
  )

(list
  (float (alexandria:mean *x*))
  (n50 *x*)
  '--
  (float (alexandria:mean *y*))
  (n50 *y*)
  )
