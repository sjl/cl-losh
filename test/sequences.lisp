(in-package :losh.test)

(defparameter *words* nil)

(defun words ()
  (when (null *words*)
    (setf *words* (gathering-vector ()
                    (do-file (line "/usr/share/dict/words")
                      (gather line)))))
  *words*)

(define-test make-sorting-predicate
  (flet ((check (original expected &rest preds)
           (let ((actual (sort (copy-seq original)
                               (apply #'make-sorting-predicate preds))))
             (is (equalp expected actual)))))
    (check '("zz" "yy" "abc")
           '("abc" "yy" "zz")
           #'string<)
    (check '("zz" "yy" "abc")
           '("yy" "zz" "abc")
           (cons #'< #'length)
           #'string<)
    (check '("yy" "zz" "abc")
           '("zz" "yy" "abc")
           (cons #'< #'length)
           #'string>)
    (check '("az" "by" "aby" "zzy")
           '("by" "aby" "zzy" "az")
           (lambda (x y)
             (char< (char x (1- (length x)))
                    (char y (1- (length y)))))
           (cons #'< #'length)
           #'string<)
    (check '("az" "by" "aby" "zzy")
           '("by" "aby" "zzy" "az")
           (cons #'char< (lambda (s) (char s (1- (length s)))))
           (cons #'< #'length)
           #'string<)))


(define-sorting-predicate sort-trivial<
  #'string<)

(define-sorting-predicate sort-short<
  (#'< :key #'length)
  #'string<)

(define-sorting-predicate sort-short>
  (#'< :key #'length)
  #'string>)

(define-sorting-predicate sort-last-char<
  (lambda (x y)
    (char< (char x (1- (length x)))
           (char y (1- (length y)))))
  (#'< :key #'length)
  #'string<)

(define-sorting-predicate sort-fancy<
  (#'char< :key (lambda (s) (char s (1- (length s)))))
  (#'< :key #'length)
  #'string<)

(define-sorting-predicate sort-fancy-quoted<
  ('char< :key (lambda (s) (char s (1- (length s)))))
  ('< :key #'length)
  'string<)

(define-test define-sorting-predicate
  (flet ((check (original expected pred)
           (let ((actual (sort (copy-seq original) pred)))
             (is (equalp expected actual)))))
    (check '("zz" "yy" "abc")
           '("abc" "yy" "zz")
           #'sort-trivial<)
    (check '("zz" "yy" "abc")
           '("yy" "zz" "abc")
           #'sort-short<)
    (check '("yy" "zz" "abc")
           '("zz" "yy" "abc")
           #'sort-short>)
    (check '("az" "by" "aby" "zzy")
           '("by" "aby" "zzy" "az")
           #'sort-last-char<)
    (check '("az" "by" "aby" "zzy")
           '("by" "aby" "zzy" "az")
           #'sort-fancy<)
    (check '("az" "by" "aby" "zzy")
           '("by" "aby" "zzy" "az")
           #'sort-fancy-quoted<)))

(defun sortedp (sequence predicate)
  ;; TODO Should this be a util of its own?
  (etypecase sequence
    (list (loop :for x = (pop sequence)
                :until (null sequence)
                :never (funcall predicate (first sequence) x)))
    (sequence (loop :with l = (length sequence)
                    :for x :from 0 :below l
                    :for y :from 1 :below l
                    :never (funcall predicate (elt sequence y) (elt sequence x))))))

(defun vowelp (char)
  (find (char-downcase char) "aeiou"))

(defun vowels< (a b)
  (< (count-if #'vowelp a) (count-if #'vowelp b)))

(defun random-elts (n sequence &key (result-type 'list))
  "Return `N` random elements from `sequence` (duplicates allowed).

  This wil not be fast if `sequence` is a list.

  "
  (ecase result-type
    (list (loop :repeat n :collect (random-elt sequence)))
    (vector (loop :with result = (make-array n)
                  :for i :from 0 :below n
                  :do (setf (aref result i) (random-elt sequence))
                  :finally (return result)))))


(define-test fuzz-sorting-predicates
  (let ((specs (vector 'string<
                       (cons '< 'length)
                       (cons 'string< 'reverse)
                       'vowels<
                       (cons '< 'sxhash)))
        (words (words)))
    (do-repeat 256
      (let* ((specs (random-elts (random-range 1 (+ 3 (length specs))) specs))
             (predicate (apply #'make-sorting-predicate specs))
             (seq (random-elts (random-range 0 100) words :result-type 'vector)))
        (setf seq (sort seq predicate))
        (is (sortedp seq predicate))))))


(define-test string-join
  (is (string= "" (string-join #\x '())))
  (is (string= "A" (string-join #\x '(a))))
  (is (string= "AxB" (string-join #\x '(a b))))
  (is (string= "AxBxC" (string-join #\x '(a b c))))
  (is (string= "A, B, C" (string-join ", " #(a b c))))
  (is (string= "foo" (string-join #\space '("foo"))))
  (is (string= "f o o" (string-join #\space "foo"))))

(define-test fuzz-string-join
  (let ((words (words)))
    (do-repeat 500
      (let* ((n (random-range 0 10))
             (ws (random-elts n words))
             (sep (random-elt #(#\, "" "," ", ")))
             (result (string-join sep ws)))
        (if (zerop n)
          (is (string= "" result))
          (is (= (+ (reduce #'+ ws :key #'length)
                    (* (1- n) (length (string sep))))
                 (length result))))))))

(defun check-reductions (function expected input &rest args)
  (is (equalp expected
              (apply #'reductions function input :result-type 'list args)))
  (is (equalp (coerce expected 'vector)
              (apply #'reductions function input :result-type 'vector args)))
  (is (equalp expected
              (apply #'reductions function (coerce input 'vector) :result-type 'list args)))
  (is (equalp (coerce expected 'vector)
              (apply #'reductions function (coerce input 'vector) :result-type 'vector args))))

(define-test reductions/basic
  (check-reductions #'+ '() '())
  (check-reductions #'+ '(1) '(1))
  (check-reductions #'+ '(1 3) '(1 2))
  (check-reductions #'+ '(1 3 6) '(1 2 3))
  (check-reductions #'+ '(100 101 103 106) '(1 2 3)
                    :initial-value 100)
  (check-reductions #'cons
                    '(nil (-3) (-2 -3) (-1 -2 -3))
                    '(1 2 3)
                    :initial-value nil
                    :key #'-
                    :from-end t))

(define-test reductions/initial-value
  (check-reductions #'+ '(23) '() :initial-value 23)
  (check-reductions #'+ '(23 123) '(100) :initial-value 23)
  (check-reductions #'+ '(23 123 1123) '(100 1000) :initial-value 23))

(define-test reductions/key
  ;; Key should be called on the contents.
  (check-reductions #'+
                    '(-1 -3 -6)
                    '(1 2 3)
                    :key #'-)
  ;; Key should NOT be called on the initial value, if given.
  (check-reductions #'+
                    '(100 101 103 106)
                    '((1) (2) (3))
                    :initial-value 100
                    :key #'car))

(define-test reductions/start-end
  (check-reductions #'+ '(0 1 3 6 10 15) '(0 1 2 3 4 5) :start 0 :end nil)
  (check-reductions #'+ '(  1 3 6 10 15) '(0 1 2 3 4 5) :start 1 :end nil)
  (check-reductions #'+ '(0 1 3 6 10   ) '(0 1 2 3 4 5) :start 0 :end 5)
  (check-reductions #'+ '(    2 5  9   ) '(0 1 2 3 4 5) :start 2 :end 5)
  (check-reductions #'+ '(    2        ) '(0 1 2 3 4 5) :start 2 :end 3)
  (check-reductions #'+ '(             ) '(0 1 2 3 4 5) :start 2 :end 2)
  (check-reductions #'+ '(             ) '(0 1 2 3 4 5) :start 6 :end nil)
  (check-reductions #'+ '(    2 5  9   ) (mapcar #'list '(0 1 2 3 4 5)) :start 2 :end 5 :key #'car))

(define-test reductions/from-end
  (flet ((cat (a b) (concatenate 'string a b)))
    (check-reductions #'cat
                      '("E" "DE" "CDE" "BCDE" "ABCDE")
                      '(a b c d e)
                      :from-end t
                      :key #'string)
    (check-reductions #'cat
                      '("" "E" "DE" "CDE" "BCDE" "ABCDE")
                      '(a b c d e)
                      :from-end t
                      :key #'string :initial-value "")
    (check-reductions #'cat
                      '("" "C" "BC")
                      '(a b c d e)
                      :from-end t
                      :key #'string :initial-value ""
                      :start 1 :end 3)
    (check-reductions #'cat
                      '("C" "BC")
                      '(a b c d e)
                      :from-end t
                      :key #'string
                      :start 1 :end 3)
    (check-reductions #'cat
                      '()
                      '(a b c d e)
                      :from-end t
                      :key #'string
                      :start 1 :end 1)
    (check-reductions #'cat
                      '("")
                      '(a b c d e)
                      :from-end t
                      :key #'string :initial-value ""
                      :start 1 :end 1)))

(define-test reductions/non-list
  (check-reductions #'+ '(1 3 6) (vector 1 2 3))
  (check-reductions #'+ '(99 100) (vector 1 2 3) :start 0 :end 1 :initial-value 99)
  (check-reductions #'+ '(99) (vector 1 2 3) :start 0 :end 0 :initial-value 99))
