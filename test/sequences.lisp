(in-package :losh.test)


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
           #'sort-fancy<)))


(define-test string-join
  (is (string= "" (string-join #\x '())))
  (is (string= "A" (string-join #\x '(a))))
  (is (string= "AxB" (string-join #\x '(a b))))
  (is (string= "AxBxC" (string-join #\x '(a b c))))
  (is (string= "A, B, C" (string-join ", " #(a b c))))
  (is (string= "foo" (string-join #\space '("foo"))))
  (is (string= "f o o" (string-join #\space "foo"))))
