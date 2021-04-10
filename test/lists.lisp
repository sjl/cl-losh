(in-package :losh.test)

(define-test somelist
  (is (equal '(a b c d)
             (somelist (lambda (l)
                         (if (eq (car l) 'a)
                           l
                           nil))
                       '(a b c d))))
  (is (equal '(c d)
             (somelist (lambda (l)
                         (if (eq (car l) 'c)
                           l
                           nil))
                       '(a b c d))))
  (is (equal 6
             (somelist (lambda (l1 l2)
                         (if (eq (car l1) (car l2))
                           (+ (length l1) (length l2))
                           nil))
                       '(a b c d e)
                       '(e d c b a)))))

