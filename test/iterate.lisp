(in-package :losh.test)


(define-test within-radius
  (is (equal '(-1 0 1)  (iterate (for (x) :within-radius 1)                             (collect x))))
  (is (equal '(-1 1)    (iterate (for (x) :within-radius 1 :skip-origin t)              (collect x))))
  (is (equal '(9 10 11) (iterate (for (x) :within-radius 1                :origin (10)) (collect x))))
  (is (equal '(9 11)    (iterate (for (x) :within-radius 1 :skip-origin t :origin (10)) (collect x))))

  (is (equal '((-1 -1) (-1 0) (-1 1) (0 -1) (0 0) (0 1) (1 -1) (1 0) (1 1))
             (iterate (for (x y) :within-radius 1) (collect (list x y)))))

  (is (equal '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1))
             (iterate (for (x y) :within-radius 1 :skip-origin t)
                      (collect (list x y)))))

  (is (equal '((9 99) (9 100) (9 101) (10 99) (10 101) (11 99) (11 100) (11 101))
             (iterate (for (x y) :within-radius 1 :skip-origin t :origin (10 100))
                      (collect (list x y)))))

  (is (equal '((8 98) (8 99) (8 100) (8 101) (8 102)
               (9 98) (9 99) (9 100) (9 101) (9 102)
               (10 98) (10 99) (10 101) (10 102)
               (11 98) (11 99) (11 100) (11 101) (11 102)
               (12 98) (12 99) (12 100) (12 101) (12 102))
             (iterate (for (x y) :within-radius 2 :skip-origin t :origin (10 100))
                      (collect (list x y)))))

  (is (equal '((9 99 999) (9 99 1000) (9 99 1001)
               (9 100 999) (9 100 1000) (9 100 1001)
               (9 101 999) (9 101 1000) (9 101 1001)
               (10 99 999) (10 99 1000) (10 99 1001)
               (10 100 999) (10 100 1001)
               (10 101 999) (10 101 1000) (10 101 1001)
               (11 99 999) (11 99 1000) (11 99 1001)
               (11 100 999) (11 100 1000) (11 100 1001)
               (11 101 999) (11 101 1000) (11 101 1001))
             (iterate (for (x y z) :within-radius 1 :skip-origin t :origin (10 100 1000))
                      (collect (list x y z))))))
