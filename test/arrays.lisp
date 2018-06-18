(in-package :losh.test)


(define-test do-array
  (let ((a (make-array 4 :initial-contents '(1 2 3 4)))
        (mda (make-array (list 2 2) :initial-contents '((1 2) (3 4)))))
    (is (equal '(1 2 3 4)
               (gathering
                 (do-array (x a)
                   (gather x)))))
    (do-array (x mda)
      (incf x))
    (is (equal '(2 3 4 5)
               (gathering
                 (do-array (x mda)
                   (gather x)))))))

(define-test fill-multidimensional-array
  (let ((mda (make-array (list 2 2) :initial-contents '((1 2) (3 4)))))
    (is (equalp #2A((1 2) (3 4)) mda))
    (fill-multidimensional-array mda 9)
    (is (equalp #2A((9 9) (9 9)) mda)))

  (let ((mda (make-array (list 2 2)
               :initial-contents '((1 2) (3 4))
               :element-type t)))
    (is (equalp #2A((1 2) (3 4)) mda))
    (fill-multidimensional-array-t mda 9)
    (is (equalp #2A((9 9) (9 9)) mda)))

  (let ((mda (make-array (list 2 2)
               :initial-contents '((1 2) (3 4))
               :element-type 'fixnum)))
    (is (equalp #2A((1 2) (3 4)) mda))
    (fill-multidimensional-array-fixnum mda 9)
    (is (equalp #2A((9 9) (9 9)) mda)))

  (let ((mda (make-array (list 2 2)
               :initial-contents '((1.0 2.0) (3.0 4.0))
               :element-type 'single-float)))
    (is (equalp #2A((1.0 2.0) (3.0 4.0)) mda))
    (fill-multidimensional-array-single-float mda 9.0)
    (is (equalp #2A((9.0 9.0) (9.0 9.0)) mda))))

(define-test vector-last
  (is (equal '(nil nil)
             (multiple-value-list (vector-last #()))))
  (is (equal '(nil t)
             (multiple-value-list (vector-last #(nil)))))
  (let ((v (make-array 4
             :initial-contents '(1 2 3 4)
             :fill-pointer t)))
    (is (equal '(4 t)
               (multiple-value-list (vector-last v))))
    (setf (fill-pointer v) 2)
    (is (equal '(2 t)
               (multiple-value-list (vector-last v))))
    (setf (fill-pointer v) 0)
    (is (equal '(nil nil)
               (multiple-value-list (vector-last v))))))


(define-test bisect-left
  (let ((v #(10 12 14 16 18 20)))
    (is (equal '(14 2) (multiple-value-list (bisect-left #'<= v 15))))
    (is (equal '(14 2) (multiple-value-list (bisect-left #'<= v 14))))
    (is (equal '(20 5) (multiple-value-list (bisect-left #'<= v 999))))
    (is (equal '(10 0) (multiple-value-list (bisect-left #'<= v 11))))
    (is (equal '(nil nil) (multiple-value-list (bisect-left #'<= v 0))))))

(define-test bisect-right
  (let ((v #(10 12 14 16 18 20)))
    (is (equal '(16 3) (multiple-value-list (bisect-right #'<= v 15))))
    (is (equal '(16 3) (multiple-value-list (bisect-right #'<= v 14))))
    (is (equal '(nil nil) (multiple-value-list (bisect-right #'<= v 999))))
    (is (equal '(12 1) (multiple-value-list (bisect-right #'<= v 11))))
    (is (equal '(10 0) (multiple-value-list (bisect-right #'<= v 0))))))
