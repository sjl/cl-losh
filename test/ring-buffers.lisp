(in-package :losh.test)

(defun check-ring-buffer (rb expected-contents)
  ;; rb-contents
  (is (equal expected-contents (rb-contents rb)))
  ;; rb-count
  (is (= (length expected-contents) (rb-count rb)))
  ;; rb-empty
  (if (null expected-contents)
    (is (rb-empty-p rb))
    (is (not (rb-empty-p rb))))
  ;; rb-full
  (if (= (length expected-contents) (1- (rb-size rb)))
    (is (rb-full-p rb))
    (is (not (rb-full-p rb))))
  ;; do-ring-buffer
  (let ((contents expected-contents))
    (do-ring-buffer (val rb)
      (is (equal (pop contents) val)))
    (is (null contents)))
  ;; iterate driver
  (let ((contents expected-contents))
    (iterate (for val :in-ring-buffer rb)
             (is (equal (pop contents) val)))
    (is (null contents)))
  ;; rb-ref
  (iterate (for val :in expected-contents)
           (for i :from 0)
           (is (equal val (rb-ref rb i))))
  (iterate (for val :in (reverse expected-contents))
           (for i :downfrom -1)
           (is (equal val (rb-ref rb i)))))

(define-test basic-ring-buffers
  (let ((rb (make-ring-buffer :size 4)))
    (check-ring-buffer rb '())
    (is (= 4 (rb-size rb)))

    (rb-push rb 'a)             (check-ring-buffer rb '(a))
    (rb-push rb 'b)             (check-ring-buffer rb '(a b))
    (rb-push rb 'c)             (check-ring-buffer rb '(a b c))
    (rb-push rb 'd)             (check-ring-buffer rb '(b c d))
    (rb-push rb 'e)             (check-ring-buffer rb '(c d e))
    (rb-push rb 'f)             (check-ring-buffer rb '(d e f))
    (rb-push rb 'g)             (check-ring-buffer rb '(e f g))
    (is (eql 'e (rb-pop rb)))   (check-ring-buffer rb '(f g))
    (is (eql 'f (rb-pop rb)))   (check-ring-buffer rb '(g))
    (is (eql 'g (rb-pop rb)))   (check-ring-buffer rb '())

    (signals error (rb-pop rb))
    (check-ring-buffer rb '())

    (rb-safe-push rb 'a)
    (rb-safe-push rb 'b)
    (rb-safe-push rb 'c)
    (is (= 4 (rb-size rb)))
    (check-ring-buffer rb '(a b c))
    (signals error (rb-safe-push rb 'd))))

(define-test fuzz-ring-buffers
  (do-range ((n 2 30))
    (iterate
      (with rb = (make-ring-buffer :size n))
      (with data = (coerce (0... 400) 'vector))
      (with i = 0)
      (repeat 400)

      ;; Randomly push/pop (but never try to pop if empty).
      (if (or (rb-empty-p rb) (randomp 0.7))
        (progn (rb-push rb (aref data i))
               (incf i))
        (rb-pop rb))

      (for expected = (coerce (subseq data (- i (rb-count rb)) i) 'list))
      (check-ring-buffer rb expected))))
