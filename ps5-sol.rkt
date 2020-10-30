;; 1
;; (a)
(define (encode p)
  (let ((x (car p))
        (y (cdr p)))
    (if (= x (max x y))
        (+ x y (* x x))
        (+ x (* y y)))))

;; (b)
(define (decode z)
  (define (pow2 x) (* x x))
  (let ((sqrtz (sqrt z)))
    (if (< (- z (pow2 (floor sqrtz))) (floor sqrtz))
        (cons (- z (pow2 (floor sqrtz))) (floor sqrtz))
        (cons (floor sqrtz) (- z (pow2 (floor sqrtz)) (floor sqrtz))))))

;; 2
;; (a)
(define (make-complex a b) (cons a b))
(define (real-coeff c) (car c))
(define (imag-coeff c) (cdr c))

(define (sub-complex c d)
  (make-complex (- (real-coeff c) (real-coeff d))
                (- (imag-coeff c) (imag-coeff d))))

;; (b)
(define (div-complex cc dd)
  (let ((a (real-coeff cc))
        (b (imag-coeff cc))
        (c (real-coeff dd))
        (d (imag-coeff dd)))
    (let ((ac (* a c))
          (bd (* b d))
          (bc (* b c))
          (ad (* a d))
          (c2 (* c c))
          (d2 (* d d)))
      (make-complex (/ (+ ac bd) (+ c2 d2))
                    (/ (- bc ad) (+ c2 d2))))))

;; 3
;; (a)
(define (neg-complex-num a) 
  (make-complex (- (car a)) 
                (- (cdr a))))
(define (sum-quadratic-roots a b c)
  (div-complex (neg-complex-num b) a))

(define (prod-quadratic-roots a b c)
  (div-complex c a))

;; (b)
(define (sum-cubic-roots a b c d)
  (div-complex (neg-complex-num b) a))
(define (sum-pairs-cubic-roots a b c d)
  (div-complex c a))
(define (prod-cubic-roots a b c d)
  (div-complex (neg-complex-num d) a))

;; 4
(define (unzip list)
  (let ((a (map car list))
        (b (map cdr list)))
    (cons a b)))

;; 5
(define (zip list1 list2)
  (map cons list1 list2))
