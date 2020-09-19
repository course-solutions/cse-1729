;; 1
(define (fizzbuzz x)
  (cond
    ((and (= 0 (remainder x 3)) (= 0 (remainder x 5))) "fizzbuzz")
    ((= 0 (remainder x 3)) "fizz")
    ((= 0 (remainder x 5)) "buzz")
    (else x)))

(define (fizz x)
  (if (= 0 (remainder x 3))
      "fizz"
      (number->string x 10)))

(define (buzz x)
  (if (= 0 (remainder x 5))
      "buzz"
      (number->string x 10)))

(define (fizzbuzz2 x)
  (string-append "fizz is " (fizz x) " buzz is " (buzz x)))

;; 2
(define (piecewise x)
  (define π 3.142)
  (cond
    ((> x (* 2 π))
     (- x (* 2 π)))
    ((< x (- π))
     (- (+ x π)))
    (else (sin x))))

;; 3
;; https://misc.douban.com/vpn/douban.pac
(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (add n m)
  (if (= n 0)
      m
      (add (dec n) (inc m))))

;; 4
(define (mult n m)
  (define (calculate product multiplier freq)
    (if (= 0 freq)
        product
        (calculate (add product multiplier) multiplier (dec freq))))
  (calculate 0 m n))

;; 5
(define (power b n)
  (define (calculate base p freq)
    (if (= 0 freq)
        base
        (calculate (mult base p) p (dec freq))))
  (calculate 1 b n))

;; 6
(define (raise x n)
  (define (calculate base p freq)
    (if (= 0 freq)
        base
        (if (even? freq)
            (let ((mid (calculate base p (floor (/ freq 2)))))
              (mult mid mid))
            (let ((mid (calculate base p (floor (/ freq 2)))))
              (mult (mult mid mid) p)))))
  (calculate 1 x n))

;; 7
(define (sumEven n)
  (if (<= n 0)
      0
      (if (even? n)
          (+ n (sumEven (- n 2)))
          (sumEven (- n 1)))))

(define (sumOdd n)
  (if (<= n 0)
      0
      (if (odd? n)
          (+ n (sumOdd (- n 2)))
          (sumOdd (- n 1)))))

;; 8
(define (h-product k)
  (if (= 1 k)
      1
      (* (- 1 (/ 1 k)) (h-product (- k 1)) )))

;; 9
(define (divides a b) (= 0 (modulo b a)))

(define (divisors-upto n k)
  (cond
    ((= k 0) 0)
    ((= n 0) 0)
    ((= k 1) 1)
    (else (if (divides k n)
              (+ 1 (divisors-upto n (- k 1)))
              (divisors-upto n (- k 1))))))

(define (divisors n)
  (divisors-upto n n))

;; 10
(define (subfact n)
  (cond
    ((= n 1) 0)
    ((= n 0) 1)
    (else (* (- n 1)
             (+ (subfact (- n 1))
                (subfact (- n 2)))))))

;; 11
(define (factorial n)
  (define (calculate acc n)
    (if (<= n 0)
        acc
        (calculate (* acc n) (- n 1))))
  (calculate 1 n))


(define (new-cos x n)
  (define (calculate acc x n)
    (if (= 0 n)
        acc
        (let* ((double-n (+ n n))
               (a (expt x double-n))
               (b (factorial double-n)))
          (calculate ((if (odd? n) - +) acc (/ a b)) x (- n 1)))))
  (calculate 1 x n))
