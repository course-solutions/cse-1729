;; 1
(define (g-sum f a b)
  (if (= a b)
      (f a)
      (+ (f b) (g-sum f a (- b 1)))))

(define (geom-series-np2 n)
  (define (geometric-term k)
    (/ 1 (expt 2 k)))
  (g-sum geometric-term 0 n))

;; 2
(define (num-digits n)
  (define (calculate count n)
    (if (= n 0)
        count
        (calculate (+ count 1) (quotient n 10))))
  (if (= n 0) 1 (calculate 0 n)))

;; 3
;; (a)
(define (a n)
  (define (calculate n result)
    (if (= n 0)
        result
        (calculate (- n 1) (* result 2))))
  (calculate n 1))

;; (b)
(define (num-ancestors n)
  (define (calculate n prev result)
    (if (= n 0)
        result
        (calculate (- n 1) (* 2 prev) (+ result (* 2 prev)))))
  (calculate n 1 0))

;; 4
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (n-choose-k n k)
  (if (or (< n k) (< k 0))
      0
      (/ (factorial n)
         (* (factorial (- n k))
            (factorial k)))))

;; 5
(define (pascals-triangle n k)
  (cond
    ((< k 0) 0)
    ((< n k) 0)
    ((= 0 k n) 1)
    (else (+ (pascals-triangle (- n 1) k)
             (pascals-triangle (- n 1) (- k 1))))))
