;; 1
;; (a)
(define (harmonic n)
  (if (= 1 n)
      1
      (+ (harmonic (- n 1)) (/ 1 n))))

;; (b)
(define (eulerest n) (abs (- (harmonic n) (log n))))

;; 2
(define (prime? n)
  (define (divisor? k) (= 0 (modulo n k)))
  (define (divisors-upto k)
    (and (> k 1)
         (or (divisor? k)
             (divisors-upto (- k 1)))))
  (not (divisors-upto (- n 1))))

(define (count-primes m)
  (define (bool->int b) (if b 1 0))
  (if (= m 1)
      0
      (+ (bool->int (prime? m))
         (count-primes (- m 1)))))

;; 3
(define (rel-prime a b)
  (define (divides-both d)
    (and (= 0 (modulo a d))
         (= 0 (modulo b d))))
  (define (divisor-upto k)
    (and (> k 1)
         (or (divides-both k)
             (divisor-upto (- k 1)))))
  (not (divisor-upto (min a b))))

(define (count-rel-prime n)
  (define (bool->int b) (if b 1 0))
  (define (calculate count a b)
    (printf "a: ~a, b: ~a, result: ~a\n" a b (rel-prime a b))
    (cond
      ((and (= n a) (= n b))
       (+ count (bool->int (rel-prime a b))))
      ((and (< a n) (= n b))
       (calculate (+ count (bool->int (rel-prime a b))) (+ a 1) 1))
      (else
       (calculate (+ count (bool->int (rel-prime a b))) a (+ b 1)))))
  (calculate 0 1 1))

;; 4
;; (a)
(define (lucas n)
  (cond
    ((= n 0) 2)
    ((= n 1) 1)
    (else (+ (lucas (- n 1))
             (lucas (- n 2))))))

;; (b)
(define (rec-call-lucas n)
  (/ (lucas n) (lucas (- n 1))))

;; (c)
(define (fast-Lucas-help n k luc-a luc-b)
  (if (= n k)
      luc-a
      (fast-Lucas-help n (+ k 1) (+ luc-a luc-b) luc-a)))

(define (rec-call-fast-lucas-helper n) (fast-Lucas-help n 1 1 2))

;; 5
;; (a)
(define (P n)
  (if (= n 0)
      0
      (+ (H (- n 1)) (P (- n 1)))))

;; (b)
(define (H n)
  (if (= 0 n)
      1
      (+ (* 2 (P (- n 1))) (H (- n 1)))))

;; (c)
(define (t n)
  (define (even? n) (= 0 (remainder n 2)))
  (if (even? n)
      (* 2 (expt (P n) 2))
      (expt (H n) 2)))

;; (d)
(define (s n) (* (H n) (P n)))

;; (e)
(define (tri-square n)
  (let ((tn (t n)))
    (/ (* tn (+ 1 tn)) 2)))

;; (f)
(define (square-tri n)
  (expt (s n) 2))

;; 6
;; (a)
(define (golden n)
  (if (= 0 n)
      1
      (+ 1 (/ 1 (golden (- n 1))))))

;; (b)
(define (golden-sqrt n)
  (if (= 0 n)
      1
      (sqrt (+ 1 (golden-sqrt (- n 1))))))

;; 7
(define (explain-interval-sum m n)
  ( if (= m n)
       m
       (+ m
          (explain-interval-sum (+ m 1) (- n 1))
          n)))

;; 8
(define (ack m n)
  (cond
    ((= m 0) (+ 1 n))
    ((and (= n 0) (> m 0)) (ack (- m 1) 1))
    (else (ack (- m 1) (ack m (- n 1))))))

;; 9
(define (catalan n)
  (define (kth k)
    (/ (+ n k) k))
  (define (calculate step result)
    (cond
      ((> step n)
       result)
      ((or (= step 0)
           (= step 1))
       (calculate (+ 1 step)
                  result))
      (else
       (calculate (+ 1 step)
                  (* result (kth step))))))
  (calculate 0 1))
