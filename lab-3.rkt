#lang racket
;; 1
;; (a)
(define (pell n)
  (define (pell-helper a b cur)
    (if (= cur n)
        b
        (pell-helper b (+ a (* 2 b)) (+ 1 cur))))
  (pell-helper 0 1 1))

(define (find-pell n)
  (define (finder x)
    (let ((px (pell x)))
      (if (< px n)
          px
          (finder (- x 1)))))
  (finder (- n 1)))

;; (c)
(define (comp-pell n)
  (cond
    ((= 0 n) 2)
    ((= 1 n) 2)
    (else
     (+ (* 2 (comp-pell (- n 1))) (comp-pell (- n 2))))))

;; (d)
(define (sqrt-2-approx n)
  (/ (/ (comp-pell n) 2) (pell n)))

;; 2
(define (viete_helper prod v i n)
  (if (= i n)
      prod
      (viete_helper (* prod 0.5 v) (sqrt (+ 2 v)) (+ i 1) n)))

(define (viete n)
  (viete_helper 1 (sqrt 2) 1 n))

;; 3
(define (new-sqrt x n)
  (define (fn n)
    (cond ((= n 1) (/ (- x 1) 2))
          ((/ (- x 1) (+ 2 (fn (- n 1)))))))
  (+ 1 (fn n)))

;; 4
(define (m91 n)
  (if (> n 100)
      (- n 10)
      (m91 (m91 (+ n 11)))))
