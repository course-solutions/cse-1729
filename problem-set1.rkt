#lang racket

;; Problem 1
; (a)
(* (+ 22 42) (* 54 99))

; (b)
(* 99 (* 54 (+ 22 42)))

; (c)
(+ (* 64 102) (* 16 (/ 44 22)))

; (d)
(define limerick (+ (/ (+ 12 144 20 (* 3 (sqrt 4)))
                       7)
                    (* 5 11)))


;; Problem 2
;; 2.1: The order of operations is different. Observe how this changes the parenthetical structure of theSCHEME expression
;; 2.2: No. Any such arithmetic expression determines unambiguously which operator to apply first; the samecan be said of all subexpressions!

;; Problem 3
; (a)
(define (inc x) (+ x 1))
; (b)
(define (inc2 x) (+ x 2))
; (c)
(define (cube x) (expt x 3))
; (d)
(define (p x)
  (expt (+ (expt x 5)
           (* 16 (expt x 4))
           (* 22 (expt x 3))
           x
           9)
        2))
; (e)
(define (ninth x) (cube (cube x)))
; (f)
(define (eighty-first x) (ninth (ninth x)))

;; Problem 4
;; Without abstraction (that is, using a subordinate function), this would have required us to write 64 occur-rences of the variable x

;; Problem 5
(define (isbn10-checkdigit x10 x9 x8 x7 x6 x5 x4 x3 x2)
  (modulo (- 11
             (modulo (+ (* 10 x10)
                        (* 9 x9)
                        (* 8 x8)
                        (* 7 x7)
                        (* 6 x6)
                        (* 5 x5)
                        (* 4 x4)
                        (* 3 x3)
                        (* 2 x2))
                     11))
          11))

(define (is-isbn10? x10 x9 x8 x7 x6 x5 x4 x3 x2 x1)
  (= x1 (isbn10-checkdigit x10 x9 x8 x7 x6 x5 x4 x3 x2)))

;; Problem 6
(define (fspiral theta)
  (expt 1.618 (* theta (/ 2 3.142))))

;; Problem 7
(define (root1 a b c)
  (/ (+ (- b)
        (sqrt (- (expt b 2)
                 (* 4 a c))))
     (* 2 a)))

(define (root2 a b c)
  (/ (- (- b)
        (sqrt (- (expt b 2)
                 (* 4 a c))))
     (* 2 a)))

(define (number-of-roots a b c)
  (if (= (expt b 2)
         (* 4 a c))
      1
      2))

(define (real-roots? a b c)
  (>= (expt b 2)
      (* 4 a c)))

;; Problem 8

(define (slant-height h a)
  (let ([r (/ a 2)])
    (sqrt (+ (* h h) (* r r)))))

(define (side-area h a)
  (let ([s (slant-height h a)])
    (/ (* s a) 2)))

(define (pyramid-area h a)
  (let ([triangular-area (side-area h a)])
       (* 4 triangular-area)))

(define (pyramid-volume h a)
  (/ (* a a h) 3))
