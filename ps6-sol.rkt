#lang racket

;; 1
(define (change k l)
  (cond ((= k 0) 1)
        ((or (< k 0) (null? l)) 0)
        (else
         (+ (change k (cdr l))
            (change (- k (car l)) l)))))

;; 2

;; 3
(define (rle l)
  (define (rle-helper l x)
    (if (null? l)
        (reverse x)
        (if (= (car l) (car (car x)))
            (let* ((y (append (list (list (car (car x)) (+ (car (cdr (car x))) 1))) (cdr x))))
              (rle-helper (cdr l) y))
            (let* ((z (append (list (list (car l) 1)) x)))
              (rle-helper (cdr l) z)))))
  (if (null? l)
      '()
      (map (lambda (x) (cons (cadr x) (car x))) (rle-helper (cdr l) (list (list (car l) 1))))))

;; 4


;; 5
;; (a)
(define (list-sum l)
  (if (null? l)
      0
      (+ (car l) (list-sum (cdr l)))))

;; (b)
(define (average l)
  (if (null? l)
      0
      (/ (list-sum l) (length l))))

;; (c)
(define (var-map l)
  (map (lambda (x) (expt (- x (average l)) 2)) l))

;; (d)
(define (stdev l)
  (sqrt (average (var-map l))))

;; (e)
(define (map2 f x y)
  (if (or (null? x) (null? y))
      '()
      (cons (f (car x) (car y))
            (map2 f (cdr x) (cdr y)))))

;; (f)
(define (covar-elements l1 l2)
  (map2 (lambda (x y) (* (- x (average l1)) (- y (average l2)))) l1 l2))

;; (g)
(define (pearson l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (/ (average (covar-elements l1 l2))
         (* (stdev l1) (stdev l2)))))

;; 6
;; (a)
(define (best-fit X Y)
  (let* ((r (pearson X Y))
         (b (* r (/ (stdev Y) (stdev X)))))
    (cons (- (average Y) (* b (average X))) b)))

;; (b)
(define (best-fit-fn pX pY)
  (let* ((p (best-fit pX pY))
         (a (car p))
         (b (cdr p)))
    (lambda (x) (+ (* b x) a))))

;; 7
(define (merge la lb )
  (cond ((null? la ) lb )
        ((null? lb ) la )
        ((< (car la )(car lb ))
         (cons (car la )(merge (cdr la ) lb )))
        (else
         (cons (car lb )(merge la (cdr lb ))))))

;; 8
(define (split lst)
  (cond ((= (length lst) 1) (list (cons (car lst) '()) '()))
        (else
         (let ((mid (truncate (/ (length lst) 2))))
           (define (1sthalf lst i)
             (cond ((= i 0) '())
                   ((= i 1) (list (car lst)))
                   (else (cons (car lst) (1sthalf (cdr lst) (- i 1))))))
           (define (2ndhalf lst i)
             (cond ((= i 0) '())
                   ((= i 1) (cdr lst))
                   (else (2ndhalf (cdr lst) (- i 1)))))
           (list (1sthalf lst mid) (2ndhalf lst mid))))))

(define (mergeSort l)
  (cond ((null? l) '())
        ((null? (cdr l)) l)
        (else (merge (mergeSort (car (split l))) (mergeSort (cadr (split l)))))))
