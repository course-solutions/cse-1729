;; 1
(define (ins x l)
  (cond
    ((or (null? l) (<= x (car l))) (cons x l))
    (else (cons (car l) (ins x (cdr l))))))

;; 2
(define (insSort l)
  (if (null? l)
      '()
      (ins (car l) (insSort (cdr l)))))

;; 3
;; (a)
(define (fold-right op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (fold-right op initial (cdr sequence)))))

;; (b)
(define (fold-left op initial sequence)
  (fold-right op initial (reverse sequence)))

;; (c)
(define (my-map p sequence)
  (fold-right (lambda (x y) (cons (p x) y)) '() sequence))

;; (d)
(define (my-append seq1 seq2)
  (fold-right cons seq2 seq1))

;; (e)
(define (my-length sequence)
  (fold-right (lambda (x acc) (+ 1 acc)) 0 sequence))

;; (f)
(define (reverse-r sequence)
  (fold-right (lambda (x y) (my-append y (list x))) '() sequence))

;; (g)
(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons x y)) '() sequence))

;; (h)
(define (horner-eval x coefficient-sequence)
    (fold-right (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
                0
                coefficient-sequence))

;; 4
;; (a)
(define (prime? n)
  (define (divisor? k) (= 0 (modulo n k)))
  (define (divisors-upto k)
    (and (> k 1) (or (divisor? k) (divisors-upto (- k 1)))))
  (if (< n 2)
      #f
      (not (divisors-upto (- n 1)))))

(define (all lst)
  (or (null? lst) (and (car lst) (all (cdr lst)))))

(define (left-truncatable-prime? p)
  (define (helper n e)
    (if (>= e n)
        '()
        (cons (remainder n e)
              (helper n (* 10 e)))))
  (and (prime? p)
       (all (map prime? (helper p 10)))))

(define (nth-left-trunc-prime n)
  (define (helper start n)
    (cond ((= n 0) start)
          ((left-truncatable-prime? start) (helper (+ 1 start) (- n 1)))
          (else (helper (+ 1 start) n))))
  (- (helper 0 n) 1))

;; (b)
(define (right-truncatable-prime? p)
  (if (< p 10)
      (prime? p)
      (and (prime? p)
           (right-truncatable-prime? (quotient p 10)))))

(define (nth-right-trunc-prime n)
  (define (helper start n)
    (cond ((= n 0) start)
          ((right-truncatable-prime? start) (helper (+ 1 start) (- n 1)))
          (else (helper (+ 1 start) n))))
  (- (helper 0 n) 1))

;; (c)
(define (two-sided-prime? p)
  (and (left-truncatable-prime? p) (right-truncatable-prime? p)))

(define (nth-two-sided-prime n)
  (define (helper start n)
    (cond ((= n 0) start)
          ((two-sided-prime? start) (helper (+ 1 start) (- n 1)))
          (else (helper (+ 1 start) n))))
  (- (helper 0 n) 1))

;; 5
(define (make-tree value left right) (list value left right))
(define (value tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))

(define (tree-map tree f)
    (cond ((null? tree) '())
          ((not (pair? tree)) (f tree))
          (else (cons (tree-map (car tree) f)
                      (tree-map (cdr tree) f)))))

;; 6
(define (tree-equal? t1 t2)
  (cond ((and (null? t1) (null? t2)) #t)
        ((null? t1) #f)
        ((null? t2) #f)
        ((and (= (value t1) (value t2))
              (tree-equal? (left t1) (left t2))
              (tree-equal? (right t1) (right t2)))
          #t)
        (else #f)))

;; 7
(define (insert-list L T)
  (define (insert n T)
    (cond ((null? T) (make-tree n '() '()))
          ((= n (value T)) T)
          ((> n (value T)) (make-tree (value T) (left T) (insert n (right T))))
          ((< n (value T)) (make-tree (value T) (insert n (left T)) (right T)))))
  (cond ((null? L) T)
         (else (insert-list (cdr L) (insert (car L) T)))))

(define (sort-extract T)
  (cond ((null? T) '())
        (else (append (sort-extract (left T))
              (list (value T))
              (sort-extract (right T))))))

(define (tree-sort l) (sort-extract (insert-list l '())))
