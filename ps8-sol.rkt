(define (make-tree value left right) (list value left right))
(define (value tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))

;; 1
(define (arithvalue T)
  (let ((v (value T))
        (left-expr (left T))
        (right-expr (right T)))
    (if (and (null? left-expr) (null? right-expr))
        v
        (cond
          ((eqv? v #\+) (+ (arithvalue left-expr) (arithvalue right-expr)))
          ((eqv? v #\-) (- (arithvalue left-expr)))
          ((eqv? v #\*) (* (arithvalue left-expr) (arithvalue right-expr)))
          ((eqv? v #\/) (/ 1 (arithvalue left-expr)))))))

;; 2
(define (sort-extract T)
  (cond
    ((null? T) '())
    (else
     (append
       (sort-extract (left T))
       (list (value T))
       (sort-extract (right T))))))

(define (delete-value v T)
  (define (last-element l)
    (cond
      ((null? (cdr l)) (car l))
      (else (last-element (cdr l)))))
  (define (remove-last t)
    (cond
      ((null? t) '())
      ((null? (right t)) (left t))
      (else
       (make-tree (value t) (left t) (remove-last (right t))))))
  (cond
    ((null? T) '())
    ((= v (value T))
     (cond
       ((and (null? (left T)) (null? (right T))) '())
       ((null? (left T)) (right T))
       ((null? (right T)) (left T))
       (else
        (make-tree
          (last-element (sort-extract (left T)))
          (remove-last (left T))
          (right T)))))
    ((> v (value T))
     (make-tree (value T) (left T) (delete-value v (right T))))
    ((< v (value T))
     (make-tree (value T) (delete-value v (left T)) (right T))
     v)))

;; 3
(define (swap! v i j)
  (define temp (vector-ref v i))
  (vector-set! v i (vector-ref v j))
  (vector-set! v j temp))
 
(define (sift-down! v start end)
  (let ((child (+ (* start 2) 1)))
    (cond
      ((> child end) 'done)
      (else
       (begin
         (and (and (<= (+ child 1) end)
                   (< (vector-ref v child) (vector-ref v (+ child 1))))
              (set! child (+ child 1)))
         (if (< (vector-ref v start) (vector-ref v child))
             (begin (swap! v start child) (sift-down! v child end))
             'done))))))
 
(define (heapify v)
  (define (iter v start)
    (if (>= start 0)
        (begin
          (sift-down! v start (- (vector-length v) 1))
          (iter v (- start 1)))
        'done))
  (iter v (quotient (- (vector-length v) 2) 2)))
 
(define (heapsort v)
  (define (iter v end)
    (if (zero? end)
        'done
        (begin
          (swap! v 0 end)
          (sift-down! v 0 (- end 1))
          (iter v (- end 1)))))
  (begin (heapify v) (iter v (- (vector-length v) 1))))

(define (hsort elements)
  (if (null? elements)
      '()
      (let ((elements (list->vector elements)))
        (heapsort elements)
        (vector->list elements))))

;; 4
;; (a)
(define (size1 H) (car (car H)))
(define (heap1 H) (cdr (car H)))
(define (size2 H) (car (cdr H)))
(define (heap2 H) (cdr (cdr H)))
(define (create-heap v H1 H2)  (list v H1 H2))
(define (h-min H) (car H))
(define (left H)  (cadr H))
(define (right H) (caddr H))
(define (combine-heaps H1 H2)
  (cond
    ((null? H1) H2)
    ((null? H2) H1)
    ((< (h-min H1) (h-min H2))
     (create-heap
       (h-min H1)
       H2
       (combine-heaps (left H1) (right H1))))
    (else
     (create-heap
       (h-min H2)
       H1
       (combine-heaps (left H2) (right H2))))))
(define (extract-min H) (combine-heaps (left H) (right H)))
(define (extract-max H) (combine-heaps (left H) (right H)))

(define (equalize-heaps heap-pair)
  (let* ((s1 (car (car heap-pair)))
         (s2 (car (cdr heap-pair)))
         (h1 (cdr (car heap-pair)))
         (h2 (cdr (cdr heap-pair))))
    (cond
      ((and (= s1 0) (= s2 0)) heap-pair)
      ((and (= s1 0) (> s2 1))
       (equalize-heaps
         (cons
           (cons (+ s1 1) (create-heap (h-min h2) '() '()))
           (cons (- s2 1) (extract-max h2)))))
      ((and (> s1 1) (= s2 0))
       (equalize-heaps
         (cons
           (cons (- s1 1) (extract-min h1))
           (cons (+ s2 1) (heap-insert < (h-min h1) '())))))
      ((<= (abs (- s1 s2)) 1) heap-pair)
      ((> s1 s2)
       (equalize-heaps
         (cons
           (cons (- s1 1) (extract-max h1))
           (cons (+ s2 1) (heap-insert < (h-min h1) h2)))))
      ((< s1 s2)
       (equalize-heaps
         (cons
           (cons (+ s1 1) (heap-insert > (h-min h2) h1))
           (cons (- s2 1) (extract-max h2))))))))

;; (b)
(define (max-f less a b) (if (less a b) b a))
(define (min-f less a b) (if (less a b) a b))

(define (heap-insert f x H)
  (if (null? H)
      (create-heap x '() '())
      (let ((child-value (max-f f x (h-min H)))
            (root-value (min-f f x (h-min H))))
        (create-heap
          root-value
          (right H)
          (heap-insert f child-value (left H))))))

(define (add-number x heap-pair)
  (cond
    ((or (and (= (size1 heap-pair) 0) (= (size2 heap-pair)) 0)
         (and (= (size1 heap-pair) 0) (not (= (size2 heap-pair)) 0)))
     (equalize-heaps
       (cons
         (cons (size1 heap-pair) (heap1 heap-pair))
         (cons
           (+ (size2 heap-pair) 1)
           (insert < x (heap2 heap-pair))))))
    ((< x (h-min (heap2 heap-pair)))
     (equalize-heaps
       (cons
         (cons
           (+ (size1 heap-pair) 1)
           (heap-insert > x (heap1 heap-pair)))
         (cons (size1 heap-pair) (heap2 heap-pair)))))
    (else
     (equalize-heaps
       (cons
         (cons (size1 heap-pair) (heap1 heap-pair))
         (cons
           (+ (size2 heap-pair) 1)
           (heap-insert < x (heap2 heap-pair))))))))

;; (c)
(define (get-median heap-pair)
  (let ((newheap (equalize-heaps heap-pair)))
    (cond
      ((= (size1 newheap) (size2 newheap))
       (/ (+ (h-min (heap1 newheap)) (h-min (heap2 newheap))) 2))
      ((> (size1 newheap) (size2 newheap))
       (h-min (heap1 newheap)))
      ((< (size1 newheap) (size2 newheap))
       (h-min (heap2 newheap))))))
