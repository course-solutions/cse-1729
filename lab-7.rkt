(define (make-tree value left right ) (list value left right))
(define (value tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))

;; 1
(define (tree-size T)
  (if (null? T)
      0
      (+ 1 (tree-size (left T)) (tree-size (right T)))))

;; 2
(define (tree-depth T)
  (if (null? T)
      -1
      (+ 1 (max (tree-depth (left T)) (tree-depth (right T))))))

;; 3
(define (boolean->int b)
  (if b 1 0))

(define (count-pred P tree)
  (if (null? tree)
      0
      (+ (boolean->int (P (value tree)))
         (count-pred P (left tree))
         (count-pred P (right tree)))))

;; 4
(define (count-one-child T)
  (cond ((null? T) 0)
        ((and (null? (left T)) (null? (right T))) 0)
        ((null? (left T))
         (+ 1 (count-one-child (right T))))
        ((null? (right T))
         (+ 1 (count-one-child (left T))))
        (else (+ (count-one-child (left T))
                 (count-one-child (right T))))))

;; 5
(define (invert-bst T)
  (cond ((null? T) T)
        ((and (null? (left T)) (null? (right T))) T)
        ((make-tree (value T) (invert-bst (right T)) (invert-bst (left T))))))
