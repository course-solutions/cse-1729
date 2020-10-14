;; 1
(define (square-pair num)
  (cons num (* num num)))

;; 2
(define (rev p)
  (let ((first (car p))
        (second (cdr p)))
    (cons second first)))

;; 3
;; (a)
(define (c->p p)
  (let ((x (car p)) (y (cdr p)))
    (let ((r (sqrt (+ (* x x) (* y y))))
          (theta (atan (/ y x))))
      (cons r theta))))

;; (b)
(define (p->c p)
  (let ((r (car p))
        (theta (cdr p)))
    (let ((x (* r (cos theta)))
          (y (* r (sin theta))))
      (cons x y))))

;; 4
(define (y p1 p2)
  (let* ((x1 (car p1))
         (y1 (cdr p1))
         (x2 (car p2))
         (y2 (cdr p2))
         (m (/ (- y2 y1) (- x2 x1)))
         (b (- y1 (* m x1))))
    (lambda (x)
      (+ b (* m x)))))
