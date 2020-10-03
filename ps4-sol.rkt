;; 1
(define (harmonic n)
  (define (harmonic-helper result i)
    (if (> i n)
        result
        (harmonic-helper (+ result (/ 1 i)) (+ 1 i))))
  (harmonic-helper 0 1))

;; 2
;; (a)
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;; (b)
(define (wallis-pi n)
  (define (first n)
    (/ (* 2 n) (- (* 2 n) 1)))
  (define (second n)
    (/ (* 2 n) (+ 1 (* 2 n))))
  (define (calculate begin end result)
    (if (> begin end)
        result
        (calculate (+ 1 begin)
                   end
                   (* result (first begin) (second begin)))))
  (calculate 1 n 1))

;; 3
(define (frac-sum f g n)
  (define (calculate i result)
    (let* ((m (f i))
           (d (g i))
           (x (if (= 0 d)
                  0
                  (/ m d))))
      (if (> i n)
          result
          (calculate (+ i 1) (+ x result)))))
  (calculate (- n) 0))


;; 4
(define (der f h)
  (lambda (x)
    (/ (- (f (+ x h))
          (f x))
       h)))

(define (der-n f n h)
  (f h n))

;; 5
(define (newton f x n)
  (define df (der f .01))
  (define (newton-iter x k)
    (let ((new-x (- x
                    (/ (f x) (df x)))))
      (if (= k 0)
          x
          (newton-iter new-x (- k 1)))))
  (newton-iter x n))

;; 6
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (sum-term term a b)
  (sum term a (lambda (x) (+ x 1)) b))

(define (simpson-integrate f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (get-simpson-term k)
    (* (y k)
       (cond ((or (= k 0) (= k n)) 1)
             ((odd? k) 4)
             (else 2))))
  (* (/ h 3) (sum get-simpson-term 0 (lambda (x) (+ x 1)) n)))
