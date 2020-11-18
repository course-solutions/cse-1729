;; a
(define (fact n)
  (let ((i 1) (prod 1))
    (define (helper)
      (if (< i n)
          (begin (set! i (+ i 1)) (set! prod (* prod i)) (helper))
          '()))
    (begin (helper) prod)))

;; b
(define (new-account initial-balance)
  (let ((balance initial-balance) (rate 0.01))
    (define (deposit f)
      (begin (set! balance (+ balance f)) balance))
    (define (withdraw f)
      (begin (set! balance (- balance f)) balance))
    (define (accrue)
      (if (< balance 0)
          balance
          (begin
            (set! balance (+ balance (* balance rate)))
            balance)))
    (define (setrate newrate) (begin (set! rate newrate) rate))
    (define (bal-inq) balance)
    (lambda (method)
      (cond
        ((eq? method 'deposit) deposit)
        ((eq? method 'withdraw) withdraw)
        ((eq? method 'balance-inquire) bal-inq)
        ((eq? method 'accrue) accrue)
        ((eq? method 'setrate) setrate)))))
