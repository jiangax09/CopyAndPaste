#lang sicp
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? (caar records) key) (car records))
        (else (assoc key (cdr records)))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))


(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table))))))

(define (make-table)
  (list '*table*))

;(define (fib1 n table)
;  (let ((rec (lookup n table)))
;    (if rec rec
;        (let ((res
;               (cond ((= n 0) 0)
;                     ((= n 1) 1)
;                     (else (+
;                            (fib1 (- n 1) table)
;                            (fib1 (- n 2) table))))))                    
;          (insert! n res table)
;          res))))

(define table (make-table))

(define (memorize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((prev-res (lookup x table)))
        (or prev-res
            (let ((res (f x)))
              (insert! x res table)
              res))))))

(define memorize-fib1
  (memorize (lambda (n)
              (cond ((= n 0) 0)
                    ((= n 1) 1)
                    (else (+ (memorize-fib1 (- n 1)) (memorize-fib1 (- n 2))))))))
              
(memorize-fib1 100)