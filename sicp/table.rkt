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

(define (lookup-2 k1 k2 table)
  (let ((subtable (assoc k1 (cdr table))))
    (if subtable
        (let ((record (assoc k2 (cdr subtable))))
           (if record
               (cdr record)
               false))
        false)))


(define (insert2! k1 k2 value table)
  (let ((subtable (assoc k1 (cdr table))))
    (if subtable
        (let ((record (assoc k2 subtable)))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable (cons (cons k2 value) (cdr subtable)))))
        (set-cdr! table (cons (list k1 (cons k2 value)) (cdr table))))))

(define op-table (make-table))
(lookup-2  0 0 op-table)
(insert2! 0 0 'a op-table)
(lookup-2  0 0 op-table)
;((op-table 'lookup) 0 2)
  