#lang sicp
(define (make-table my-cmp)
  (define (assoc key records)
  (cond ((null? records) false)
        ((my-cmp (caar records) key) (car records))
        
        (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
  (define (lookup k1 k2)
   (let ((subtable (assoc k1 (cdr local-table))))
    (if subtable
        (let ((record (assoc k2 (cdr subtable))))
           (if record
               (cdr record)
               #f))
        #f)))
    
  (define (insert! k1 k2 value)
   (let ((subtable (assoc k1 (cdr local-table))))
    (if subtable
        (let ((record (assoc k2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable (cons (cons k2 value) (cdr subtable)))))
        (set-cdr! local-table
                  (cons (list k1 (cons k2 value))
                        (cdr local-table))))))    
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown Method"))))
    dispatch))

(define op-table (make-table (lambda (x y) (<= (abs (- x y)) 1))))
((op-table 'lookup) 0 0)
((op-table 'insert!) 0 0 'a)
((op-table 'lookup) 0 1)
((op-table 'lookup) 0 2)



    
  