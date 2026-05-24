#lang sicp

; (k . (v . (l . r . '())))

(define (assoc key table)
    (cond
      ((null? table) '())
      ((equal? key (car table)) table)
      ((< key (car table)) (assoc key (caddr table)))
      (else (assoc key (cadddr table)))))

(define (lookup key table)
  (let ((rec (assoc key table)))
    (if (null? rec) #f
        (cadr rec))))

(define (make-node k v l r) (list k v l r))

(define (left t) (caddr t))
(define (right t) (cadddr t))

(define (insert! key value table)
  (cond
    ((equal? key (car table)) (set-car! (cdr table) value))
    ((< key (car table))
     (if (null? (left table)) (set-car! (cddr table) (make-node key value '() '()))
         (insert! key value (car (cdr (cdr table))))))
    (else
     (if (null? (right table)) (set-car! (cdddr table) (make-node key value '() '()))
     (insert! key value (cdr (cdr (cdr table))))))))


(define t (make-node 999 'z '() '()))

(insert! 2 'a t)
(insert! 4 'b t)

(lookup 2 t)
(lookup 13 t)