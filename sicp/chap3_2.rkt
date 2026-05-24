#lang sicp

(define front-ptr car)
(define rear-ptr cdr)
(define set-front-ptr! set-car!)
(define set-rear-ptr! set-cdr!)

(define (empty-queue? q) (null? (front-ptr q)))
(define (make-queue) (cons '() '()))

(define (front-queue q)
  (if (empty-queue? q) (error "front called on an empty queue")
      (car (front-ptr q))))


(define (delete-queue q)
  (cond ((empty-queue? q) (error "delete on an empty queue"))
        (else (set-front-ptr! q (cdr (front-ptr q)))
         q)))

(define (insert-queue q elem)
  (let ((new-pair (cons elem '())))
    (cond ((empty-queue? q)
          (set-front-ptr! q new-pair)
          (set-rear-ptr! q new-pair)
          q)
          (else
           (set-cdr! (rear-ptr q) new-pair)
           (set-rear-ptr! q new-pair)
           q))))

(define (print-queue q)
  (display (front-ptr q))
  (newline))
  