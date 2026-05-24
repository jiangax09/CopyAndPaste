#lang sicp
(define (make-monitor f)
  (let ((cnt 0))
    (lambda (m)
        (cond 
          ((eq? m 'how-many-call) cnt)
          ((eq? m 'reset-count) ((set! cnt 0) cnt))
          (else (begin (set! cnt (+ 1 cnt))
                       (f m)))))))

(define (make-acc balance pd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
       "Insufficient fund"))
  (define (deposit amount)
    (set! balance (+ amount balance))
           balance)
  (define (dispatch pd0 m)
    (if (eq? pd0 pd)
        (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "unknown method")))
        (lambda (x) "incorrect password")))
  dispatch)

(define (make-acc-1 balance pd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
       "Insufficient fund"))
  (define (deposit amount)
    (set! balance (+ amount balance))
           balance)
  (define (call-police) (error "call-police"))
  (let ((error-cnt 0))
  (lambda (pd0 m n)
    (if (eq? pd0 pd)
        (begin 
        (cond
          ((eq? m 'withdraw) (withdraw n))
          ((eq? m 'deposit) (deposit n))
          (else (error "unknown method")))
        (set! error-cnt 0))
        (begin
               (if (> error-cnt 2)
                   (call-police)
                   (set! error-cnt (+ 1 error-cnt))
                   ))))))

(define (estmiate-pi trials)
  (sqrt (/ 6 (monte-carlo
                    trials c-test))))

(define (c-test) (= (gcd (random 10000) (random 10000)) 1))

(define (monte-carlo
                    trials experiment)
  (define (iter remaining passed)
    (cond ((= remaining 0)
           (/ passed trials))
          ((experiment)
           (iter (- remaining 1) (+ passed 1)))
          (else (iter (- remaining 1) passed))))
  (iter trials 0))

(define (new-rand seed)
  (define (generate) (begin
                       (set! seed (random seed))
                       seed))
  (define (reset val) (set! seed val))
  (define (dispatch method)
        (cond ((eq? method 'reset) reset)
              ((eq? method 'generate) generate)
              (else (error "unknown method"))))
  dispatch)


(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        (error "not enough")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error 'make-account "unknown request" m)))
        (lambda (x) "Incorrect password")))
  dispatch)

(define (make-joint-account acc pd0 new-pd)
  (lambda (pd m)
    (acc (if (eq? pd new-pd) pd0 #f) m)))
(define peter-acc (make-account 100 'pd0))
(define paul-acc (make-joint-account peter-acc 'pd0 'pd1))

(define (f y)
  (let ((x 0))
      (let ((old-x x))
        (set! x y)
        old-x)))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
  
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (detect-cycle x)
  (define (iter t1 t2)
    (cond ((or (null? t1) (null? t2)) #f)
          ((eq? t1 t2) #t)
          ((null? (cdr t2)) #f)
          (iter (cdr t1) (cdr (cdr t2)))))
  (iter (car x) (car x)))

(define (has-cycle? x)
  (define (iter slow fast)
    (cond ((or (null? fast) (null? (cdr fast))) #f) ; End of list reached, no cycle
          ((eq? slow fast) #t)                      ; Pointers met, cycle detected
          (else (iter (cdr slow) (cddr fast)))))    ; Move slow by 1, fast by 2
  
  ;; We must handle the very first step carefully to prevent 
  ;; slow and fast from triggering (eq? slow fast) immediately.
  (if (or (null? x) (null? (cdr x)))
      #f
      (iter x (cdr x))))