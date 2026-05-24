#lang sicp

(define (make-table)
  (list '*table*))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (lookup keys table)
  (if (null? keys)
      #f
      (let ((subtable (assoc (car keys) (cdr table))))
        (cond ((not subtable) #f)
              ((null? (cdr keys))
               (let ((value-record (assoc '*value* (cdr subtable))))
                 (if value-record
                     (cdr value-record)
                     #f)))
              (else (lookup (cdr keys) subtable))))))

(define (insert! keys value table)
  (define (iter keys table)
    (let* ((key (car keys))
           (subtable (assoc key (cdr table))))

      (if (null? (cdr keys))
          ;; final key
          (let ((target
                 (if subtable
                     subtable
                     (let ((new-subtable (list key)))
                       (set-cdr! table (cons new-subtable (cdr table)))
                       new-subtable))))

            (let ((value-record
                   (assoc '*value*
                          (cdr target))))
              (if value-record
                  (set-cdr! value-record value)
                  (set-cdr! target
                            (cons (cons '*value* value)
                                  (cdr target))))))

          ;; recurse deeper
          (let ((next-table
                 (if subtable
                     subtable
                     (let ((new-subtable (list key)))
                       (set-cdr! table (cons new-subtable (cdr table)))
                       new-subtable))))
            (iter (cdr keys) next-table)))))

  (iter keys table))

(define t (make-table))

(insert! '(a b c) 1 t)
(insert! '(a b d) 2 t)
(insert! '(x y) 9 t)

(lookup '(a b c) t) ; => 1
(lookup '(a b d) t) ; => 2
(lookup '(x y) t)   ; => 9
(lookup '(s b) t)