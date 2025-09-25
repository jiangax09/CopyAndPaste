(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))


(defun bref (buf n)
  (svref (buf-vec buf) (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
               (mod n (length (buf-vec buf))))
        val))

(defun new-buf (len)
  (make-buf :vec (make-array len)))


(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x))

(defun buf-pop (b)
  (prog1
      (bref b (incf (buf-start b)))
    (setf (buf-used b) (buf-start b) (buf-new b) (buf-end b))))


(defun buf-next (b)
  (when (< (buf-used b) (buf-end b))
    (bref b (incf (buf-used b)))))


(defun buf-reset (b)
  (setf (buf-used b) (buf-start b) (buf-new b) (buf-end b)))


(defun buf-clear (b)
  (setf (buf-used b) -1 (buf-start b) -1 (buf-new b) -1 (buf-end b) -1))


(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))

(defun stream-subst (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((char= c (char old pos))
             (incf pos)
             (cond ((= pos len)
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)
                    (buf-insert c buf))))
            ((zerop pos)
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t
             (unless from-buf (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))

(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output :if-exists :supersede)
      (stream-subst old new in out))))

(defun copy-file-lines-truncate-at-percent (input-path output-path)
  "Copy INPUT-PATH to OUTPUT-PATH line by line.
If a line contains %, keep only text before the first %."
  (with-open-file (in input-path :direction :input)
    (with-open-file (out output-path :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
      (do ((line (read-line in nil :eof)
                 (read-line in nil :eof)))
          ((eq line :eof))
        (let* ((pos (position #\% line)))
          (write-line (if pos
                          (subseq line 0 pos) ; everything before %
                          line)
                      out))))))


(defun skip-comments (input-name output-name)
  (with-open-file (in input-name :direction :input )
    (with-open-file (out output-name :direction :output :if-exists :supersede :if-does-not-exist :create)
      (do ((line (read-line in nil :eof)
                 (read-line in nil :eof)))
          ((eq line :eof))
        (let* ((pos (position #\% line)))
           (write-line
            (if pos
                (subseq line 0 pos)
                line)
            out))))))
;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(defun ge (a b)
  (if (> a b) a b))

(defun ch2_8 (n)
  (if (eql n 0)
      'done
      (progn (print ".")
             (ch2_8 (- n 1)))))

(defun ch2_8_1 (n)
  (do ((i 0 (+ i 1)))
      ((> i n) 'done)
    (print ".")))

(defun cnt(lst elem)
  (if (null lst)
      0
      (if (eql (car lst) elem)
          (+ 1 (cnt (cdr lst) elem))
          (cnt (cdr lst) elem)
      )
      ))

;; (load "library.lisp")
(defun our-assoc (key alist)
  (and (consp alist)
       (let ((pair (car alist)))
             (if (eql key (car pair))
                 pair
                 (our-assoc key (cdr alist))))))

(defun new-union-impl (l1 l2)
  (if (null l2)
      l1
      (let ((e2 (car l2)))
        (if (member e2 l1)
            (new-union-impl l1 (cdr l2))
            (new-union-impl (cons e2 l1) (cdr l2))))
      ))

(defun new-union (l1 l2) (new-union-impl (reverse l1) l2))

(defun occ-1 (lst elem)
  (if (null lst)
      0
      (if (eql (car lst) elem)
          (+ 1 (occ-1 (cdr lst) elem))
          (occ-1 (cdr lst) elem))))

(defun pos++ (lst)
  (let ((result '())
        (position 0))
    (dolist (elem lst)
      (cons result (+ elem position))
      (incf position))
    result))

(defun temp-f (start end acc)
  (if (>= start end)
      (reverse acc)
      (temp-f (+ 1 start) end (cons start acc))
      )
  )

(defun print-dot-notation (lst)
  (cond
    ((null lst)
     (princ "NIL"))
    ((atom lst)
     (princ lst))
    (t
     (princ "(")
     (print-dot-notation (car lst))
     (princ " . ")
     (print-dot-notation (cdr lst))
     (princ ")"))))


(defstruct (node3) val (a nil) (b nil) (c nil))
(defun copy-tree-3 (obj tree)
  (if (null tree)
      nil
      (let ((curr (node3-val tree)))
        (make-node3
         :val curr
         :a (copy-tree-3 obj (node3-a tree))
         :b (copy-tree-3 obj (node3-b tree))
         :c (copy-tree-3 obj (node3-c tree))
    ))))


(defun diff-1 (lst)
  (if (<= (length lst) 2)
      t
      (do* ((i 1 (+ i 1))
        (prev 0 curr)
        (curr (aref lst 0) (aref lst i))
        (result t (and result (eql 1 (abs (- curr prev))))))
       ((>= i (- (length lst) 1)) result))))

(defun diff-2 (lst)
    (if (< (length lst) 2)
        t
        (do ((i 0 (1+ i)))
            ((= i (1- (length lst))) t)
          (unless (= 1 (abs (- (aref lst (1+ i)) (aref lst i))))
            (return nil)))))

;;Find first non-positive number
(defun f1-1 (lst)
  (do ((i 0 (1+ i)))
      ((= i (length lst)) nil)
      (unless (> 0 (aref lst i)) (return i))))

;;  Validate all elements are even
(defun unless-2 (lst)
  (if (null lst)
      nil
      (do ((i 0 (1+ i)))
          ((= i (length lst)) t)
        (unless (evenp (aref lst 2)) (return nil)))))


;; Count until condition fails
;; "Count how many elements from start are in ascending order"
(defun unless-3 (lst)
  (if (< (length lst) 2)
      (length lst)
      (do ((i 1 (1+ i))
           (count 1 (1+ count)))
          ((= i (length lst)) count)
        (unless (>= (aref lst i) (aref lst (- i 1))) (return count)))))


;; Example 4: Find first string longer than N characters
(defun unless-4 (strs n)
  (do ((remains strs (cdr strs)))
      ((null remains) nil)
    (when (> (length (car remains)) n) (return (car remains)))))

;; Example 5: Validate parentheses are balanced
(defun when-1 (lst)
  (if (null lst)
      t
      (do* ((i 0 (1+ i))
            (cnt 0 (cond
                     ((char= (aref lst i) #\() (1+ cnt))
                     ((char= (aref lst i) #\)) (1- cnt)))))
           ((>= i (length lst)) (= cnt 0))
        (when (> 0 cnt) (return nil)))))

;; Example 6: Process array until error condition
(defun safe-divide-array (arr divisor)
  "Divide each element by divisor, stop on zero divisor"
  (let ((result (make-array (length arr))))
    (do ((i 0 (1+ i)))
        ((= i (length arr)) result)
      (unless (/= divisor 0)  ; unless divisor is non-zero
        (error "Division by zero at position ~a" i))
      (setf (aref result i) (/ (aref arr i) divisor)))))


(defun test-unless-examples ()
  (format t "=== Testing UNLESS in DO loops ===~%~%")

  ;; Test 1
;;  (format t "1. Find first non-positive in #(5 3 -2 8):~%")
;;  (format t "   Result: ~a~%~%" (find-first-non-positive #(5 3 -2 8)))
;;
;;  ;; Test 2
;;  (format t "2. All even? #(2 4 6 8): ~a~%" (all-even-p #(2 4 6 8)))
;;  (format t "   All even? #(2 4 7 8): ~a~%~%" (all-even-p #(2 4 7 8)))
;;
;;  ;; Test 3
;;  (format t "3. Count ascending in #(1 2 3 2 4 5):~%")
;;  (format t "   Result: ~a~%~%" (count-ascending #(1 2 3 2 4 5)))
;;
;;  ;; Test 4
;;  (format t "4. Find string longer than 3 chars:~%")
;;  (format t "   Result: ~a~%~%"
;;          (find-long-string '("hi" "bye" "hello" "ok") 3))
;;
;;  ;; Test 5
;;  (format t "5. Balanced parens?~%")
;;  (format t "   '((()))': ~a~%" (balanced-parens-p "((()))"))
;;  (format t "   '(()': ~a~%~%" (balanced-parens-p "(()"))
;;
;;  ;; Test 6
;;  (format t "6. Find gap in sequence #(1 2 3 5 6):~%")
;;  (format t "   Gap at position: ~a~%~%" (find-gap #(1 2 3 5 6)))
;;
;;  ;; Test 7
;;  (format t "7. Sum while positive #(5 3 8 -2 4):~%")
;;  (format t "   Sum: ~a~%~%" (sum-while-positive #(5 3 8 -2 4)))
;;
;;  ;; Test 8
;;  (format t "8. Find valid number (positive, even, <100) in #(1 3 -4 24 150):~%")
;;  (format t "   Result: ~a~%~%" (find-valid-number #(1 3 -4 24 150)))
;;
;;  ;; Test 9
;;  (format t "9. Validate grades #(85 92 105 88):~%")
;;  (let ((result (validate-student-grades #(85 92 105 88))))
;;    (format t "   ~a~%" (if (eq result t) "All valid"
;;                            (format nil "Invalid at position ~a" result)))))
)
;; (test-unless-examples)

(defun tt1 (lst)
  (do ((remains lst (cdr remains))
       (prev nil (car remains)))
      ((null remains) nil)
    (format t "~A~%" prev)))

(defun diff-1-m1 (lst)
  (block check-diff
    (mapc (lambda (curr prev)
            (unless (= 1 (abs (- curr prev)))
              (return-from check-diff nil))) (cdr lst) lst) t))

(defun diff-2-m1 (lst)
  (if (< (length lst) 2)
      lst  ; return the list (like mapc would)
      (let ((valid t))
        (mapc (lambda (current next)
                (unless (= 1 (abs (- next current)))
                  (setf valid nil)))
              lst (cdr lst))
        (if valid lst nil))))


(defun hypothetical-modifying-version (lst)
  (block check
    (mapc (lambda (x)
            (setf (car lst) 999)  ; Modify the list (hypothetically)
            (when (> x 10)
              (return-from check nil)))
          lst)
    lst))

(defun mm1 (lst)
  (if (null lst)
      nil
      (let ((m1 (first lst))
            (m2 (first lst)))
        (dolist (elem (cdr lst))
          (when (< elem m1) (setf m1 elem))
          (when (> elem m2) (setf m2 elem)))
        (values m1 m2))))

(defun ff1 (&rest args)
  (if args
      (+ 1 (apply #'ff1 (cdr args)))
      0))


(defun remove-if (fn lst)
  (lambda (fn lst) (filter fn lst)))

(defun ff1-debug (&rest args)
  (format t "Called with args: ~a~%" args)
  (if args
      (progn
        (format t "Recursing with: ~a~%" (cdr args))
        (+ 1 (ff1-debug (cdr args))))
      0))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setf wins obj max score))))
        (values wins max))))

(defun gt-tracker (num)
  (let ((gt nil))
    (lambda (x)
      (when (or (null gt) (> num x))
        (setf gt x))
      gt)))

(defun p-cat (file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil 'eof)
               (read-line str nil 'eof)))
        ((eql line 'eof))
      (format t "~A~%" line))))

(defun do-1 ()
  (do ((i 1 (1+ i))
       (j 2 (* j i)))
      ((> i 5) 'finished)
    (format t "i=~a, j=~a~%" i j)))

(do-1)

(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

(defun file->lines (fname)
  (with-open-file (stream fname :direction :input)
    (let (lines)
      (do ((line (read-line stream nil nil)
                 (read-line stream nil nil)))
          ((null line) (nreverse lines))
        (push line lines)))))

(defun file->exprs (fname)
  (with-open-file (stream fname :direction :input)
    (let (exprs)
      (do ((expr (read stream nil nil)
                 (read stream nil nil)))
          ((eq expr :eof) (nreverse exprs))
        (push expr exprs)))))
