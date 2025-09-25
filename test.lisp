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
