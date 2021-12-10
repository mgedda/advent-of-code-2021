(defun readfile (filename)
  "Read file rows to list."
  (with-input-file str filename
                   (let ((row)
                         (res (list)))
                     (while (not (eq (setq row (read-line str))
                                     '*eof*'))
                       (setf res (cons (string-trim " " row) res)))
                     (reverse res))))


(defun al-parse-integer (str)
  "Parse string containing an integer. (NOTE ONLY POSITIVE INTEGERS!)"
  (let ((base 48)
        (i 0)
        (mult 0)
        (s (reverse (explode str)))
        (acc 0)
        (res 0))
    (while (not (eq i (length str)))
      (setf mult (expt 10 i))
      (setf acc (- (char-int (car s)) base))
      (setf acc (* acc mult))
      (setf res (+ res acc))
      (setf i (1+ i))
      (setf s (cdr s)))
    (+ res 0)
    ))

(defun bitlist-to-int (b)
  "Convert list of bit values to integer."
  (let ((mult 0)
        (i 0)
        (rb (reverse b))
        (acc 0))
    (while (< i (length b))
      (setf mult (expt 2 i))
      (setf acc (+ acc (* (first rb) mult)))
      (setf rb (rest rb))
      (setf i (1+ i))
      )
    acc)
  )

(defun invert-bitlist (bitlist)
  "Invert list of bits."
  (mapcar #'(lambda (bit) (if (= 1 bit) 0 1)) bitlist)
  )

(defun parse-binary (s)
  "Convert binary string to integer."
  (bitlist-to-int (mapcar #'al-parse-integer (explode s))))
  

(defun string-of-ints-to-list (str delim)
  (mapcar #'al-parse-integer (string-trim-and-split str delim))
  )

(defun string-left-trim-s (str x)
  "Recursively remove string X from beginning of string STR"
  (if (equal (string-pos str x) 0)
      (let ((sub (substring (length x) (1- (length str)) str)))
        (string-left-trim-s sub x))
    (mkstring str)))
 
(defun string-trim-and-split (str delim)
  (string-split (string-trim " " str) delim))

(defun string-split (str delim)
  "Split string using delimiter string."
  (let ((delim_len (length delim))
        (res '())
        pos
        elem)
    (while (< 0 (length str))
      ;; Find delimeter position in string
      (setf pos (string-pos str delim))
      (if (= pos 0)
          ;; Remove delim at start of string
          (setf str (string-left-trim-s str delim))
        (progn
          (cond ((not pos)
                 ;; Only the last element left
                 (progn (setf elem str)
                        (setf str "")))
                (t 
                 ;; (pos > 0) Extract part of string before delimeter
                 (progn (setf elem (substring 0 (1- pos) str))
                        ;; Remove element from string
                        (setf str (substring pos (1- (length str)) str))
                        ;; Remove delimiter from string
                        (setf str (string-left-trim-s str delim)))
                 ))
          
          ;; Add element to result
          (setf res (cons elem res))
          )
        )
      )
    ;; Return result
    (reverse res)  
    )
  )

(defun make-list (len val)
  "Make a list with LEN number of VAL."
  (let ((i len)
        (res '()))
    (while (> i 0)
      (setf res (cons val res))
      (setf i (1- i)))
    res)
  )

(defun sign (x)
  (if (zerop x) 1 (/ x (abs x)))
  )

