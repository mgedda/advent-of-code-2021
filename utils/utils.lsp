(defun readfile (filename)
  "Read file rows to list."
  (with-input-file str filename
                   (let ((row)
                         (res (list)))
                     (while (not (eq (setq row (read-line str))
                                     '*eof*'))
                       (setf res (cons (string-trim " " row) res)))
                     (reverse res))))


(defun parse-to-num (str)
  "Parse string containing an integer to number."
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

