(defun parse-intstr-to-real (str)
  "Parse string containing an integer to real."
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


(defun readfile (filename)
  "Read file rows to list."
  (with-input-file str filename
                   (let ((row)
                         (res (list)))
                     (while (not (eq (setq row (read-line str))
                                     '*eof*'))
                       (setf res (cons (string-trim " " row) res)))
                     (reverse res))))

