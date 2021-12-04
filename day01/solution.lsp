(load "../utils/utils.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 1
;;;

(defun solve-part-1 (inputfile)
  (let ((input (mapcar 'parse-to-num (readfile inputfile)))
        (counter 0))
    (while (> (length input) 1)
      (if (< (first input) (first (rest input)))
          (setf counter (1+ counter)))
      (setf input (rest input)))
    (+ counter 0)))


;; Test
;;
(defun part-1-test ()
  (solve-part-1 "input-test.txt"))

;; Entry point
;;
(defun part-1 ()
  (solve-part-1 "input.txt"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 2
;;;

(defun solve-part-2 (inputfile)
  (let ((input (listtoarray (mapcar 'parse-to-num (readfile inputfile))))
        (i 0)
        (res 0))  ; result
    (while (< i (- (length input) 3))
      (let ((v1 (+ (aref input i) (aref input (1+ i)) (aref input (+ i 2))))
            (v2 (+ (aref input (1+ i)) (aref input (+ i 2)) (aref input (+ i 3)))))
        (if (< v1 v2)
            (setf res (1+ res))))
      (setf i (1+ i)))
    (+ res 0)))


;; Test
;;
(defun part-2-test ()
  (solve-part-2 "input-test.txt"))

;; Entry point
;;
(defun part-2 ()
  (solve-part-2 "input.txt"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests
;;;

(defun test ()
  (checkequal "Part 1 - test" ((part-1-test) 7))
  (checkequal "Part 1" ((part-1) 1448))
  (checkequal "Part 2 - test" ((part-2-test) 5))
  (checkequal "Part 2" ((part-2) 1471)))
