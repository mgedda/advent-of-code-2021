(load "../utils/utils.lsp")

(debugging t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 1
;;;

(defun parse-input (inputfile)
  (let ((rows (readfile inputfile)))
    (mapcar #'(lambda (str) (al-parse-integer str))
            (string-split (first rows) ","))
    )
  )

(defun update-hist (hist)
  (let ((z (aref hist 0)))  ; save hist[0]
    (setf (aref hist 0) (aref hist 1))
    (setf (aref hist 1) (aref hist 2))
    (setf (aref hist 2) (aref hist 3))
    (setf (aref hist 3) (aref hist 4))
    (setf (aref hist 4) (aref hist 5))
    (setf (aref hist 5) (aref hist 6))
    (setf (aref hist 6) (+ (aref hist 7) z))
    (setf (aref hist 7) (aref hist 8))
    (setf (aref hist 8) z)
    hist)
  )

(defun solve (inputfile days)
  (let* ((fish (parse-input inputfile))
         (hist (make-array 9 :initial-element 0))) 
    ;; Store number of fishes in a histogram
    (mapc #'(lambda (v) (incf (aref hist v))) fish)
    (while (> days 0)
      (setf hist (update-hist hist))
      (setf days (1- days))
      )
    (reduce #'+ (arraytolist hist))
    )
  )

;; Test
;;
(defun part-1-test ()
  (solve "input-test.txt" 80))

;; Entry point
;;
(defun part-1 ()
  (solve "input.txt" 80))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 2
;;;



;; Test
;;
(defun part-2-test ()
  (solve "input-test.txt" 256))

;; Entry point
;;
(defun part-2 ()
  (solve "input.txt" 256))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests
;;;

(defun test ()
  (checkequal "Part 1 - test" ((part-1-test) 5934))
  (checkequal "Part 1" ((part-1) 373378))
  (checkequal "Part 2 - test" ((part-2-test) 26984457539))
  (checkequal "Part 2" ((part-2) 1682576647495)))
