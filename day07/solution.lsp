(load "../utils/utils.lsp")

(debugging t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 1
;;;


(defun fuel-cost-1 (n)
  n)

(defun sum-of-diffs (crabs x fn)
  (reduce #'+ (mapcar #'(lambda (a) (funcall fn (abs (- a x)))) crabs)))

(defun pos-dist-pairs (crabs fn)
  (mapcar #'(lambda (x)
              (let ((sum (sum-of-diffs crabs x fn))) (list x sum)))
          crabs))

(defun min-fuel-usage (crabs fn)
  (let ((pairs (pos-dist-pairs crabs fn))
        (m '(0 999999999)))
    (mapc #'(lambda (p) (when (> (second m) (second p)) (setf m p))) pairs)
    (second m))
  )

(defun parse-input (inputfile)
  (let ((input (readfile inputfile)))
    (mapcar #'(lambda (str) (al-parse-integer str))
            (string-split (first input) ","))
    )
  )

(defun solve (inputfile fn)
  (let ((crabs (parse-input inputfile)))
    (min-fuel-usage crabs fn)
    )
  )


;; Test
;;
(defun part-1-test ()
  (solve "input-test.txt" 'fuel-cost-1))

;; Entry point
;;
(defun part-1 ()
  (solve "input.txt" 'fuel-cost-1))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 2
;;;

(defun fuel-cost-2 (n)
  (/ (* n (1+ n)) 2))

;; Test
;;
(defun part-2-test ()
  (solve "input-test.txt" 'fuel-cost-2))

;; Entry point
;;
(defun part-2 ()
  (solve "input.txt" 'fuel-cost-2))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests
;;;

(defun test ()
  (checkequal "Part 1 - test" ((part-1-test) 37))
  (checkequal "Part 1" ((part-1) 339321))
  (checkequal "Part 2 - test" ((part-2-test) 168))
  (checkequal "Part 2" ((part-2) 95476244)))


