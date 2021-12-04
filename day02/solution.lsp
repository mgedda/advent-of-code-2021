(load "../utils/utils.lsp")

(debugging t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 1
;;;

;; x - horizontal position
;; d - depth
(defstruct point x d)

(defun sum-movements (rows p)
  (mapc
   #'(lambda (row)
       (let* ((line (string-split row " "))
              (instr (first line))
              (value (al-parse-integer (second line))))
         (case instr
          ("forward" (setf (point-x p) (+ (point-x p) value)))
          ("down" (setf (point-d p) (+ (point-d p) value)))
          ("up" (setf (point-d p) (- (point-d p) value))))))
   rows)
  p)

(defun solve-part-1 (inputfile)
  (let* ((input (readfile inputfile))
         (dest (sum-movements input (make-point :x 0 :d 0))))
    (* (point-x dest) (point-d dest))
    )
  )

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

;; x - horizontal position
;; d - depth
;; a - aim
(defstruct subm x d a)

(defun sum-movements-2 (rows s)
  (mapc
   #'(lambda (row)
       (let* ((line (string-split row " "))
              (instr (first line))
              (value (al-parse-integer (second line))))
         (case instr
          ("forward"
           (setf (subm-x s) (+ (subm-x s) value))
           (setf (subm-d s) (+ (subm-d s) (* (subm-a s) value))))
          ("down" (setf (subm-a s) (+ (subm-a s) value)))
          ("up" (setf (subm-a s) (- (subm-a s) value))))
         ))
   rows)
  s)

(defun solve-part-2 (inputfile)
  (let* ((input (readfile inputfile))
         (s (sum-movements-2 input (make-subm :x 0 :d 0 :a 0))))
    (* (subm-x s) (subm-d s))
    )
  )

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
  (checkequal "Part 1 - test" ((part-1-test) 150))
  (checkequal "Part 1" ((part-1) 2215080))
  (checkequal "Part 2 - test" ((part-2-test) 900))
  (checkequal "Part 2" ((part-2) 1864715580)))
