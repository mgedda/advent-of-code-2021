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

(defun reduce-days (fish)
  (mapcar #'(lambda (f) (if (= f 0) 6 (1- f) )) fish)
  )

(defun spawn-fish (fish)
  (make-list (length (remove-if-not #'(lambda (x) (= x 0)) fish)) 8)
  )

(defun cycle (fish days)
  (if (= days 0)
      fish
    (progn
      (let* ((fish_prim (reduce-days fish))
            (new_fish (spawn-fish fish))
            (fish_updated (append fish_prim new_fish)))
        (cycle fish_updated (1- days))
        )
      )
    )
  )


(defun solve-part-1 (inputfile days)
  (let* ((fish (parse-input inputfile))) 
    (length (cycle fish days))
    )
  )

;; Test
;;
(defun part-1-test ()
  (solve-part-1 "input-test.txt" 80))

;; Entry point
;;
(defun part-1 ()
  (solve-part-1 "input.txt" 80))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 2
;;;


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

(defun solve-part-2 (inputfile days)
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
(defun part-2-test ()
  (solve-part-2 "input-test.txt" 256))

;; Entry point
;;
(defun part-2 ()
  (solve-part-2 "input.txt" 256))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests
;;;

(defun test ()
  (checkequal "Part 1 - test" ((part-1-test) 5934))
  (checkequal "Part 1" ((part-1) 373378))
  (checkequal "Part 2 - test" ((part-2-test) 26984457539))
  (checkequal "Part 2" ((part-2) 1682576647495)))
