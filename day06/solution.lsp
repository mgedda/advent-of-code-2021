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


(defun solve (inputfile days)
  (let* ((fish (parse-input inputfile))) 
    (length (cycle fish days))
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
  (checkequal "Part 2" ((part-2) -1)))
