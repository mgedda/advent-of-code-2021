(load "../utils/utils.lsp")

(debugging t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 1
;;;

(defun sum-bits (rows)
  (let ((acclst (make-list (length (first rows)) 0)))
    (mapc
     #'(lambda (row)
         (let* ((bits (explode row))
                (bitvals (mapcar #'(lambda (b) (al-parse-integer b)) bits)))
           (setf acclst (mapcar #'+ bitvals acclst))
           ))
     rows)
    acclst 
    )
  )

(defun gamma-bitlist (numrows bitsums)
  (let ((bitlist '())
        (thres (/ numrows 2)))
    (setf bitlist
          (mapcar #'(lambda (sum) (if (> sum thres) 1 0)) bitsums))
    bitlist
    )
  )

(defun solve-part-1 (inputfile)
  (let* ((input (readfile inputfile))
         (bitsums (sum-bits input))
         (g-bitlist (gamma-bitlist (length input) bitsums))
         (gamma-rate (bitlist-to-int g-bitlist))
         (epsilon-rate (bitlist-to-int (invert-bitlist g-bitlist))))
    (* gamma-rate epsilon-rate)
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


;;
;; bitlist (1 0 0 1 1 0)
;; rows    ("100101", "000111", ...) 
;;
;; (defstruct pair :row :lst)
;; pair row="100101" lst=(1 0 0 1 0 1)
;;

(defstruct pair row lst)


(defun generate-pairs (rows)
  (mapcar
   #'(lambda (row)
       (make-pair
        :row row
        :lst (mapcar #'al-parse-integer (explode row)) ))
   rows)
  )

  
(defun get-match (pairs bitlist)
  (if (= (length pairs) 1)
      (first pairs)
    (let* (
           (reduced-pairs (remove-if-not
                           #'(lambda (p)
                               (= (first (pair-lst p)) (first bitlist)))
                           pairs))
           (shaved-pairs (mapcar
                          #'(lambda (p)
                              (make-pair
                               :row (pair-row p)
                               :lst (rest (pair-lst p))))
                              reduced-pairs))
           )
      ;;(help)
      (get-match shaved-pairs (rest bitlist))
      )    
    )
  )


(defun solve-part-2 (inputfile)
  (let* ((input (readfile inputfile))
         (pairs (generate-pairs input))
         (bitsums (sum-bits input))
         (oxygen-bitlist (gamma-bitlist (length input) bitsums))
         (oxygen-pair (get-match pairs oxygen-bitlist))
         (oxygen-value (parse-binary (pair-row oxygen-pair)))
         (co2-bitlist (invert-bitlist oxygen-bitlist))
         (co2-pair (get-match pairs co2-bitlist))
         (co2-value (parse-binary (pair-row co2-pair)))
         )
    (* oxygen-value co2-value)
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
  (checkequal "Part 1 - test" ((part-1-test) 198))
  (checkequal "Part 1" ((part-1) 2035764))
  (checkequal "Part 2 - test" ((part-2-test) 230))
  (checkequal "Part 2" ((part-2) -1)))
