(load "../utils/utils.lsp")

(debugging t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 1
;;;

(defstruct board rows cols)

(defun parse-numbers (inputfile)
  (let ((input (readfile inputfile)))
    (string-of-ints-to-list (first input) ","))
  )

(defun parse-board (input)
  (let ((rows nil)
        (cols nil))    
    (setf rows (list
                (string-of-ints-to-list (first input) " ")
                (string-of-ints-to-list (second input) " ")
                (string-of-ints-to-list (third input) " ")
                (string-of-ints-to-list (fourth input) " ")
                (string-of-ints-to-list (fifth input) " ")))
    (setf cols (mapcar #'(lambda (n1 n2 n3 n4 n5) (list n1 n2 n3 n4 n5))
                       (first rows)
                       (second rows)
                       (third rows)
                       (fourth rows)
                       (fifth rows)))
    (make-board :rows rows :cols cols)
    )
  )
    
(defun parse-boards (inputfile)
  (let ((input (readfile inputfile))
        (boards '()))
    (setf input (cddr input))   ; Remove first two rows

    (while (not (null input))
      (setf boards (cons (parse-board input) boards))
      (setf input (cdr (cdr (cdr (cdr (cdr (cdr input)))))))
      )
    (reverse boards)
    )
  )

(defun update-board (number board)
  (let ((rows (board-rows board))
        (cols (board-cols board)))
     (setf (first rows) (remove-if #'(lambda (x) (= x number)) (first rows)))
     (setf (second rows) (remove-if #'(lambda (x) (= x number)) (second rows)))
     (setf (third rows) (remove-if #'(lambda (x) (= x number)) (third rows)))
     (setf (fourth rows) (remove-if #'(lambda (x) (= x number)) (fourth rows)))
     (setf (fifth rows) (remove-if #'(lambda (x) (= x number)) (fifth rows)))

     (setf (first cols) (remove-if #'(lambda (x) (= x number)) (first cols)))
     (setf (second cols) (remove-if #'(lambda (x) (= x number)) (second cols)))
     (setf (third cols) (remove-if #'(lambda (x) (= x number)) (third cols)))
     (setf (fourth cols) (remove-if #'(lambda (x) (= x number)) (fourth cols)))
     (setf (fifth cols) (remove-if #'(lambda (x) (= x number)) (fifth cols)))
    )
  )

(defun update-boards (number boards)
  (mapc #'(lambda (board) (update-board number board)) boards)
  )

(defun find-winning-board (boards)
  (let ((winning-board nil))
    (mapc
     #'(lambda (board)
         (mapc
          #'(lambda (row)
              (when (null row) (setf winning-board board)))
          (board-rows board))
         (mapc
          #'(lambda (col)
              (when (null col) (setf winning-board board)))
          (board-cols board))
         )
     boards)
    winning-board)
  )

(defun calc-score (board number)
  (let ((rows (board-rows board)))
    (* number (reduce #'+ (append
                           (first rows)
                           (second rows)
                           (third rows)
                           (fourth rows)
                           (fifth rows))))
    )
  )

(defun solve-part-1 (inputfile)
  (let* ((numbers (parse-numbers inputfile))
         (boards (parse-boards inputfile))
         (winning-board nil)
         (number 0))
    (while (null winning-board)
      (setf number (first numbers))
      (update-boards number boards)
      (setf winning-board (find-winning-board boards))
      (setf numbers (rest numbers))
      )
    (calc-score winning-board number)
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

(defun is-winning-board (board)
  (let ((res nil))
    (mapc
     #'(lambda (row)
         (when (null row) (setf res t)))
     (board-rows board))
    (mapc
     #'(lambda (col)
         (when (null col) (setf res t)))
     (board-cols board))
    res)
  )

(defun solve-part-2 (inputfile)
  (let* ((numbers (parse-numbers inputfile))
         (boards (parse-boards inputfile))
         (number 0)
         (winning-boards nil))
    (while (> (length boards) 0)
      (setf number (first numbers))
      (update-boards number boards)
      (setf winning-boards (remove-if-not #'is-winning-board boards))
      (setf boards (set-difference boards winning-boards))
      (setf numbers (rest numbers))
      )
    (calc-score (first winning-boards) number)
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
  (checkequal "Part 1 - test" ((part-1-test) 4512))
  (checkequal "Part 1" ((part-1) 10374))
  (checkequal "Part 2 - test" ((part-2-test) 1924))
  (checkequal "Part 2" ((part-2) 24742)))
