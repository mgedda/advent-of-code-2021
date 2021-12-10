(load "../utils/utils.lsp")

(debugging t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 1
;;;


(defstruct line p1 p2)

(defstruct point x y)

(defun parse-point (coord-str)
  (make-point :x (al-parse-integer (first (string-split coord-str ",")))
              :y (al-parse-integer (second (string-split coord-str ","))))
  )

(defun parse-lines (inputfile)
  (let* ((rows (readfile inputfile))
         (coord-pairs (mapcar #'(lambda (row) (string-split row " -> ")) rows)))
    (mapcar
     #'(lambda (pair)
         (make-line :p1 (parse-point (first pair))
                    :p2 (parse-point (second pair)))
         )
     coord-pairs)
    )
  )

(defun remove-diagonal-lines (lines)
  (remove-if-not
   #'(lambda (line) (let ((p1 (line-p1 line))
                          (p2 (line-p2 line)))
                      (or (equal (point-x p1) (point-x p2))
                          (equal (point-y p1) (point-y p2)))))
   lines)
  )


(defun make-points-from-lines (lines)
  (let ((points '()))
    (mapc
     #'(lambda (line)
         (let* ((p1 (line-p1 line))
                (p2 (line-p2 line))
                (x1 (point-x p1))
                (x2 (point-x p2))
                (y1 (point-y p1))
                (y2 (point-y p2))
                (dx (- x2 x1))
                (dy (- y2 y1))
                (x 0)
                (y 0)
                (len (1+ (max (abs dx) (abs dy)))))
           (dotimes (i len)
             (if (= dx 0) (setf x x1) (setf x (+ x1 (* i (sign dx)))))
             (if (= dy 0) (setf y y1) (setf y (+ y1 (* i (sign dy)))))
             (setf points (cons (make-point :x x :y y) points))
            ))
         )
     lines)
    points)
  )

(defun make-point-hist (points)
  (let ((histogram (make-hash-table :test (function equal))))
    (mapc #'(lambda (point)
              ;; Add point to histogram
              (if (gethash point histogram)
                  (setf (gethash point histogram)
                        (1+ (gethash point histogram))) 
                (setf (gethash point histogram) 1))
              )
          points)
    histogram)
  )

(defun count-overlaps (point-hist)
  (let ((counter 0))
    (maphash #'(lambda (key val)
                 (when (> val 1)
                   (setf counter (1+ counter))))
             point-hist)
    counter)
  )

(defun solve-part-1 (inputfile)
  (let* ((lines (parse-lines inputfile))
         (non-diag-lines (remove-diagonal-lines lines))
         (points (make-points-from-lines non-diag-lines))
         (point-hist (make-point-hist points)))
    (count-overlaps point-hist)
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

(defun solve-part-2 (inputfile)
  (let* ((lines (parse-lines inputfile))
         (points (make-points-from-lines lines))
         (point-hist (make-point-hist points)))
    (count-overlaps point-hist)
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
  (checkequal "Part 1 - test" ((part-1-test) 5))
  (checkequal "Part 1" ((part-1) 5145))
  (checkequal "Part 2 - test" ((part-2-test) 12))
  (checkequal "Part 2" ((part-2) 16518)))
