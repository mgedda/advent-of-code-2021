(load "../utils/utils.lsp")

(debugging t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 1
;;;

(defstruct area buffer xs ys)

(defun parse-input (inputfile)
  (let* ((rows (readfile inputfile))
         (xs (length (first rows)))
         (ys (length rows))
         (area (make-area :buffer (make-array (* xs ys) :initial-element 0) :xs xs :ys ys))
         (x 0)
         (y 0))
    (mapc #'(lambda (row)
              (mapc #'(lambda (col)
                        (setf (aref (area-buffer area) (+ x (* y xs)))
                              (al-parse-integer col))
                        (incf x))
                    (explode row))
              (incf y)
              (setf x 0))
          rows)
    area)
  )

(defun neighbors (x y xs ys)
  (let ((nbrs '()))
    (when (> x 0) (setf nbrs (cons (list (1- x) y) nbrs)))
    (when (< x (1- xs)) (setf nbrs (cons (list (1+ x) y) nbrs)))
    (when (> y 0) (setf nbrs (cons (list x (1- y)) nbrs)))
    (when (< y (1- ys)) (setf nbrs (cons (list x (1+ y)) nbrs)))
    nbrs
    )
  )

(defun find-local-min (area)
  (let ((buf (area-buffer area))
        (xs (area-xs area))
        (ys (area-ys area))
        (x 0)
        (y 0)
        (min_coords '()))
    (while (< y ys)
      (setf x 0)
      (while (< x xs)
        (let ((val (aref buf (+ x (* y xs))))
              (nbrs (neighbors x y xs ys)))
          (when (every #'(lambda (nbr)
                           (let* ((nx (first nbr))
                                  (ny (second nbr))
                                  (nval (aref buf (+ nx (* ny xs)))))
                             (< val nval)))
                       nbrs)
            (setf min_coords (cons (list x y) min_coords)))
          )
        (incf x))
      (incf y))
    min_coords)
  )


(defun get-heights (area points)
  (let ((buf (area-buffer area))
        (xs (area-xs area)))
    (mapcar #'(lambda (p)
                (let ((x (first p))
                      (y (second p)))
                  (1+ (aref buf (+ x (* y xs))))))
            points)
    )
  )

(defun solve-part-1 (inputfile)
  (let* ((area (parse-input inputfile))
         (low_points (find-local-min area))
         (heights (get-heights area low_points)))
    (reduce #'+ heights)
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

(defun index (x y xs)
  (+ x (* y xs)))

(defun val (buf x y xs)
  (aref buf (index x y xs)))

(defun get-basin (area point)
  (let ((buf (area-buffer area))
        (xs (area-xs area))
        (ys (area-ys area))
        (basin '())
        (visited '())
        (queue (list point))
        (current nil)
        (x 0)
        (y 0)
        (h 0)
        (nbrs'()))
    (while queue
      (setf current (first queue))   ;; pop next node
      (pop queue)
      (setf x (first current))
      (setf y (second current))
      (setf h (val buf x y xs))
      (when (< h 9)
          (progn
            (push current basin)
            (setf nbrs (neighbors x y xs ys))   ;; get neighbors
            (setf nbrs (remove-if
                        #'(lambda (nbr) (or (member nbr visited) (member nbr queue)))
                        nbrs))
            (mapc #'(lambda (nbr) (push nbr queue)) nbrs)
            ))
      (push current visited)       ;; move to visited set
      )
    basin)
  )


(defun get-basins (area low_points)
  (mapcar #'(lambda (point) (get-basin area point)) low_points)
  )
  
(defun solve-part-2 (inputfile)
  (let* ((area (parse-input inputfile))
         (low_points (find-local-min area))
         (basins (get-basins area low_points))
         (sizes (mapcar #'(lambda (basin) (length basin)) basins)))
    (setf sizes (sort sizes #'>))
    (* (first sizes) (second sizes) (third sizes))
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
  (checkequal "Part 1 - test" ((part-1-test) 15))
  (checkequal "Part 1" ((part-1) 554))
  (checkequal "Part 2 - test" ((part-2-test) 1134))
  (checkequal "Part 2" ((part-2) 1017792)))


