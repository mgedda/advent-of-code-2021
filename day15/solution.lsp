(load "../utils/utils.lsp")

(debugging t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 1
;;;

(defstruct map buffer xs ys)

(defun parse-input (inputfile)
  "Read file into map"
  (let* ((rows (readfile inputfile))
         (xs (length (first rows)))
         (ys (length rows))
         (map (make-map :buffer (make-array (* xs ys) :initial-element 0) :xs xs :ys ys))
         (x 0)
         (y 0))
    (mapc #'(lambda (row)
              (mapc #'(lambda (col)
                        (setf (aref (map-buffer map) (+ x (* y xs)))
                              (al-parse-integer col))
                        (incf x))
                    (explode row))
              (incf y)
              (setf x 0))
          rows)
    map)
  )

(defun neighbors (pos map)
  (let ((nbrs '())
        (x (first pos))
        (y (second pos))
        (xs (map-xs map))
        (ys (map-ys map)))
    (when (> x 0) (setf nbrs (cons (list (1- x) y) nbrs)))
    (when (< x (1- xs)) (setf nbrs (cons (list (1+ x) y) nbrs)))
    (when (> y 0) (setf nbrs (cons (list x (1- y)) nbrs)))
    (when (< y (1- ys)) (setf nbrs (cons (list x (1+ y)) nbrs)))
    nbrs
    )
  )

(defun index (pos map)
  "i = x + y*xs"
  (let ((xs (map-xs map)))
    (+ (first pos) (* (second pos) xs))))

(defun get-pos (i map)
  "x = i mod xs, y = i / xs"
  (let ((xs (map-xs map)))
    (list (mod i xs) (floor (/ i xs)))))

(defun val (pos map)
  (let ((xs (map-xs map)))
    (aref (map-buffer map) (index pos map))))

(defun size (map)
  (* (map-xs map) (map-ys map)))


(defun update-map (pos new-val map)
  "Set POS in MAP to NEW-VAL."
  (setf (aref (map-buffer map) (index pos map)) new-val))

(defun new-map-same-size (init-val other-map)
  "Make a new map with same size as OTHER-MAP."
  (make-map :buffer (make-array (size other-map) :initial-element init-val)
                            :xs (map-xs other-map)
                            :ys (map-ys other-map)))


(defun get-all-positions (map)
  (let ((xs (map-xs map))
        (ys (map-ys map))
        (positions '()))
    (dotimes (i (* xs ys))
      (push (get-pos i map) positions))
    (reverse positions)
    )  
  )

(defun get-unvisited-positions (positions visited_map)
  (remove-if #'(lambda (pos)
                 (> (val pos visited_map) 0))
             positions))


(defun get-min-cost-nodes (positions cost_map)
  "Poor mans priority queue."
  (let ((min-cost-nodes '())
        (min_cost 999999999))
    (mapc #'(lambda (pos)
              (let ((pos_cost (val pos cost_map)))
                (cond ((< pos_cost min_cost)
                       (setf min_cost pos_cost)
                       (setf min-cost-nodes (list pos)))
                      ((= pos_cost min_cost)
                       (push pos min-cost-nodes))
                      )
                )
              )
          positions)
    min-cost-nodes)
  )


(defun find-min-path-cost (start goal map)
  (let ((cost_map (new-map-same-size 9999999999 map))
        (parent_map (new-map-same-size nil map))
        (visited_map (new-map-same-size 0 map))
        (unvisited_positions (get-all-positions map))
        (current start)
        (nbrs'()))
    (update-map start 0 cost_map)   ;; Set cost at start node to zero
    (while (not (= current goal))
      ;; (1) Update cost to neighbours
      ;; (2) Update parent map for neighbours
      ;; (3) Mark current as visited
      ;; (4) Choose new current pos with min cost
      (setf nbrs (neighbors current map))              ;; get neighbors
      (setf nbrs (remove-if #'(lambda (pos)
                                (> (val pos visited_map) 0)) ;; Remove visited
                            nbrs))
      (mapc #'(lambda (nbr)
                (let* ((curr_cost (val current cost_map))
                       (nbr_cost (val nbr cost_map))
                       (nbr_val (val nbr map))
                       (acc_cost (+ curr_cost nbr_val)))
                  (when (< acc_cost nbr_cost)
                    (update-map nbr acc_cost cost_map)  ;; update cost for neighbor
                    (update-map nbr current parent_map) ;; update parent map for neighbor
                    )
                  ))
            nbrs)

      (update-map current 1 visited_map)   ;; mark current as visited
      (setf unvisited_positions (get-unvisited-positions unvisited_positions visited_map))
      (setf current (first (get-min-cost-nodes unvisited_positions cost_map)))
      )
    
    (val current cost_map)
    )
  )


(defun solve-part-1 (inputfile)
  (let* ((map (parse-input inputfile))
         (xs (map-xs map))
         (ys (map-ys map))
         (min_cost (find-min-path-cost '(0 0) (list (1- xs) (1- ys)) map)))
    min_cost)
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
  (checkequal "Part 1 - test" ((part-1-test) 40))
  (checkequal "Part 1" ((part-1) 595))
  (checkequal "Part 2 - test" ((part-2-test) 315))
  (checkequal "Part 2" ((part-2) -1)))


