(in-package :m)

(defun mult-matrix (left right)
  (let* ((l-rows (array-dimension left 0))
	 (l-cols (array-dimension left 1))
	 (r-rows (array-dimension right 0))
	 (r-cols (array-dimension right 1))
	 (new (make-array `(,l-rows ,r-cols))))
    (assert (= l-cols r-rows)
	    (l-cols l-rows)
	    "Matrix dimesions don't match: left has ~A cols and right has ~A rows" l-cols r-rows)
    (dotimes (i l-rows)
      (dotimes (j r-cols)
	(setf (aref new i j)
	      (reduce (lambda (c elem)
			(+ c (* (car elem)
				(cadr elem))))
		      (map 'vector #'list
			   (matrix-row left i)
			   (matrix-col right j))
		      :initial-value 0))))
    new))
  
	       
(defun matrix-row (matrix i)
  (let ((cols (- (array-dimension matrix 1) 1))
	(new (make-array (array-dimension matrix 1))))
    (loop for x from 0 to cols
       do (setf (aref new x) (aref matrix i x))
       finally (return new))))

(defun matrix-col (matrix i)
  (let ((rows (- (array-dimension matrix 0) 1))
	(new (make-array (array-dimension matrix 0))))
    (loop for x from 0 to rows
       do (setf (aref new x) (aref matrix x i))
       finally (return new))))

(defun translate (matrix vector)
  )

(defun rotate (matrix vector)
  )

(defun scale (matrix size)
  )
