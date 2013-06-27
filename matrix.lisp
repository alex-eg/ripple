(in-package :m)

(defun identity-matrix ()
  (make-array '(3 3)
	      :element-type 'single-float
	      :initial-contents
	      '((1.0 0.0 0.0)
		(0.0 1.0 0.0)
		(0.0 0.0 1.0))))

(defmacro do-matrix (((i n) (j m)) &rest body)
  `(dotimes (,i ,n)
     (dotimes (,j ,m)
       (progn ,@body))))

(defun transpose (matrix)
  (let* ((n (array-dimension matrix 0))
	 (m (array-dimension matrix 1))
	 (transposed-matrix (make-array `(,m ,n)
					:element-type 'single-float)))
    (do-matrix ((i n) (j m))
      (setf (aref transposed-matrix j i)
	    (aref matrix i j)))
    transposed-matrix))

(defun *-mat-mat (left right)
  (let* ((l-rows (array-dimension left 0))
	 (l-cols (array-dimension left 1))
	 (r-rows (array-dimension right 0))
	 (r-cols (array-dimension right 1))
	 (new (make-array `(,l-rows ,r-cols))))
    (assert (= l-cols r-rows)
	    (l-cols l-rows)
	    "Matrix dimesions don't match: left has ~A cols and right has ~A rows" l-cols r-rows)
    (do-matrix ((i l-rows) (j r-cols))
      (setf (aref new i j)
	    (reduce (lambda (c elem)
		      (+ c (* (car elem)
			      (cadr elem))))
		    (map 'vector #'list
			 (matrix-row left i)
			 (matrix-col right j))
		    :initial-value 0)))
    new))

(defun *-mat-num (matrix number)
  (let ((new-matrix (identity-matrix))
	(n (array-dimension matrix 0))
	(m (array-dimension matrix 1)))
    (do-matrix ((i n) (j m))
      (setf (aref new-matrix i j)
	    (* number (aref matrix i j))))
  new-matrix))

(defun +-mat (matrix &rest other-matrices)
  (labels ((add-matrix (left right)
	     (let* ((n (array-dimension left 0))
		    (m (array-dimension left 1))
		    (new-matrix (make-array `(,n ,m)
					    :element-type 'single-float
					    :initial-element 0.0)))
	       (do-matrix ((i n) (j m))
		 (setf (aref new-matrix i j)
		       (+ (aref left i j)
			  (aref right i j))))
	     new-matrix)))
  (reduce #'add-matrix other-matrices 
	  :initial-value matrix)))

(defun mat-4 (mat-3)
  (let ((new (make-array '(4 4)
			 :element-type 'single-float
			 :initial-element 0.0)))
    (do-matrix ((i 3) (j 3))
      (setf (aref new i j)
	    (aref mat-3 i j)))
  (setf (aref new 3 3) 1.0)
  new))

(defun matrix-row (matrix i)
  (let ((cols (- (array-dimension matrix 1) 1))
	(row (make-array (array-dimension matrix 1))))
    (dotimes (j cols)
      (setf (aref row j) (aref matrix i j)))
    row))

(defun matrix-col (matrix j)
  (let ((rows (- (array-dimension matrix 0) 1))
	(col (make-array (array-dimension matrix 0))))
    (dotimes (i rows)
      (setf (aref col i) (aref matrix i j)))
    col))

(defun translate (vector)
  "Creates translation matrix in homogenuos coordinates."
  (let ((translate-matrix (mat-4 *I*)))
    (dotimes (i 3)
       (setf (aref translate-matrix i 3)
	     (aref vector i)))
    translate-matrix))

(defun rotate (vector angle)
  "Creates rotation matrix."
  (let* ((x (v:x vector))
	 (y (v:y vector))
	 (z (v:z vector))
	 
	 (first-component (*-mat-num (identity-matrix) (cos angle)))
	 (second-component-matrix (make-array '(3 3)
					      :element-type 'single-float
					      :initial-contents 
					      `((,(* x x) ,(* x y) ,(* x z))
						(,(* x y) ,(* y y) ,(* y z))
						(,(* x z) ,(* z y) ,(* z z)))))
	 (second-component (*-mat-num second-component-matrix (- 1 (cos angle))))
	 (third-component-matrix (make-array '(3 3)
					     :element-type 'single-float
					     :initial-contents
					     `((0.0 ,(- z) ,y)
					       (,z 0.0 ,(- x))
					       (,(- y) ,x 0.0))))
	 (third-component (*-mat-num third-component-matrix (sin angle)))
	 (rotate-matrix (+-mat first-component
			       second-component
			       third-component)))
    rotate-matrix))

(defun scale (vector)
  (let ((scale-matrix (identity-matrix)))
    (dotimes (i 3)
      (setf (aref scale-matrix i i)
	    (aref vector i)))
    scale-matrix))
