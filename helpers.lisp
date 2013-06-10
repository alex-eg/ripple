(in-package :helpers)

(defun radians (a)
  "Converts degrees to radians"
  (* pi (/ a 180.0)))

(defun mag (vec)
  "Returns vector magnitude"
  (sqrt 
   (reduce (lambda (c x)
	     (+ c (* x x)))
	   vec
	   :initial-value 0.0)))

(defun normalize (vec)
  "Returns normalized vector"
  (let ((len (mag vec)))
    (map 'vector (lambda (x)
		   (/ x len))
	 vec)))
		   	

(defun cross (a b)
  "Returns cross product of two vectors"
  )
 
