(in-package :v)

(defun x (vec)
  (aref v 0))

(defun y (vec)
  (aref v 1))

(defun z (vec)
  (aref v 2))

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
    (if (= len 0.0)
	(make-array len :initial-element 0.0)
	(map 'vector 
	     (lambda (x)
	       (/ x len))
	     vec))))
		   	
(defun cross (a b)
  "Returns cross product of two vectors"
  (assert (= (length a) (length b) 3))
  (let ((a-x (v:x a))
	(a-y (v:y a))
	(a-z (v:z a))
	
	(b-x (v:x b))
	(b-y (v:y b))
	(b-z (v:z b)))
    #2a((- (* a-y b-z) (* b-y a-z))
	(- (* a-z b-z) (* b-z a-x))
	(- (* a-x b-y) (* b-x a-y)))))
