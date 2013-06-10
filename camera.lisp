(in-package :camera)

(defclass camera ()
  ((position 
    :accessor cam-position
    :initarg :position
    :initform #(0 -2 2))
   (upvec 
    :accessor cam-upvec
    :initarg :upvec
    :initform #(0 -1 0))
   (sight 
    :accessor cam-sight 
    :initarg :sight
    :initform #(0 0 0))
   (field-of-view-y 
    :accessor cam-fovy
    :initarg :fov
    :initform 50.0)
   (aspect-ratio
    :accessor cam-aspect
    :initarg :aspect
    :initform 1.0)
   (z-near-clip
    :accessor cam-z-near
    :initarg :z-near
    :initform 0.1)
   (z-far-clip
    :accessor cam-z-far
    :initarg :z-far
    :initform 99.0)))

(defun get-projection-matrix (cam)
  (let* ((fovy (cam-fovy cam))
	 (aspect (cam-aspect cam))
	 (z-near (cam-z-near cam))
	 (z-far (cam-z-far cam))

	 (range (* (tan (radians (/ fovy 2)))
		   z-near))

	 (left (- (* range aspect)))
	 (right (* range aspect))
	 (top range)
	 (bottom (- range)))
    (make-array '(4 4) :initial-contents
		(list (list (/ (* 2 z-near) (- right left)) 0.0 0.0 0.0)
		      (list 0.0 (/ (* 2 z-near) (- top bottom)) 0.0 0.0)
		      (list 0.0 0.0 (- (/ (+ z-far z-near) 
					  (- z-far z-near))) -1.0)
		      (list 0.0 0.0 (- (/ (* 2 z-near z-far) 
					  (- z-far z-near))) 0.0)))))
	
(defun get-look-at-matrix (cam)
  (let* ((eye (cam-position cam))
	 (center (cam-sight cam))
	 (up (cam-upvec cam))

	 (f (v:normalize (map 'vector 
			    #'-
			    center
			    eye)))
	 (u (v:normalize up))
	 (s (v:normalize (cross f u)))
	 (real-u (v:cross s f))

	 (eye-x (v:x eye))
	 (eye-y (v:y eye))
	 (eye-z (v:z eye))

	 (s-x (v:x s))
	 (s-y (v:y s))
	 (s-z (v:z s))
	 
	 (u-x (v:x real-u))
	 (u-y (v:y real-u))
	 (u-z (v:y real-u))
	 
	 (f-x (v:x f))
	 (f-y (v:y f))
	 (f-z (v:z f))

	 (trans-x (- (+ (* s-x eye-x) (* s-y eye-y) (* s-z eye-z))))
	 (trans-y (- (+ (* u-y eye-y) (* u-y eye-y) (* u-z eye-z))))
	 (trans-z (+ (* f-x eye-x) (* f-y eye-y) (* f-z eye-z))))
    (make-array '(4 4) 
		:initial-contents
		(list
		 (list s-x s-y s-z trans-x)
		 (list u-x u-y u-z trans-y)
		 (list (- f-x) (- f-y) (- f-z) trans-z)
		 (list 0.0 0.0 0.0 1.0)))))
	 
  
(defun update-matrices (cam)
  (gl:matrix-mode :projection)
  (gl:load-matrix (get-projection-matrix cam))
  
  (gl:matrix-mode :modelview)
  (gl:load-matrix (get-look-at-matrix cam)))
