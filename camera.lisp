(in-package :camera)

(defclass camera ()
  ((eye
    :accessor cam-eye
    :initarg :eye
    :initform #(0.0 0.0 0.0))
   (up-vec 
    :accessor cam-up
    :initarg :up
    :initform #(0.0 -1.0 0.0))
   (center
    :accessor cam-center
    :initarg :center
    :initform #(0.0 0.0 1.0))
   (field-of-view-y 
    :accessor cam-fovy
    :initarg :fov
    :initform 50.0)
   (aspect-ratio
    :accessor cam-aspect
    :initarg :aspect
    :initform 4/3)
   (vertical-angle
    :accessor cam-vertical-angle
    :initarg :vertical-angle
    :initform 0.0)
   (horizontal-angle
    :accessor cam-horizontal-angle
    :initarg :horizontal-angle
    :initform 0.0)
   (z-near-clip
    :accessor cam-znear
    :initarg :z-near
    :initform 0.1)
   (z-far-clip
    :accessor cam-zfar
    :initarg :z-far
    :initform 99.0)))

(defmethod print-camera-parameters ((cam camera))
  (let ((c (cam-center cam))
	(e (cam-eye cam))
	(u (cam-up cam))
	(va (cam-vertical-angle cam))
	(ha (cam-horizontal-angle cam)))
    (format t "Eye: ~A~%Center: ~A~%Up: ~A~%Vertical: ~A~%Horizontal: ~A~%"
	    e c u va ha)))

(defmethod update-vectors ((cam camera))
  (let* ((ha (helpers:radians (cam-horizontal-angle cam)))
	 (va (helpers:radians (cam-vertical-angle cam)))

	 ; rotation about X axis
	 (sx (sin va))
	 (cx (cos va))
	 ; rotation about Y axis
	 (sy (sin ha))
	 (cy (cos ha))
	 ; rotation about Z axis is always 90 degrees
	 ; so, cz = 0
	 ; and sz = 1

	 (view (vector (* cy sx)
		       sy
		       (* cy cx)))
	 (up (vector (- cx)
		     0.0
		     (* (sin va) (cos ha))))
	 (center (v:add view (cam-eye cam))))
    
    (setf (cam-up cam) up)
    (setf (cam-center cam) center)))
		     
(defmethod rotate-yaw ((cam camera) (f float))
  (let ((ha (cam-horizontal-angle cam)))
    (if (and (>= ha 0.0)
	     (<= ha 360.0))
	(setf (cam-horizontal-angle cam)
	      (+ ha f))))
  (let ((ha (cam-horizontal-angle cam)))
    (cond ((> ha 360.0) (setf (cam-horizontal-angle cam) (- ha 360.0)))
	  ((< ha 0.0) (setf (cam-horizontal-angle cam) (+ 360.0 ha)))
	  (t nil)))
  (update-vectors cam))

(defmethod rotate-pitch ((cam camera) (f float))
  (let ((va (cam-vertical-angle cam)))
    (if (and (<= va 180.0)
	     (>= va 0.0))
	(setf (cam-vertical-angle cam)
	      (+ va f))))
  (let ((va (cam-vertical-angle cam)))
    (cond ((> va 180.0) (setf (cam-vertical-angle cam) 180.0))
	  ((< va 0.0) (setf (cam-vertical-angle cam) 0.0))
	  (t nil)))
  (update-vectors cam))
      

(defun update-matrices (cam)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective (cam-fovy cam)
		   (cam-aspect cam)
		   (cam-znear cam)
		   (cam-zfar cam))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (let ((eye (cam-eye cam))
	(center (cam-center cam))
	(up (cam-up cam)))
    (glu:look-at (v:x eye) (v:y eye) (v:z eye)
		 (v:x center) (v:y center) (v:z center)
		 (v:x up) (v:y up) (v:z up))))
		 

(defmacro with-old-parameters ((cam &key eye center up view) &rest body)
  "Sets environment with defined old-eye, old-center and old-up variables. It also sets the old-view vector as normalized subtraction of old-center and old-eye vectors"
  (let ((binding-list (remove nil 
			      (list `(,eye (camera:cam-eye ,cam))
				    `(,center (camera:cam-center ,cam))
				    `(,up (camera:cam-up ,cam))
				    `(,view (v:normalize (v:sub ,center ,eye))))
			      :key #'car)))
    `(let* ,binding-list
       (progn ,@body))))
