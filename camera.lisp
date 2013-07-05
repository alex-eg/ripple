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
   (z-near-clip
    :accessor cam-znear
    :initarg :z-near
    :initform 0.1)
   (z-far-clip
    :accessor cam-zfar
    :initarg :z-far
    :initform 99.0)))

(defmethod cam-view ((cam camera))
  (let ((eye (cam-eye cam))
	(center (cam-center cam)))
    (v:sub center eye)))

(defmethod print-camera-parameters ((cam camera))
  (let ((c (cam-center cam))
	(e (cam-eye cam))
	(u (cam-up cam))
	(view (cam-view cam)))
    (format t "Eye: ~A~%Center: ~A~%Up: ~A~%View: ~A~%"
	    e c u view)))

(defmethod rotate-yaw ((cam camera) (f float))
  (let* ((eye (cam-eye cam))
	 (view (v:normalize (cam-view cam)))
	 (up (cam-up cam))

	 (rot-matrix (m:rotate up (helpers:radians f)))

	 (new-eye (v:add eye
			 (m:coerce-vector 
			  (m:*-mat-mat	
			   view
			   rot-matrix)))))
      (setf (cam-center cam) new-eye)))

(defmethod rotate-pitch ((cam camera) (f float))
  (let* ((view (cam-view cam))
	 (eye (cam-eye cam))
	 (up (cam-up cam))
	 (side (v:cross up view))
	 
	 (rot-matrix (m:rotate side (helpers:radians f)))

	 (new-view (m:coerce-vector
		    (m:*-mat-mat view rot-matrix)))
	 (new-center (v:add eye new-view))
	 ;; What's algorithmically better? Matrix multiplication, or cross product?
	 ;; Suppose it's cross production
	 (new-up (v:normalize (v:cross new-view side))))
    (setf (cam-center cam) new-center)
    (setf (cam-up cam) new-up)))

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
				    `(,view (camera:cam-view ,cam)))
			      :key #'car)))
    `(let* ,binding-list
       (progn ,@body))))
