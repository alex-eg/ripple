(in-package :camera)

(defclass camera ()
  ((eye
    :accessor cam-eye
    :initarg :eye
    :initform #(3 -3 3))
   (up-vec 
    :accessor cam-up
    :initarg :up
    :initform #(0 -1 0))
   (sight 
    :accessor cam-center
    :initarg :center
    :initform #(2 0 2))
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
		 

(defmacro with-old-parameters (cam &optional (old-eye old-center old-up old-view) &rest body)
  "Sets environment with defined old-eye, old-center and old-up variables. It also sets the old-view vector as normalized subtraction of old-center and old-eye vectors"
  `(let* ((,old-eye (camera:cam-eye ,cam))
	 (,old-center (camera:cam-center ,cam))
	 (,old-up (camera:cam-up ,cam))
	 (,old-view (v:normalize (v:sub ,old-center ,old-eye))))
     (progn ,@body)))
