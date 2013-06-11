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
  (let ((eye (cam-position cam))
	(center (cam-sight cam))
	(up (cam-upvec cam)))
    (glu:look-at (v:x eye) (v:y eye) (v:z eye)
		 (v:x center) (v:y center) (v:z center)
		 (v:x up) (v:y up) (v:z up))))
		 
