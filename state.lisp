(in-package :state)

(defclass state ()
  ((vao
    :accessor state-vao
    :initform nil)
   (shader-pool
    :accessor shader-pool
    :initform (make-hash-table))
   (camera-pool
    :accessor camera-pool
    :initform (make-hash-table))
   (texture-pool
    :accessor texture-pool
    :initforn (make-hash-table))))

(defun use-shader-program (state program-name)
  (let ((shader (gethash program-name (shader-pool state))))
    (gl:use-program (shader:program-id shader))))

(defun use-default-program ()
  (gl:use-program 0))
