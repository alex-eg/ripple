(in-package :light-source)

(defclass omni (resource)
  ((position
    :accessor position
    :initarg :position
    :initform #(0.0 0.0 0.0 1.0))
   (color
    :accessor color
    :initarg :color
    :initform #(1.0 1.0 1.0 1.0))))
  
