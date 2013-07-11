(in-package :shader)

(defclass shader (resource)
  ((filename 
    :accessor shader-filename
    :initarg :path)
   (type
    :accessor shader-type
    :initarg :shader-type)
   (compiled?
    :accessor shader-compiled?
    :initform nil)
   (source
    :accessor shader-source)))
   
(defclass shader-program (resource)
  ((linked?
    :accessor shader-program-linked?
    :initform nil)))

    
    
