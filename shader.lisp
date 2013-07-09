(in-package :shader)

(defclass shader (resource)
  ((filename 
    :accessor shader-filename
    :initarg :path)
   (compiled?
    :accessor shader-program-compiled?
    :initform nil)
   (type
    :accessor shader-type
    :initarg :shader-type)
   (compiled?
    :accessor shader-compiled?
    :iniform nil)
   (source
    :accessor shader-source)))
   
(defclass shader-program (resource)
  ((linked?
    :accessor shader-program-linked?
    :initform nil)))

    
    
