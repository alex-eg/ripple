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
    :initform (make-hash-table))))

(defmacro add (state type name thing)
  (let* ((slot (cdr
                (assoc (make-regular-symbol 
                        (symbol-name type) "STATE")
                       '((shader . "shader-pool")
                         (camera . "camera-pool")
                         (texture . "texture-pool")))))
         (hash (slot-value state (make-regular-symbol slot "STATE"))))
    `(setf (gethash ,name ,hash) ,thing)))

(defun use-shader-program (state program-name)
  (let ((shader (gethash program-name (shader-pool state))))
    (gl:use-program (shader:program-id shader))))

(defun use-default-program ()
  (gl:use-program 0))
