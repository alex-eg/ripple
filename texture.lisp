(in-package :texture)

(defclass texture (resource)
  ((data
    :accessor tex-data
    :initarg :data
    :initform nil)
   (type 
    :reader tex-type)))
    

(defmethod load-from-file ((tex texture) (file pathname))
  (let ((file (load-targa-file pathname)))
    ))
    
(defun load-targa-file (path)
  (with-open-file (file path
			:direction :input
			:if-does-not-exist :error
			:element-type '(unsigned-byte 8))
    (
