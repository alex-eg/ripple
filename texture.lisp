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
  (labels ((valid-targa? (stream len)
	     (let ((spec (make-array 16
				     :element-type '(unsigned-byte 8)))
		   (oldpos (file-position stream)))
	       (file-position stream (- len 18))
	       (read-sequence spec stream)
	       (file-position stream oldpos)
	       (string= "TRUEVISION-XFILE"
			(map 'string #'code-char spec)))))
    (with-open-file (tga path
			 :direction :input
			 :if-does-not-exist :error
			 :element-type '(unsigned-byte 8))
      (let ((len (file-length tga)))
	(or (valid-targa? tga len)
	    (error 

