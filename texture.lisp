(in-package :texture)

(defclass texture (resource)
  ((data
    :accessor tex-data
    :initarg :data
    :initform nil)
   (type 
    :reader tex-type)
   (height :accessor tex-height)
   (width :accessor tex-width)))

(defstruct targa-texture
  data
  height
  width
  type)
 
(defmethod load-from-file ((tex texture) (file pathname))
  (let ((targa-texture (load-targa-file pathname)))
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
			(map 'string #'code-char spec))))
	     (compressed? (stream)
	       (let ((header (make-array 12
					 :element-type '(unsigned-byte 8)))
		     (oldpos (file-position stream)))
		 (file-position stream 0)
		 (read-sequence header stream)
		 (file-position stream oldpos)
		 (equal (aref header 2) 10))))
    (with-open-file (tga path
			 :direction :input
			 :if-does-not-exist :error
			 :element-type '(unsigned-byte 8))
      (let ((len (file-length tga))
	    (tex-struct (make-targa-texture)))
	(or (valid-targa? tga len)
	    (error "File ~A is not of a valid New Targa Format" path))
	(format t "This file contains ~Ax~A image, color encoding is ~A~%"
		(
	(if (compressed? tga)
	    
	    (format t "File is not a compressed targa"))))))
	

