(in-package :texture)

(defclass texture (resource)
  ((data
    :accessor tex-data
    :initarg :data
    :initform nil)
   (type 
    :accessor tex-type)
   (bpp 
    :accessor tex-bpp)
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
	   (read-header (stream)
	     (let ((header (make-array 18
				       :element-type '(unsigned-byte 8))))
	       (file-position stream 0)
	       (read-sequence header stream)
	       header))
	   (compressed? (stream)
	     (let ((header (read-header stream)))
	       (equal (aref header 2) 10)))
	   (load-common-parts (stream tex-struct)
	     (let ((header (read-header stream)))
	       (setf (tex-width tex-struct)
		     (+ (* 256 (aref header 13))
			(aref header 14)))
	       (setf (tex-height tex-struct)
		     (+ (* 256 (aref header 15))
			(aref header 16)))
	       (setf (tex-type tex-struct)
		     (if (= (aref header 17) 32)
			 :rgba
			 :rgb))
	       (setf (tex-data tex-struct)
		     (make-array (* (/ (aref header 17) 8)
				    (tex-height tex-struct)
				    (tex-width tex-struct))
				 :element-type '(unsigned-byte 8))))
	     tex-struct)
	   (load-uncompressed-targa (stream tex-struct)
	     (setf tex-struct (load-common-parts stream tex-struct))
	     (setf (tex-data tex-struct)
		   (read-sequence (tex-data tex-struct) stream))
	     tex-struct)

	     
	   (load-compressed-targa (stream tex-struct)
	     (setf tex-struct (load-common-parts stream tex-struct))
	     tex-struct))
    (with-open-file (tga path
			 :direction :input
			 :if-does-not-exist :error
			 :element-type '(unsigned-byte 8))
      (let ((len (file-length tga))
	    (tex-struct (make-targa-texture)))
	(or (valid-targa? tga len)
	    (error "File ~A is not of a valid New Targa Format" path))
	(if (compressed? tga)
	    (load-compressed-targa tga tex-struct)
	    (load-uncompressed-targa tga tex-struct))
	tex-struct))))

