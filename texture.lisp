(in-package :texture)

(defclass texture (resource:resource)
  ((data
    :accessor tex-data
    :initarg :data
    :initform nil)
   (type 
    :accessor tex-type
    :initform :not-set)
   (bpp 
    :accessor tex-bpp
    :initform 24)
   (height :accessor tex-height)
   (width :accessor tex-width)))

(defstruct targa-texture
  data
  height
  width
  type)
 
(defmethod load-from-file ((tex texture) (file pathname))
  (let ((targa-texture (load-targa-file file)))
    targa-texture))
    
(defun load-targa-file (path)
  (labels 
      ((valid-targa? (stream len)
	 "Check if file is a properly formed NEW Targa file"
	 (let ((spec (make-array 
		      16
		      :element-type '(unsigned-byte 8)))
	       (oldpos (file-position stream)))
	   (file-position stream (- len 18))
	   (read-sequence spec stream)
	   (file-position stream oldpos)
	   (string= "TRUEVISION-XFILE"
		    (map 'string #'code-char spec))))
  ;; --------------------------------------
       (read-header (stream)
	 "Reset stream position and read targa header"
	 (let ((header (make-array 
			18
			:element-type '(unsigned-byte 8))))
	   (file-position stream 0)
	   (read-sequence header stream)
	   header))
  ;; --------------------------------------
       (compressed? (stream)
	 "Check if file is RLE compressed"
	 (let ((header (read-header stream)))
	   (equal (aref header 2) 10)))
  ;; --------------------------------------
       (load-common-parts (stream texture)
	 "Load texture parameters, common for both compressed and ~
uncompressed types"
	 (let ((header (read-header stream)))
	   (setf (tex-width texture)
		 (+ (* 256 (aref header 11))
		    (aref header 12)))
	   (setf (tex-height texture)
		 (+ (* 256 (aref header 13))
		    (aref header 14)))
	   (cond ((= (aref header 16) 32)
		  (setf (tex-type texture) :rgba)
		  (setf (tex-bpp texture) 32))
		 ((= (aref header 16) 24)
		  (setf (tex-type texture) :rgb)
		  (setf (tex-bpp texture) 24)))
	   (setf (tex-data texture)
		 (make-array (* (/ (aref header 16) 8)
				(tex-height texture)
				(tex-width texture))
			     :element-type '(unsigned-byte 8))))
	 texture)
  ;; --------------------------------------
       (load-uncompressed-targa (stream texture)
	 "Load rest of the uncompressed texture"
	 (setf texture (load-common-parts stream texture))
	 (setf (tex-data texture)
	       (read-sequence (tex-data texture) stream))
	 texture)
  ;; --------------------------------------
       (load-compressed-targa (stream texture)
	 "Load rest of RLE compressed texture"
	 (setf texture (load-common-parts stream texture))
		 (tex-width texture)
		 (tex-height texture)
		 (tex-bpp texture)
		 (tex-type texture)
		 (length (tex-data texture))
		 (file-position stream)
		 (file-length stream))
	 (let ((pixel-count (* (tex-width texture)
			       (tex-height texture)))
	       (bytes-per-pixel (/ (tex-bpp texture) 8)))
	   (do ((current-pixel 0)
		(current-byte 0))
	       ((>= current-pixel pixel-count))
	     (let* ((chunk-header (read-byte stream))
		    (buffer (make-array 
			     bytes-per-pixel
			     :element-type '(unsigned-byte 8))))
	       
	       (if (< chunk-header 128)
		   ;;; Raw chunk
		   (progn 
		     (incf chunk-header)
		     (dotimes (count chunk-header)
		       (read-sequence buffer stream)
		       (let ((new (destructuring-bind (c b a . d)
				      (coerce buffer 'list)
				    (or (and d (vector a b c (first d)))
					(vector a b c)))))
			 (setf (subseq (tex-data texture)
				       current-byte
				       (+ bytes-per-pixel current-byte))
			       new))
		       (incf current-pixel)
		       (setf current-byte (+ current-byte bytes-per-pixel))))
		   ;;; Else - RLE chunk
		   (progn
		     (setf chunk-header (- chunk-header 127))
		     (read-sequence buffer stream)
		     (let ((new (destructuring-bind (c b a . d)
				    (coerce buffer 'list)
				  (or (and d (vector a b c (first d)))
				      (vector a b c)))))
		       (dotimes (count chunk-header)
			 (setf (subseq (tex-data texture)
				       current-byte
				       (+ bytes-per-pixel current-byte))
			       new)
			 (incf current-pixel)
			 (setf current-byte (+ current-byte bytes-per-pixel))))))))
	   texture)))
    (with-open-file (tga path
			 :direction :input
			 :if-does-not-exist :error
			 :element-type '(unsigned-byte 8))
      (let ((len (file-length tga))
	    (texture (make-instance 'texture)))
	(or (valid-targa? tga len)
	    (error "File ~A is not of a valid New Targa Format" path))
	(if (compressed? tga)
	    (load-compressed-targa tga texture)
	    (load-uncompressed-targa tga texture))
	texture))))

