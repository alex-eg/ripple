(in-package :mesh)

(defclass mesh (resource:resource)
  ((vertex-buffer-id
    :initform nil
    :accessor vertex-buffer-id)
   (normal-buffer-id
    :initform nil
    :accessor normal-buffer-id)))

(defun load-mesh (mesh vertex-vector normal-vector)
  (setf (vertex-buffer-id mesh) (gl:gen-buffers 1))
  (gl:bind-buffer :array-buffer (vertex-buffer-id mesh))
  (gl:buffer-data :array-buffer :static-draw vertex-vector)

  (setf (normal-buffer-id mesh) (gl:gen-buffers 1))
  (gl:bind-buffer :array-buffer (normal-buffer-id mesh))
  (gl:buffer-data :array-buffer :static-draw normal-vector))
        

  
