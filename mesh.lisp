(in-package :mesh)

(defclass mesh (resource:resource)
  ((vertex-buffer-id
    :initform nil
    :accessor vertex-buffer-id)
   (normal-buffer-id
    :initform nil
    :accessor normal-buffer-id)
   (verts-count
    :initform 0
    :accessor verts-count)))

(defun load-mesh (mesh vertex-list normal-list)
  (setf (vertex-buffer-id mesh) (car (gl:gen-buffers 1)))
  (setf (verts-count mesh) (length vertex-list))
  (gl:bind-buffer :array-buffer (vertex-buffer-id mesh))
  (let* ((elt-len (length (car vertex-list)))
         (arr (gl:alloc-gl-array :float (* (length vertex-list)
                                           elt-len))))
    (dotimes (i (length vertex-list))
      (dotimes (j elt-len)
        (setf (gl:glaref arr (+ (* i elt-len) j))
              (svref (elt vertex-list i) j))))
    (gl:buffer-data :array-buffer :static-draw arr))

  (setf (normal-buffer-id mesh) (car (gl:gen-buffers 1)))
  (gl:bind-buffer :array-buffer (normal-buffer-id mesh))
  (let* ((elt-len (length (car normal-list)))
         (arr (gl:alloc-gl-array :float (* (length normal-list)
                                           elt-len))))
    (dotimes (i (length normal-list))
      (dotimes (j elt-len)
        (setf (gl:glaref arr (+ (* i elt-len) j))
              (svref (elt normal-list i) j))))
    (gl:buffer-data :array-buffer :static-draw arr)))
