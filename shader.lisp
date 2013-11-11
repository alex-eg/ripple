(in-package :shader)

(defclass shader (resource:resource)
  ((filename
    :accessor shader-filename
    :initform nil
    :initarg :path)
   (type
    :accessor shader-type
    :initarg :shader-type)
   (compiled?
    :accessor shader-compiled?
    :initform nil)
   (source
    :accessor shader-source
    :initform "")))

(defclass shader-program (resource:resource)
  ((program
    :accessor program-id
    :initform nil)
   ;; Shader source file paths
   (vertex-shader
    :accessor vertex-shader
    :initform nil)
   (tess-control-shader
    :accessor tess-control-shader
    :initform nil)
   (tess-evaluation-shader
    :accessor tess-evaluation-shader
    :initform nil)
   (geometry-shader
    :accessor geometry-shader
    :initform nil)
   (fragment-shader
    :accessor fragment-shader
    :initform nil)
   (compute-shader
    :accessor compute-shader
    :initform nil)))

;;----------------------------------------------

(defmethod set-shader ((program shader-program) (shader-path pathname)
                       (shader-type symbol))
  (setf (slot-value program shader-type) shader-path))

(flet ((get-log (get-log-function id name)
         (let ((log (funcall get-log-function id)))
           (if (> (length log) 0)
               (format t "~A reports: ~A~%" name log)))))

  (defun get-compile-errors (shader-id)
    (get-log #'gl:get-shader-info-log shader-id "Shader"))

  (defun get-link-errors (program-id)
    (get-log #'gl:get-program-info-log program-id "Program")))

(defun load-shader-from-file (source)
  (let ((shader-lines nil))
    (with-open-file (shader-file
                     source
                     :direction :input
                     :if-does-not-exist :error)
      (do ((line (read-line shader-file nil)
                 (read-line shader-file nil)))
          ((null line))
        (setf shader-lines (cons line shader-lines))))
      (reverse shader-lines)))

(defun shader-loadedp (program type)
  (slot-value program type))

(defun compile-program (program)
  (setf (program-id program) (gl:create-program))
  (let ((shader-type-list '(:vertex-shader
                            :tess-control-shader
                            :tess-evaluation-shader
                            :geometry-shader
                            :fragment-shader
                            :compute-shader))
        (attached-shaders '()))
    (labels ((load-and-compile-shader (program type)
               (let* ((shader-id (gl:create-shader type))
                      (program-id (program-id program))
                      (shader-path (slot-value program type))
                      (shader-source (load-shader-from-file
                                      shader-path)))
                 (gl:attach-shader program-id shader-id)
                 (gl:shader-source shader-id shader-source)
                 (gl:compile-shader shader-id)
                 (get-compile-errors shader-id)
                 (setf attached-shaders
                       (append attached-shaders (list shader-id))))))
      (mapcar (lambda (type)
                (if (shader-loadedp program type)
                    (load-and-compile-shader program type)))
              shader-type-list)
      (gl:link-program (program-id program))
      (get-link-errors (program-id program))
      (mapcar (lambda (shader-id)
                (gl:detach-shader (program-id program)
                                  shader-id)
                (gl:delete-shader shader-id))
              attached-shaders))))
