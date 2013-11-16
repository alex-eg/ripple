(in-package :shader)

(defclass shader (resource)
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

(defclass shader-program (resource)
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

(defun load-shader-from-file (source)
  (let ((shader-lines nil))
    (with-open-file (shader-file
                     source
                     :direction :input
                     :if-does-not-exist :error)
      (do ((line (read-line shader-file nil)
                 (read-line shader-file nil)))
          ((null line))
        (setf shader-lines (cons (format nil "~a~%" line)
                                 shader-lines))))
      (reverse shader-lines)))

(flet ((get-log (get-log-function id name)
         (let ((log (funcall get-log-function id)))
           (if (> (length log) 0)
               (format t "~A reports:~%~A~%" name log)))))
  (defun get-compile-errors (shader-id shader-type shader-name)
    (get-log #'gl:get-shader-info-log shader-id 
             (format nil "~A shader ~A" 
                     (string-capitalize
                      (subseq shader-type 0
                              (position #\- shader-type)))
                     shader-name)))
  (defun get-link-errors (program-id)
    (get-log #'gl:get-program-info-log program-id "Program")))

(defmacro set-shader (program shader-path shader-type)
  `(setf (slot-value ,program 
                     (intern (string-upcase (symbol-name ,shader-type))
                             "SHADER"))
         ,shader-path))

(defun compile-program (program)
  (setf (program-id program) (gl:create-program))
  (let ((shader-type-list '("vertex-shader"
                            "tess-control-shader"
                            "tess-evaluation-shader"
                            "geometry-shader"
                            "fragment-shader"
                            "compute-shader"))
        (attached-shaders '()))
    (labels ((shader-loaded? (program type)
               (slot-value program (make-regular-symbol type "SHADER")))
             (load-and-compile-shader (program type)
               (let* ((shader-id (gl:create-shader (make-keyword type)))
                      (program-id (program-id program))
                      (shader-path (slot-value
                                    program
                                    (make-regular-symbol type "SHADER")))
                      (shader-source (load-shader-from-file
                                      shader-path)))
                 (gl:attach-shader program-id shader-id)
                 (gl:shader-source shader-id shader-source)
                 (gl:compile-shader shader-id)
                 (get-compile-errors shader-id type shader-path)
                 (setf attached-shaders
                       (append attached-shaders (list shader-id))))))
      (mapcar (lambda (type)
                (if (shader-loaded? program type)
                    (load-and-compile-shader program type)))
              shader-type-list)
      (gl:link-program (program-id program))
      (get-link-errors (program-id program))
      (mapcar (lambda (shader-id)
                (gl:detach-shader (program-id program)
                                  shader-id)
                (gl:delete-shader shader-id))
              attached-shaders))))
