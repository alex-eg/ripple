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
   (mesh-pool
    :accessor mesh-pool
    :initform (make-hash-table))
   (texture-pool
    :accessor texture-pool
    :initform (make-hash-table))))

;; When add new resource pools to state class,
;; don't forget to add them to the list below.

(defun get-pool-accessor (type)
  (cdr (assoc
        type
        '((:shader . "shader-pool")
          (:camera . "camera-pool")
          (:mesh . "mesh-pool")
          (:texture . "texture-pool")))))

(defmacro get (state type name)
  (setf type (or (and (symbolp type) type)
                 (eval type)))
  (let* ((slot (get-pool-accessor type)))
    `(gethash ,name
              (slot-value
               ,state
               (make-regular-symbol ,slot "STATE")))))

(defmacro add (state type name thing)
  (setf type (or (and (symbolp type) type)
                 (eval type)))
  (let* ((slot (get-pool-accessor type)))
    `(setf (gethash ,name
                    (slot-value
                     ,state
                     (make-regular-symbol ,slot "STATE")))
           ,thing)))

(defun use-shader-program (state program-name)
  (let ((shader (gethash program-name (shader-pool state))))
    (gl:use-program (shader:program-id shader))))

(defun use-default-program ()
  (gl:use-program 0))

(defun render-mesh (state mesh-string-name)
  (let ((mesh (get state :mesh mesh-string-name)))
    (gl:enable-vertex-attrib-array 0)
    (gl:bind-buffer :array-buffer (mesh:vertex-buffer-id mesh))
    (gl:vertex-attrib-pointer 0 4 :float nil 0 0)
    ;; layout in shader ______| |    |    |  | |
    ;; size ____________________|    |    |  | |
    ;; type _________________________|    |  | |
    ;; normalized? _______________________|  | |
    ;; stride _______________________________| |
    ;; array buffer offset ____________________|

    (gl:enable-vertex-attrib-array 1)
    (gl:bind-buffer :array-buffer (mesh:normal-buffer-id mesh))
    (gl:vertex-attrib-pointer 1 3 :float nil 0 0)

    (gl:draw-arrays :triangles 0 (mesh:verts-count mesh))

    (gl:disable-vertex-attrib-array 0)
    (gl:disable-vertex-attrib-array 1)))
