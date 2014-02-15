(in-package :ripple)

(defmacro restartable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defvar *rotate-matrix* (m:mat-4 (m:rotate #(0.0 1.0 1.0) 0.157)))
(defvar *angle* 0.157)

(defun draw (state)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (let* ((blinn (shader:program-id (state:get state :shader 'blinn)))
         (cam (state:get state :camera 'main))
         (steel (state:get state :material 'steel))
         (omni (state:get state :light-source 'omni))

         (light-pos (gl:get-uniform-location blinn "lightPosition"))
         (light-color (gl:get-uniform-location blinn "lightColor"))
         (ambient (gl:get-uniform-location blinn "ambient"))
         (diffuse (gl:get-uniform-location blinn "diffuse"))
         (specular (gl:get-uniform-location blinn "specular"))
         (emission (gl:get-uniform-location blinn "emission"))
         (shininess (gl:get-uniform-location blinn "shininess"))

         (rotation (gl:get-uniform-location blinn "rot"))
         (model-view (gl:get-uniform-location blinn "MV"))
         (projection (gl:get-uniform-location blinn "P")))
    (camera:update-matrices cam)

    (gl:uniform-matrix model-view 4 (vector (camera:cam-model-view-matrix cam)))
    (gl:uniform-matrix projection 4 (vector (camera:cam-projection-matrix cam)))
    (gl:uniform-matrix rotation 4 (vector *rotate-matrix*))

    (setf *rotate-matrix* (m:mat-4 (m:rotate #(1.0 1.0 1.0) *angle*)))
    (setf *angle* (+ *angle* 0.0157))
    (if (> *angle* (* 2 PI))
        (setf *angle* (- *angle* (* 2 PI))))

    (gl:uniformfv light-pos (light-source:position omni))
    (gl:uniformfv light-color (light-source:color omni))
    (gl:uniformfv ambient (material:ambient steel))
    (gl:uniformfv diffuse (material:diffuse steel))
    (gl:uniformfv specular (material:specular steel))
    (gl:uniformfv emission (material:emission steel))
    (gl:uniformf shininess (material:shininess steel)))

  (state:render-mesh state 'suzanne)
  (sdl:update-display))

(defun main-loop ()
  (let ((*default-pathname-defaults* (asdf:system-source-directory :ripple))
        (current-state (make-instance
                        'state:state)))
    (state:add current-state :camera 'main
               (make-instance 'camera:view-camera
                              :up #(0.0 0.0 1.0)
                              :center #(0.0 1.0 0.0)
                              :eye #(0.0 -10.0 0.0)))
    (state:add current-state :light-source 'omni
               (make-instance 'light-source:omni
                              :position #(15.0 15.0 15.0 1.0)
                              :color #(0.7 0.7 0.7 1.0)))
    (state:add current-state :material 'steel
               (make-instance 'material:material
                              :ambient #(0.1 0.1 0.1 1.0)
                              :diffuse #(0.3 0.5 0.8 1.0)
                              :specular #(1.0 1.0 1.0 1.0)
                              :emission #(0.0 0.0 0.0 1.0)
                              :shininess 500.0))
    (state:add current-state :texture 'checker
               (texture:load-from-file
                (make-instance 'texture:texture)
                #P"./resources/textures/checker.tga"))
    (state:add current-state :shader 'blinn
               (make-instance 'shader:shader-program))

    (state:add current-state :mesh 'triangle
               (make-instance 'mesh:mesh))
    (state:add current-state :mesh 'hexagon
               (make-instance 'mesh:mesh))
    (state:add current-state :mesh 'suzanne
               (make-instance 'mesh:mesh))
    (sdl:with-init ()
      (sdl:window 800 600
                  :opengl :hw :double-buffer :resizable)
      (sdl:enable-key-repeat 50 20)
      ;; cl-opengl needs platform specific support to be able to load GL
      ;; extensions, so we need to tell it how to do so in lispbuilder-sdl
      (setf cl-opengl-bindings:*gl-get-proc-address*
            #'sdl-cffi::sdl-gl-get-proc-address)

      (format t "OpenGL version string: ~a~%" (gl:gl-version))
      (format t "GLSL version string: ~a~%" (gl:glsl-version))

      (gl:enable :depth-test)
      (gl:clear-color 0.0 0.27 0.37 1.0)
      (let ((blinn (state:get current-state :shader 'blinn))
            (cam (state:get current-state :camera 'main)))
        (shader:set-shader blinn :fragment-shader
                           #P"./resources/shaders/light.frag.glsl")
        (shader:set-shader blinn :vertex-shader
                           #P"./resources/shaders/light.vert.glsl")
        (shader:compile-program blinn)
        (gl:use-program (shader:program-id blinn))

        ;;Creating VAO
        (setf (state:state-vao current-state) (gl:gen-vertex-array))
        (gl:bind-vertex-array (state:state-vao current-state))

        (mesh:load-mesh (state:get current-state :mesh 'triangle)
                        '(#( 0.0 -2.0  1.0  1.0)
                          #( 2.0  2.0  1.0  1.0)
                          #(-2.0  2.0  1.0  1.0))
                        '(#(-1.0 1.0 1.0)
                          #(1.0 -1.0 1.0)
                          #(1.0 1.0 -1.0)))

        (obj-loader:load-mesh-from-file (state:get current-state :mesh 'suzanne)
                                        #P"./resources/models/suzanne.obj")
        (destructuring-bind (verts normals vert-index normal-index)
            (procedural:hexagonal-grid 0 0 0.5 1.0)
          (obj-loader:load-mesh-from-lists (state:get current-state :mesh 'hexagon)
                                           verts normals vert-index nil normal-index))
        (sdl:with-events ()
          (:key-down-event
           (:key key)
           (when (sdl:key= key :sdl-key-q)
             (camera:rotate-roll cam 1.9))
           (when (sdl:key= key :sdl-key-e)
             (camera:rotate-roll cam -1.9)))

          (:quit-event () t)
          (:mouse-button-down-event
           (:button b)
           (when (sdl:key= b sdl:sdl-button-wheel-up)
             (camera:move-forward cam 1.0))
           (when (sdl:key= b sdl:sdl-button-wheel-down)
             (camera:move-forward cam -1.0)))

          (:mouse-motion-event
           (:x-rel dx :y-rel dy)
           (when (sdl:mouse-right-p) ;; rotate view
             (camera:rotate-yaw cam (/ dx -10.0))
             (camera:rotate-pitch cam (/ dy 10.0)))
           (when (sdl:mouse-left-p) ;; move through the field
             (if (sdl:get-key-state :sdl-key-lshift)
               (camera:move-vertical cam (/ dx 100.0) (/ dy 100.0))
               (camera:move-side cam (/ dx 100.0) (/ dy 100.0)))))
          (:idle ()
                 ;; this lets slime keep working while the main loop is running
                 ;; in sbcl using the :fd-handler swank:*communication-style*
                 ;; (something similar might help in some other lisps, not
                 ;; sure which though)
                 #+(and sbcl (not sb-thread)) (restartable
                                                (sb-sys:serve-all-events 0))
                 (restartable (draw
                               current-state))))))))
