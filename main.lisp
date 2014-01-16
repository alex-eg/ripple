(in-package :ripple)

(defmacro restartable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun draw (state)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (let* ((blinn (shader:program-id (state:get state :shader "blinn")))
         (cam (state:get state :camera "main"))

         (light-pos (gl:get-uniform-location blinn "lightPosition"))
         (light-color (gl:get-uniform-location blinn "lightColor"))
         (ambient (gl:get-uniform-location blinn "ambient"))
         (diffuse (gl:get-uniform-location blinn "diffuse"))
         (specular (gl:get-uniform-location blinn "specular"))
         (emission (gl:get-uniform-location blinn "emission"))
         (shininess (gl:get-uniform-location blinn "shininess"))

         (model-view (gl:get-uniform-location blinn "MV"))
         (projection (gl:get-uniform-location blinn "P")))
    (camera:update-matrices cam)
    (gl:uniform-matrix model-view 4 (vector (camera:cam-model-view-matrix cam)))
    (gl:uniform-matrix projection 4 (vector (camera:cam-projection-matrix cam))))

  (state:render-mesh state "triangle")
  (sdl:update-display))

(defun main-loop ()
  (let ((current-state (make-instance
                        'state:state)))
    (state:add current-state :camera "main"
               (make-instance 'camera:camera
                              :up #(0.0 -1.0 0.0)
                              :center #(0.0 0.0 1.0)
                              :eye #(0.0 0.0 0.0)))
    (state:add current-state :texture "checker"
               (texture:load-from-file
                (make-instance 'texture:texture)
                #P"./resources/textures/checker.tga"))
    (state:add current-state :shader "blinn"
               (make-instance 'shader:shader-program))

    (state:add current-state :mesh "triangle"
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
      (let ((blinn (state:get current-state :shader "blinn"))
            (cam (state:get current-state :camera "main")))
        (shader:set-shader blinn :fragment-shader
                           #P"./resources/shaders/light.frag.glsl")
        (shader:set-shader blinn :vertex-shader
                           #P"./resources/shaders/light.vert.glsl")
        (shader:compile-program blinn)
        (gl:use-program (shader:program-id blinn))

        ;;Creating VAO
        (setf (state:state-vao current-state) (gl:gen-vertex-array))
        (gl:bind-vertex-array (state:state-vao current-state))

        (mesh:load-mesh (state:get current-state :mesh "triangle")
                        '(#( 0.0 -2.0  4.5  1.0)
                          #( 2.0  2.0  4.5  1.0)
                          #(-2.0  2.0  4.5  1.0))
                        '(#(-1.0 1.0 1.0)
                          #(1.0 -1.0 1.0)
                          #(1.0 1.0 -1.0)))

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
             (camera:with-old-parameters (cam :eye old-eye
                                              :center old-center
                                              :view old-view)
               (let ((new-pos (v:+ old-view old-eye))
                     (new-center (v:+ old-center old-view)))
                 (setf (camera:cam-eye cam) new-pos)
                 (setf (camera:cam-center cam) new-center))))

           (when (sdl:key= b sdl:sdl-button-wheel-down)
             (camera:with-old-parameters (cam :eye old-eye
                                              :center old-center
                                              :view old-view)
               (let ((new-pos (v:- old-eye old-view))
                     (new-center (v:- old-center old-view)))
                 (setf (camera:cam-center cam) new-center)
                 (setf (camera:cam-eye cam) new-pos)))))

          (:mouse-motion-event
           (:x-rel dx :y-rel dy)
           (when (sdl:mouse-right-p) ;; rotate view
             (camera:rotate-yaw cam (/ dx -10.0))
             (camera:rotate-pitch cam (/ dy 10.0)))
           (when (sdl:mouse-left-p) ;; move through the field
             (camera:with-old-parameters (cam :eye eye
                                              :center center
                                              :up up
                                              :view view)
               (let* ((old-dir (vector (v:x view) 0.0 (v:z view)))
                      (strafe (v:cross up view))
                      (d-dir (v:+ (v:*. old-dir (/ dy 10))
                                  (v:*. strafe (/ dx 10))))

                      (new-eye (v:+ eye d-dir))
                      (new-center (v:+ center d-dir)))

                 (setf (camera:cam-eye cam) new-eye)
                 (setf (camera:cam-center cam) new-center)))))
          (:idle ()
                 ;; this lets slime keep working while the main loop is running
                 ;; in sbcl using the :fd-handler swank:*communication-style*
                 ;; (something similar might help in some other lisps, not
                 ;; sure which though)
                 #+(and sbcl (not sb-thread)) (restartable
                                                (sb-sys:serve-all-events 0))
                 (restartable (draw
                               current-state))))))))
;;-----------------------------------------------
(main-loop)
