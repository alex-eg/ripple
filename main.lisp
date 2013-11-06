(in-package :ripple)

(defmacro restartable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun draw-triangle ()
  (gl:with-primitive :triangles
    (gl:color 1 1 1)
    (gl:vertex 0 0 0)
    (gl:color 0 1 0)
    (gl:vertex 0.5 1 0)
    (gl:color 0 0 1)
    (gl:vertex 1 0 0)))

(defun draw-grid (width length stride)
  (loop for i from 0 to width by stride do
       (gl:with-primitive :line-strip
         (loop for j from 0 to length by stride do
              (gl:color 0.76 0.0 0.0)
              (gl:vertex i (* (sin i) (cos j)) j))))
  (loop for j from 0 to length by stride do
       (gl:with-primitive :line-strip
         (loop for i from 0 to width by stride do
              (gl:color 0.76 0.0 0.0)
              (gl:vertex i (* (sin i) (cos j)) j)))))

(defun draw (camera texture)
  "draw a frame"
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (camera:update-matrices camera)
  (gl:translate 15 0.0 15)
  (cl-glut:wire-sphere 30.0 32 32)
  (gl:translate -15 0.0 -15)
  (draw-grid 30 30 0.3)
  (draw-textured-quad texture)
  ;; finish the frame
  (gl:flush)
  (sdl:update-display))

(defun draw-textured-quad (texture)
  (gl:bind-texture :texture-2d texture)
  (gl:color 1.0 1.0 1.0)
  (gl:with-primitive :quads
    (gl:tex-coord 0.0 0.0)
    (gl:vertex 10.0 -5.0 -5.0)
    (gl:tex-coord 4.0 0.0)
    (gl:vertex 10.0 -5.0 5.0)
    (gl:tex-coord 4.0 4.0)
    (gl:vertex 10.0 5.0 5.0)
    (gl:tex-coord 0.0 4.0)
    (gl:vertex 10.0 5.0 -5.0)))

(let ((cam (make-instance 'camera:camera
                          :center #(1.0 0.0 0.0)
                          :eye #(0.0 0.0 0.0)))
      (texture nil)
      (cur-state (make-instance 'state:state)))
  (defun main-loop ()
    (sdl:with-init ()
      (sdl:window 800 600
                  :opengl :hw :double-buffer :resizable)
      (sdl:enable-key-repeat 50 20)
      ;; cl-opengl needs platform specific support to be able to load GL
      ;; extensions, so we need to tell it how to do so in lispbuilder-sdl
      (setf cl-opengl-bindings:*gl-get-proc-address*
            #'sdl-cffi::sdl-gl-get-proc-address)
      (gl:enable :texture-2d)
      (gl:enable :depth-test)
      (cl-glut:init)
      (let ((tex (texture:load-from-file
                  (make-instance 'texture:texture)
                  #P"./resources/textures/checker.tga")))
        (setf texture (first (gl:gen-textures 1)))
        (gl:bind-texture :texture-2d texture)
        (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
        (gl:tex-parameter :texture-2d :texture-wrap-r :repeat)
        (gl:tex-parameter :texture-2d :texture-min-filter :linear)
        (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
        (format t "Loading texture~%")
        (gl:tex-image-2d :texture-2d 0 :rgb
                         (texture:tex-width tex)
                         (texture:tex-height tex)
                         0 :rgb :unsigned-byte
                         (texture:tex-data tex)))
      (format t "Loaded texture~%")

      ;;Creating VAO
      (setf (state:state-vao cur-state) (gl:gen-vertex-array))
      (gl:bind-vertex-array (state:state-vao cur-state))


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
             (let ((new-pos (v:add old-view old-eye))
                   (new-center (v:add old-center old-view)))
               (setf (camera:cam-eye cam) new-pos)
               (setf (camera:cam-center cam) new-center))))
           

         (when (sdl:key= b sdl:sdl-button-wheel-down)
           (camera:with-old-parameters (cam :eye old-eye
                                            :center old-center
                                            :view old-view)
             (let ((new-pos (v:sub old-eye old-view))
                   (new-center (v:sub old-center old-view)))
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
                    (d-dir (v:add (v:mul-num old-dir (/ dy 10))
                                  (v:mul-num strafe (/ dx 10))))

                    (new-eye (v:add eye d-dir))
                    (new-center (v:add center d-dir)))

               (setf (camera:cam-eye cam) new-eye)
               (setf (camera:cam-center cam) new-center)))))
        (:idle ()
               ;; this lets slime keep working while the main loop is running
               ;; in sbcl using the :fd-handler swank:*communication-style*
               ;; (something similar might help in some other lisps, not sure which though)
               #+(and sbcl (not sb-thread)) (restartable
                                              (sb-sys:serve-all-events 0))
               (restartable (draw cam texture)))))))
;;-------------------------
(main-loop)
