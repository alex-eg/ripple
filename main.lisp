(in-package :ripple)

(defmacro restartable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue"  )))

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
	      (gl:color 1 1 1)
	      (gl:vertex i j 0))))
  (loop for j from 0 to length by stride do    
       (gl:with-primitive :line-strip
	 (loop for i from 0 to width by stride do
	      (gl:color 1 1 1)
	      (gl:vertex i j 0)))))

(defun draw ()
  "draw a frame"
  (gl:clear :color-buffer-bit)
  ;; draw a triangle
;;  (gl:rotate pi 1 1 0)
  (gl:push-matrix)
;;  (draw-triangle)
  (draw-grid 3 3 0.1)
  (gl:pop-matrix)
  ;; finish the frame
  (gl:flush)
  (sdl:update-display))

(defun main-loop ()
  (sdl:with-init ()
    (sdl:window 640 480 :flags sdl:sdl-opengl)
    ;; cl-opengl needs platform specific support to be able to load GL
    ;; extensions, so we need to tell it how to do so in lispbuilder-sdl
    (setf cl-opengl-bindings:*gl-get-proc-address* 
	  #'sdl-cffi::sdl-gl-get-proc-address)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
             ;; this lets slime keep working while the main loop is running
             ;; in sbcl using the :fd-handler swank:*communication-style*
             ;; (something similar might help in some other lisps, not sure which though)
             #+(and sbcl (not sb-thread)) (restartable
                                           (sb-sys:serve-all-events 0))
             (restartable (draw))))))


(main-loop)
