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
  (let ((h 0))
    (loop for i from 0 to width by stride do
	 (gl:with-primitive :line-strip
	   (loop for j from 0 to length by stride do    
		(gl:color 0.0 0.38 0.38)
		(gl:vertex i (* (sin i) (cos j)) j))))
    (loop for j from 0 to length by stride do    
	 (gl:with-primitive :line-strip
	   (loop for i from 0 to width by stride do
		(gl:color 0.0 0.38 0.38)
		(gl:vertex i (* (sin i) (cos j)) j))))))

(defvar *cam*)
(setf *cam* (make-instance 'camera:camera
			   :eye #(0 -3 0)))
(defun draw ()
  "draw a frame"
  (gl:clear :color-buffer-bit)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (camera:update-matrices *cam*)
  ;; draw a triangle
  ;;  (draw-triangle)
  (gl:rotate (/ pi 3.0) 0 1 0)
  (draw-grid 30 30 0.3)
  ;; finish the frame
  (gl:flush)
  (sdl:update-display))

(defun main-loop ()
  (sdl:with-init ()
    (sdl:window 640 480 
		:opengl :hw :double-buffer :resizable)
    ;; cl-opengl needs platform specific support to be able to load GL
    ;; extensions, so we need to tell it how to do so in lispbuilder-sdl
    (setf cl-opengl-bindings:*gl-get-proc-address* 
	  #'sdl-cffi::sdl-gl-get-proc-address)
    
    (sdl:with-events ()
      (:quit-event () t)
      (:mouse-button-down-event 
       (:button b)
       
       (cond ((= b sdl:mouse-wheel-up)
      	      (camera:with-old-parameters (*cam* :eye old-eye 
						 :center old-center
						 :view old-view)
      		(let ((newpos (v:add old-view old-eye)))
      		  (setf (camera:cam-eye *cam*) newpos))))
	     
      	     ((= b sdl:mouse-wheel-down)
      	      (camera:with-old-parameters (*cam* :eye old-eye 
						 :center old-center
						 :view old-view)
      		(let ((newpos (v:sub old-eye old-view)))
      		  (setf (camera:cam-eye *cam*) newpos))))

      	     (t (format t "button ~A pressed~%" b))))
      
      (:mouse-motion-event 
       (:x-rel dx :y-rel dy)
       (when (sdl:mouse-right-p)
	 (camera:with-old-parameters (*cam* :eye eye
					    :center center
					    :up up
					    :view view)
	   (let* ((strafe (v:normalize (v:cross up view)))
		  (d-strafe (v:mul-num strafe (/ dx 10)))
		  (d-updown (v:mul-num (vector 0.0 1.0 0.0) (/ dy 10)))
		  (d-view (v:add d-strafe d-updown))
		  (new-view (v:add view d-view))
		  (new-center (v:add eye new-view))
		  (new-up (v:normalize (v:cross new-view strafe))))
	     (setf (camera:cam-center *cam*) new-center)
	     (setf (camera:cam-up *cam*) new-up))))
       
       (when (sdl:mouse-left-p)
	 (camera:with-old-parameters (*cam* :eye eye
					    :center center
					    :up up
					    :view view)
	   (let* ((old-dir (vector (v:x view) 0.0 (v:z view)))
		  (strafe (v:cross up view))
		  (d-dir (v:add (v:mul-num old-dir (/ dy 10))
				(v:mul-num strafe (/ dx 10))))

		  (new-eye (v:add eye d-dir))
		  (new-center (v:add center d-dir)))
	     
	     (setf (camera:cam-eye *cam*) new-eye)
	     (setf (camera:cam-center *cam*) new-center)))))
      (:idle ()
             ;; this lets slime keep working while the main loop is running
             ;; in sbcl using the :fd-handler swank:*communication-style*
             ;; (something similar might help in some other lisps, not sure which though)
             #+(and sbcl (not sb-thread)) (restartable
					    (sb-sys:serve-all-events 0))
             (restartable (draw))))))


(main-loop)
