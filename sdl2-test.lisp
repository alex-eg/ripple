(in-package :sdl2-test)

(require :sdl2)
(require :cl-opengl)

(defun basic-test ()
  "The kitchen sink."
  (sdl2:with-init ()
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)
    (sdl2:with-window (win :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
          ;; basic window/gl setup
          (format t "Setting up window/gl.~%")
          (finish-output)
          (sdl2:gl-make-current win gl-context)
          (gl:viewport 0 0 800 600)
          (gl:matrix-mode :projection)
          (gl:ortho -2 2 -2 2 -2 2)
          (gl:matrix-mode :modelview)
          (gl:load-identity)
          (gl:clear-color 0.0 0.0 1.0 1.0)
          (gl:clear :color-buffer)
          (format t "Opening game controllers.~%")
          (finish-output)
          ;; main loop
          (format t "Beginning main loop.~%")
          (finish-output)
          (sdl2:with-event-loop (:method :poll)
            (:keydown
             (:keysym keysym)
             (let ((scancode (sdl2:scancode-value keysym))
                   (sym (sdl2:sym-value keysym))
                   (mod-value (sdl2:mod-value keysym)))
               (cond
                 ((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
                 ((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
                 ((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor)))
               (format t "Key sym: ~a, code: ~a, mod: ~a~%"
                       sym
                       scancode
                       mod-value)))
            (:keyup
             (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
               (sdl2:push-event :quit)))
            (:mousemotion
             (:x x :y y :xrel xrel :yrel yrel :state state)
             (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
                     x xrel y yrel state))
            (:idle
             ()
             (restartable (draw win)))

            (:quit () t))))))

(defmacro restartable (&body body)
  "helper macro since we use continue restarts a lot
 (remember to hit C in slime or pick the restart so errors don't kill the app)"
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun draw (win)
  (gl:clear :color-buffer)
  (gl:begin :triangles)
  (gl:color 1.0 0.0 0.0)
  (gl:vertex 0.0 1.0)
  (gl:vertex -1.0 -1.0)
  (gl:vertex 1.0 -1.0)
  (gl:end)
  (gl:flush)
  (sdl2:gl-swap-window win))
