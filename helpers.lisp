(in-package :helpers)

(defun radians (a)
  "Converts degrees to radians"
  (* pi (/ a 180.0)))

(defun make-keyword (name)
  (values (intern (string-upcase name) "KEYWORD")))

(defmacro make-regular-symbol (symbol-name package-name)
  `(values (intern (string-upcase ,symbol-name) ,package-name)))

(defun mouse-right-p ()
  (sdl2:mouse-state-p 3))

(defun mouse-left-p ()
  (sdl2:mouse-state-p 1))
