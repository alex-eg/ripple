(in-package :cl-user)

(defpackage :ripple-asd
  (:use :cl :asdf))

(in-package :ripple-asd)

(defsystem :ripple
  :components ((:file "packages")
	       (:file "camera")
	       (:file "vector")
	       (:file "matrix")
	       (:file "helpers")
	       (:file "resource")
	       (:file "texture")
	       (:file "shader")
	       (:file "main"))
  :depends-on (:cl-opengl
	       :lispbuilder-sdl 
	       :cl-glut
	       :cl-glu))
