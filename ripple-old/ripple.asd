(in-package :cl-user)

(defpackage :ripple-asd
  (:use :cl :asdf))

(in-package :ripple-asd)

(defsystem :ripple
  :serial t
  :components ((:file "packages")
	       (:file "camera")
	       (:file "vector")
	       (:file "matrix")
	       (:file "helpers")
	       (:file "main"))
  :depends-on (:cl-opengl
	       :lispbuilder-sdl 
	       :cl-glu))
