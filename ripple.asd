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
               (:file "resource")
               (:file "texture")
               (:file "shader")
               (:file "state")
               (:file "main"))
  :depends-on (:cl-opengl
               :lispbuilder-sdl
               :cl-glut
               :cl-glu))
