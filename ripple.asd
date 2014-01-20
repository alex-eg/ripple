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
               (:file "mesh")
               (:file "light-source")
               (:file "material")
               (:file "obj-loader")
               (:file "state")
               (:file "main"))
  :depends-on (:cl-opengl
               :lispbuilder-sdl
               ))
