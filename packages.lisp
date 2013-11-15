(in-package :cl-user)

(defpackage :ripple
  (:use :cl :cl-opengl))

(defpackage :helpers
  (:use :cl)
  (:export :radians
           :make-regular-symbol
           :make-keyword))

(defpackage :v ; vectors
  (:use :helpers :cl)
  (:shadow cl:+ cl:-)
  (:export :x
           :y
           :z
           :+
           :-
           :*.
           :normalize
           :cross
           :mag))

(defpackage :m ; matrices
  (:use :cl :helpers)
  (:export :translate
           :rotate
           :scale
           :identity-matrix
           :do-matrix
           :transpose
           :*-mat-mat
           :*-mat-num
           :+-mat
           :set-mat-row
           :mat-4
           :matrix-row
           :coerce-matrix
           :coerce-vector
           :matrix-col))

(defpackage :camera
  (:use :cl :helpers)
  (:export :camera
           :cam-eye
           :cam-center
           :cam-up
           :cam-fovy
           :cam-view
           :rotate-yaw
           :rotate-pitch
           :rotate-roll
           :with-old-parameters
           :cam-aspect
           :cam-znear
           :cam-zfar
           :update-matrices))

(defpackage :resource
  (:use :cl :helpers)
  (:export :resource
           :resource-name))

(defpackage :texture
  (:use :cl :helpers :resource)
  (:export :texture
           :tex-type
           :tex-data
           :tex-bpp
           :tex-width
           :tex-height
           :load-from-file))

(defpackage :shader
  (:use :cl :helpers :resource)
  (:export :shader-program
           :program-id
           :set-shader
           :compile-program))

(defpackage :state
  (:use :cl)
  (:export :state
           :state-vao))
