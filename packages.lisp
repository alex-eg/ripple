(in-package :cl-user)

(defpackage :ripple
  (:use :cl :cl-opengl))

(defpackage :helpers
  (:use :cl)
  (:export :radians))

(defpackage :v ; vectors
  (:use :cl :helpers)
  (:export :x
	   :y
	   :z
	   :add
	   :sub
	   :mul-num
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
	   :with-old-parameters
	   :cam-aspect
	   :cam-znear
	   :cam-zfar
	   :update-matrices))

(defpackage :resource
  (:use :cl :helpers))

(defpackage :texture
  (:use :cl :helpers :resource)
  (:export :texture))

(defpackage :shader
  (:use :cl :helpers :resource)
  (:export :shader
	   :vertex
	   :fragment
	   :program))
