(in-package :cl-user)

(defpackage :ripple
  (:use :cl :cl-opengl))

(defpackage :helpers
  (:use :cl)
  (:export :radians
	   :with-mouse-button))

(defpackage :v
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

(defpackage :m
  (:use :cl :helpers)
  (:export :translate
	   :rotate
	   :scale))

(defpackage :camera
  (:use :cl :helpers)
  (:export :camera
	   :cam-center
	   :cam-up
	   :cam-fovy
	   :cam-eye
	   :with-old-parameters
	   :cam-aspect
	   :cam-znear
	   :cam-zfar
	   :update-matrices))
	   
  
