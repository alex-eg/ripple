(in-package :cl-user)

(defpackage :ripple
  (:use :cl :cl-opengl))

(defpackage :helpers
  (:use :cl)
  (:export :radians))

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
	   :cam-position
	   :cam-up
	   :cam-fovy
	   :cam-sight
	   :cam-aspect
	   :cam-znear
	   :cam-zfar
	   :update-matrices))
	   
  
