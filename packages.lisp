(in-package :cl-user)

(defpackage :ripple
  (:use :cl :cl-opengl))

(defpackage :v
  (:use :cl :helpers)
  (:export :x
	   :y
	   :z
	   :normalize
	   :cross
	   :mag))

(defpackage :m
  (:use :cl :helpers)
  (:export :translate
	   :rotate
	   :scale))

(defpackage :helpers
  (:use :cl)
  (:export :radians))

(defpackage :camera
  (:use :cl :helpers)
  (:export :camera
	   :update-matrices))
	   
  
