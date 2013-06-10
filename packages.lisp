(in-package :cl-user)

(defpackage :ripple
  (:use :cl :cl-opengl))

(defpackage :v
  (:use :cl)
  (:export :x
	   :y
	   :z))

(defpackage :helpers
  (:use :cl)
  (:export :radians
	   :normalize
	   :cross))

(defpackage :camera
  (:use :cl :helpers)
  (:export :camera
	   :update-matrices))
	   
  
