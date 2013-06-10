(in-package :cl-user)

(defpackage :ripple
  (:use :cl :cl-opengl))

(defpackage :helpers
  (:use :cl)
  (:export :radians
	   :normalize
	   :cross))

(defpackage :camera
  (:use :cl :helpers)
  (:export :camera
	   :update-matrices))
	   
  
