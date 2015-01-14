(in-package :ripple)

(defclass material (resource)
  ((ambient
    :accessor ambient
    :initarg :ambient
    :initform #(1.0 1.0 1.0 1.0))
   (diffuse
    :accessor diffuse
    :initarg :diffuse
    :initform #(1.0 1.0 1.0 1.0))
   (specular
    :accessor specular
    :initarg :specular
    :initform #(1.0 1.0 1.0 1.0))
   (emission
    :accessor emission
    :initarg :emission
    :initform #(1.0 1.0 1.0 1.0))
   (shininess
    :accessor shininess
    :initarg :shininess
    :initform 1.0)))
