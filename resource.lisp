(in-package :ripple)

(defclass resource ()
  ((name
    :accessor resource-name
    :initarg :name
    :initform :unnamed)))
