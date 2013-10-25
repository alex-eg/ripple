(in-package :resource)

(defclass resource ()
  ((name
    :accessor resource-name
    :initarg :name
    :initform :unnamed)))
