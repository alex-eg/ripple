(in-package :camera)

(defclass camera ()
  ((eye
    :accessor cam-eye
    :initarg :eye
    :initform #(0.0 0.0 0.0))
   (up-vec
    :accessor cam-up
    :initarg :up
    :initform #(0.0 -1.0 0.0))
   (center
    :accessor cam-center
    :initarg :center
    :initform #(0.0 0.0 1.0))
   (model-view
    :accessor cam-model-view-matrix
    :initarg nil)
   (projection
    :accessor cam-projection-matrix
    :initarg nil)
   (field-of-view-y
    :accessor cam-fovy
    :initarg :fov
    :initform 50.0)
   (aspect-ratio
    :accessor cam-aspect
    :initarg :aspect
    :initform 4/3)
   (z-near-clip
    :accessor cam-znear
    :initarg :z-near
    :initform 0.1)
   (z-far-clip
    :accessor cam-zfar
    :initarg :z-far
    :initform 99.0)))

(defclass flying-camera (camera) nil)
(defclass view-camera (camera)
  ((pivot-point
    :accessor cam-pivot-point
    :initarg :pivot
    :initform #(0.0 0.0 0.0))))

(defun cam-view (cam)
  (let ((eye (cam-eye cam))
        (center (cam-center cam)))
    (v:- center eye)))

(defun print-camera-parameters (cam)
  (let ((c (cam-center cam))
        (e (cam-eye cam))
        (u (cam-up cam))
        (view (cam-view cam)))
    (format t "Eye: ~A~%Center: ~A~%Up: ~A~%View: ~A~%"
            e c u view)))

(defmethod initialize-instance :after ((cam view-camera) &key)
  (setf (cam-pivot-point cam) (cam-center cam))
  (setf (cam-center cam)
        (v:normalize (v:+ (cam-eye cam)
                          (cam-view cam)))))

;; Common method
(defun rotate-roll (cam f)
  (let* ((view (cam-view cam))
         (up (cam-up cam))
         (rot-matrix (m:rotate view (helpers:radians f)))
         (new-up (v:normalize (m:coerce-vector
                               (m:*-mat-mat up rot-matrix)))))
    (setf (cam-up cam) new-up)))

(defgeneric rotate-yaw (cam f))
(defgeneric rotate-pitch (cam f))
(defgeneric move-side (cam dx dy))
(defgeneric move-forward (cam d))

;; Flying camera methods
(defmethod rotate-yaw ((cam flying-camera) (f float))
  (let* ((eye (cam-eye cam))
         (view (v:normalize (cam-view cam)))
         (up (cam-up cam))
         (rot-matrix (m:rotate up (helpers:radians f)))
         (new-center (v:+ eye
                         (m:coerce-vector
                          (m:*-mat-mat
                           view
                           rot-matrix)))))
    (setf (cam-center cam) new-center)))

(defmethod rotate-pitch ((cam flying-camera) (f float))
  (let* ((view (cam-view cam))
         (eye (cam-eye cam))
         (up (cam-up cam))
         (side (v:cross up view))
         (rot-matrix (m:rotate side (helpers:radians f)))
         (new-view (m:coerce-vector
                    (m:*-mat-mat view rot-matrix)))
         (new-center (v:+ eye new-view))
         (new-up (v:normalize (v:cross new-view side))))
    (setf (cam-center cam) new-center)
    (setf (cam-up cam) new-up)))

(defmethod move-forward ((cam flying-camera) (dest float))
  (with-old-parameters (cam :eye eye
                            :center center
                            :view view)
    (let ((new-eye (v:+ view eye))
          (new-center (v:+ center view)))
      (setf (cam-center cam) new-center)
      (setf (cam-eye cam) new-eye))))

(defmethod move-side ((cam flying-camera) (dx float) (dy float))
  (with-old-parameters (cam :eye eye
                            :center center
                            :up up
                            :view view)
    (let* ((dir (vector (v:x view) 0.0 (v:z view)))
           (strafe (v:cross up view))
           (d-dir (v:+ (v:*. dir dy)
                       (v:*. strafe dx)))
           (new-eye (v:+ eye d-dir))
           (new-center (v:+ center d-dir)))
      (setf (camera:cam-eye cam) new-eye)
      (setf (camera:cam-center cam) new-center))))

;; View camera methods
(defmethod rotate-yaw ((cam view-camera) (f float))
  (let* ((view (v:- (cam-pivot-point cam)
                    (cam-eye cam)))
         (up (cam-up cam))
         (rot-matrix (m:rotate up (helpers:radians f)))
         (new-eye (v:- (cam-pivot-point cam)
                       (m:coerce-vector
                        (m:*-mat-mat
                         view
                         rot-matrix)))))
    (setf (cam-eye cam) new-eye)))

(defmethod rotate-pitch ((cam view-camera) (f float))
  (let* ((view (v:- (cam-pivot-point cam)
                    (cam-eye cam)))
         (up (cam-up cam))
         (side (v:normalize (v:cross up view)))
         (rot-matrix (m:rotate side (helpers:radians f)))
         (new-view (m:coerce-vector
                    (m:*-mat-mat view rot-matrix)))
         (new-eye (v:- (cam-pivot-point cam) new-view))
         (new-up (v:normalize (v:cross new-view side))))
    (setf (cam-eye cam) new-eye)
    (setf (cam-up cam) new-up)))

(defmethod move-forward ((cam view-camera) (dir float))
  (with-old-parameters (cam :eye eye
                            :center center
                            :pivot pivot
                            :view view)
    (let* ((d (v:*. (v:normalize (v:- pivot view)) dir))
           (new-eye (v:+ eye d))
           (new-pivot (v:+ pivot d))
           (new-center (v:+ center d)))
      (setf (cam-eye cam) new-eye)
      (setf (cam-pivot-point cam) new-pivot)
      (setf (cam-center cam) new-center))))

(defmethod move-side ((cam view-camera) (dx float) (dy float))
  (with-old-parameters (cam :eye eye
                            :center center
                            :up up
                            :pivot pivot
                            :view view)
    (let* ((dir (vector (v:x view) 0.0 (v:z view)))
           (strafe (v:cross up view))
           (d-dir (v:+ (v:*. dir dy)
                       (v:*. strafe dx)))
           (new-eye (v:+ eye d-dir))
           (new-pivot (v:+ pivot d-dir))
           (new-center (v:+ center d-dir)))
      (setf (cam-eye cam) new-eye)
      (setf (cam-pivot-point cam) new-pivot)
      (setf (cam-center cam) new-center))))

(defun update-matrices (cam)
  (setf (cam-model-view-matrix cam)
        (m:look-at (cam-eye cam)
                   (cam-center cam)
                   (cam-up cam)))
  (setf (cam-projection-matrix cam)
        (m:perspective (cam-fovy cam)
                       (cam-aspect cam)
                       (cam-znear cam)
                       (cam-zfar cam))))

(defmacro with-old-parameters ((cam &key eye center up view pivot) &rest body)
  "Sets environment with defined old-eye, old-center and old-up variables.
It also sets the old-view vector as normalized subtraction of old-center
and old-eye vectors"
  (let ((binding-list
         (remove nil
                 (list `(,eye (camera:cam-eye ,cam))
                       `(,center (camera:cam-center ,cam))
                       `(,up (camera:cam-up ,cam))
                       `(,pivot (camera:cam-pivot-point ,cam))
                       `(,view (camera:cam-view ,cam)))
                 :key #'car)))
    `(let* ,binding-list
       (progn ,@body))))
