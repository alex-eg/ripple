(in-package :ripple)

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

(defmacro with-old-parameters ((cam &key eye center up view pivot) &rest body)
  "Sets environment with defined old-eye, old-center and old-up variables.
It also sets the old-view vector as normalized subtraction of old-center
and old-eye vectors"
  (let ((binding-list
         (remove nil
                 (list `(,eye (cam-eye ,cam))
                       `(,center (cam-center ,cam))
                       `(,up (cam-up ,cam))
                       `(,pivot (cam-pivot-point ,cam))
                       `(,view (cam-view ,cam)))
                 :key #'car)))
    `(let* ,binding-list
       (progn ,@body))))

(defun cam-view (cam)
  (let ((eye (cam-eye cam))
        (center (cam-center cam)))
    (v- center eye)))

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
        (normalize (v+ (cam-eye cam)
                          (cam-view cam)))))

;; Common method
(defun rotate-roll (cam f)
  (let* ((view (cam-view cam))
         (up (cam-up cam))
         (rot-matrix (rotate view (radians f)))
         (new-up (normalize (coerce-vector
                               (*-mat-mat up rot-matrix)))))
    (setf (cam-up cam) new-up)))

(defgeneric rotate-yaw (cam f))
(defgeneric rotate-pitch (cam f))
(defgeneric move-forward (cam d))
(defgeneric move-side (cam dx dy))
(defgeneric move-vertical (cam dx dz))

;; Flying camera methods
(defmethod rotate-yaw ((cam flying-camera) (f float))
  (let* ((eye (cam-eye cam))
         (view (normalize (cam-view cam)))
         (up (cam-up cam))
         (rot-matrix (rotate up (radians f)))
         (new-center (v+ eye
                         (coerce-vector
                          (*-mat-mat
                           view
                           rot-matrix)))))
    (setf (cam-center cam) new-center)))

(defmethod rotate-pitch ((cam flying-camera) (f float))
  (let* ((view (cam-view cam))
         (eye (cam-eye cam))
         (up (cam-up cam))
         (side (cross up view))
         (rot-matrix (rotate side (radians f)))
         (new-view (coerce-vector
                    (*-mat-mat view rot-matrix)))
         (new-center (v+ eye new-view))
         (new-up (normalize (cross new-view side))))
    (setf (cam-center cam) new-center)
    (setf (cam-up cam) new-up)))

(defmethod move-forward ((cam flying-camera) (dest float))
  (with-old-parameters (cam :eye eye
                            :center center
                            :view view)
    (let ((new-eye (v+ view eye))
          (new-center (v+ center view)))
      (setf (cam-center cam) new-center)
      (setf (cam-eye cam) new-eye))))

(defmethod move-side ((cam flying-camera) (dx float) (dy float))
  (with-old-parameters (cam :eye eye
                            :center center
                            :up up
                            :view view)
    (let* ((dir (vector (x view) 0.0 (z view)))
           (strafe (cross up view))
           (d-dir (v+ (*. dir dy)
                       (*. strafe dx)))
           (new-eye (v+ eye d-dir))
           (new-center (v+ center d-dir)))
      (setf (cam-eye cam) new-eye)
      (setf (cam-center cam) new-center))))

(defmethod move-vertical ((cam flying-camera) (dx float) (dz float))
  (with-old-parameters (cam :eye eye
                            :center center
                            :up up
                            :view view)
    (let* ((strafe (cross up view))
           (d-dir (v+ (*. up dz)
                       (*. strafe dx)))
           (new-eye (v+ eye d-dir))
           (new-center (v+ center d-dir)))
      (setf (cam-eye cam) new-eye)
      (setf (cam-center cam) new-center))))

;; View camera methods
(defmethod rotate-yaw ((cam view-camera) (f float))
  (let* ((view (v- (cam-pivot-point cam)
                    (cam-eye cam)))
         (up (cam-up cam))
         (rot-matrix (rotate up (radians f)))
         (new-eye (v- (cam-pivot-point cam)
                       (coerce-vector
                        (*-mat-mat
                         view
                         rot-matrix)))))
    (setf (cam-eye cam) new-eye)))

(defmethod rotate-pitch ((cam view-camera) (f float))
  (let* ((view (v- (cam-pivot-point cam)
                    (cam-eye cam)))
         (up (cam-up cam))
         (side (normalize (cross up view)))
         (rot-matrix (rotate side (radians f)))
         (new-view (coerce-vector
                    (*-mat-mat view rot-matrix)))
         (new-eye (v- (cam-pivot-point cam) new-view))
         (new-up (normalize (cross new-view side))))
    (setf (cam-eye cam) new-eye)
    (setf (cam-up cam) new-up)))

(defmethod move-forward ((cam view-camera) (dir float))
  (with-old-parameters (cam :eye eye
                            :center center
                            :pivot pivot
                            :view view)
    (let* ((d (*. (normalize (v- pivot view)) dir))
           (new-eye (v+ eye d))
           (new-pivot (v+ pivot d))
           (new-center (v+ center d)))
      (setf (cam-eye cam) new-eye)
      (setf (cam-pivot-point cam) new-pivot)
      (setf (cam-center cam) new-center))))

(defmethod move-side ((cam view-camera) (dx float) (dy float))
  (with-old-parameters (cam :eye eye
                            :center center
                            :up up
                            :pivot pivot
                            :view view)
    (let* ((dir (vector (x view) (y view) 0.0))
           (strafe (cross up view))
           (d-dir (v+ (*. dir dy)
                       (*. strafe dx)))
           (new-eye (v+ eye d-dir))
           (new-pivot (v+ pivot d-dir))
           (new-center (v+ center d-dir)))
      (setf (cam-eye cam) new-eye)
      (setf (cam-pivot-point cam) new-pivot)
      (setf (cam-center cam) new-center))))

(defmethod move-vertical ((cam view-camera) (dx float) (dz float))
  (with-old-parameters (cam :eye eye
                            :center center
                            :up up
                            :pivot pivot
                            :view view)
    (let* ((strafe (cross up view))
           (d-dir (v+ (*. 
                        (make-vector :x 0.0 :y 0.0 :z 1.0)
                        dz)
                       (*. strafe dx)))
           (new-eye (v+ eye d-dir))
           (new-pivot (v+ pivot d-dir))
           (new-center (v+ center d-dir)))
      (setf (cam-eye cam) new-eye)
      (setf (cam-pivot-point cam) new-pivot)
      (setf (cam-center cam) new-center))))

(defun update-matrices (cam)
  (setf (cam-model-view-matrix cam)
        (look-at (cam-eye cam)
                   (cam-center cam)
                   (cam-up cam)))
  (setf (cam-projection-matrix cam)
        (perspective (cam-fovy cam)
                       (cam-aspect cam)
                       (cam-znear cam)
                       (cam-zfar cam))))
