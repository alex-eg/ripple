(in-package :v)

(defun x (v)
  (aref v 0))

(defun y (v)
  (aref v 1))

(defun z (v)
  (aref v 2))

(defun + (v1 v2)
  "Sums two vectors"
  (vector
   (cl:+ (x v1) (x v2))
   (cl:+ (y v1) (y v2))
   (cl:+ (z v1) (z v2))))

(defun - (v1 v2)
  "Subtracts one vector from another"
  (vector
   (cl:- (x v1) (x v2))
   (cl:- (y v1) (y v2))
   (cl:- (z v1) (z v2))))

(defun *. (vect num)
  (vector (* num (x vect))
          (* num (y vect))
          (* num (z vect))))

(defun mag (vec)
  "Returns vector magnitude"
  (sqrt
   (reduce (lambda (c x)
             (cl:+ c (* x x)))
           vec
           :initial-value 0.0)))

(defun normalize (vec)
  "Returns normalized vector"
  (let ((len (mag vec)))
    (if (= len 0.0)
        (make-array (length vec) :initial-element 0.0)
        (map 'vector
             (lambda (x)
               (/ x len))
             vec))))

(defun cross (a b)
  "Computes cross product of two vectors"
  (let ((a-x (x a))
        (a-y (y a))
        (a-z (z a))

        (b-x (x b))
        (b-y (y b))
        (b-z (z b)))
    (make-array 3
                :initial-contents
                (list (cl:- (* a-y b-z) (* b-y a-z))
                      (cl:- (* a-z b-x) (* b-z a-x))
                      (cl:- (* a-x b-y) (* b-x a-y))))))
