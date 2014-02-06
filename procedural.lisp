(in-package :procedural)

"Special package for macros for generating normal and vertex data"

(defmacro hexagonal-grid (w h height radius)
  "w and h are dimensions of the grid, height and radius - of each individual hexagon"
  w h ;; unused so far
  (let* ((xys
          (loop for i from 0 to 5
             with a = (/ PI 3.0)
             collect (* (cos (* a i)) radius) into xs
             collect (* (sin (* a i)) radius) into ys
             finally
               (return
                 (cons
                  (cons 0.0 xs) ;; 0.0 for central point
                  (cons 0.0 ys)))))
         (zs
          (loop for i from 0 to 6
             collect 0.0 into zs0
             collect height into zsh
             finally (return (append zs0 zsh))))
         (verts
          (mapcar (lambda (x y z)
                    (make-array
                     4
                     :element-type 'single-float
                     :initial-contents (list
                                        (coerce x 'single-float)
                                        (coerce y 'single-float)
                                        z
                                        1.0)))
                  (append (car xys) (car xys))
                  (append (cdr xys) (cdr xys))
                  zs)))
    `(list ,@verts)))
