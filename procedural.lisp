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
                  zs))
         (normals
          (append 
           (loop for i from 0.5 to 5.5
              with a = (/ PI 3.0)
              collect (cos (* a i)) into xs
              collect (sin (* a i)) into ys
              finally
                (return
                  (mapcar (lambda (x y)
                            (make-array
                             3
                             :element-type 'single-float
                             :initial-contents (list
                                                (coerce x 'single-float)
                                                (coerce y 'single-float)
                                                0.0)))
                          xs ys)))
           (list 
            #(0.0 0.0 1.0)
            #(0.0 0.0 -1.0)))))

    `(list (list ,@verts) (list ,@normals)
           '(0 1 2 0 2 3 0 3 4 0 4 5 0 5 6 0 6 1)
           '(6 6 6 6 6 6))))

(defmacro coordinate-frame (size)
  (let ((x (make-array 
            4
            :initial-contents '(1.0 0.0 0.0 1.0)))
        (y (make-array
            4 
            :initial-contents '(0.0 1.0 0.0 1.0)))
        (z (make-array
            4
            :initial-contents '(0.0 0.0 1.0 1.0))))))
    
                      
