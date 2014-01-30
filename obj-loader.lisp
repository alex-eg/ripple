(in-package :obj-loader)

"http://www.fileformat.info/format/wavefrontobj/egff.htm"

(defun load-mesh-from-file (mesh filename)
  (destructuring-bind (raw-verts raw-normals vert-index tex-index normal-index)
      (load-obj-file filename)
    (let ((verts nil)
          (normals nil))
      (dotimes (i (length vert-index))
        (setf verts (cons (nth (nth i vert-index) raw-verts) verts))
        (setf normals (cons (nth (nth i normal-index) raw-normals) normals)))
      (mesh:load-mesh mesh (reverse verts) (reverse normals)))))

(defun load-obj-file (filename)
  (with-open-file (stream filename)
    (raw-load-obj-file stream nil nil nil nil nil)))

(defun raw-load-obj-file (stream verts normals vert-index tex-index normal-index)
  (let ((line (read-line stream nil)))
    (if (null line) (list (reverse verts) (reverse normals) vert-index
                          tex-index normal-index)
        (with-input-from-string (s line)
          (let ((token (make-string 32 :initial-element #\Space)))
            (cl-utilities:read-delimited token s :delimiter #\Space)
            (setf token (subseq token 0 (position #\Space token)))
            (cond ((string= "f" token)
                   (destructuring-bind (vl tl nl) (parse-face (read-line s))
                     (let ((new-vl (append vert-index (reverse vl)))
                           (new-tl (append tex-index (reverse tl)))
                           (new-nl (append normal-index (reverse nl))))
                     (raw-load-obj-file stream verts normals new-vl new-tl new-nl))))
                  ((string= "v" token)
                   (let ((new-verts (cons (parse-vertex (read-line s)) verts)))
                     (raw-load-obj-file stream new-verts
                                        normals vert-index
                                        tex-index normal-index)))
                  ((string= "vn" token)
                   (let ((new-normals (cons (parse-normal (read-line s)) normals)))
                     (raw-load-obj-file stream verts
                                        new-normals vert-index
                                        tex-index normal-index)))
                  (t (raw-load-obj-file stream verts normals vert-index tex-index normal-index))))))))

(defun parse-face (line)
  (labels ((add-to-list (value-string lst)
             (if (string-not-equal "" value-string)
                 (cons (1- (read-from-string value-string)) lst)))
           (parse-point (lists point)
             (mapcar #'add-to-list
                     (cl-utilities:split-sequence #\/ point)
                     lists)))
    (reduce #'parse-point (cl-utilities:split-sequence
                           #\Space
                           line)
            :initial-value (list nil nil nil))))

(defun parse-vertex (line)
  (cdr
   (reduce (lambda (num-vert coord)
             (let ((num (car num-vert))
                   (vert (cdr num-vert)))
               (setf (aref vert num)
                     (read-from-string coord))
               (cons (1+ num) vert)))
           (cl-utilities:split-sequence #\Space line)
           :initial-value
           (cons 0 (make-array '(4) :element-type 'single-float
                               :initial-element 1.0)))))

(defun parse-normal (line)
  (cdr
   (reduce (lambda (num-vert coord)
             (let ((num (car num-vert))
                   (vert (cdr num-vert)))
               (setf (aref vert num)
                     (read-from-string coord))
               (cons (1+ num) vert)))
           (cl-utilities:split-sequence #\Space line)
           :initial-value
           (cons 0 (make-array '(3) :element-type 'single-float)))))
