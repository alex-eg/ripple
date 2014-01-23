(in-package :obj-loader)

"http://www.fileformat.info/format/wavefrontobj/egff.htm"

(defun load-mesh (mesh filename)
  (destructuring-bind (verts normals indices)
      (load-obj-file filename)
    (format t "~A ~A ~A~%" verts normals indices)))
;    (mesh:load-mesh mesh verts normals)))

(defun load-obj-file (filename)
  (let ((verts nil)
        (normals nil)
        (vert-index nil)
        (normal-index nil))
    (with-open-file (stream filename)
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (with-input-from-string (s line)
          (let ((token (helpers:delimited-read s #\Space)))
            (cond ((string= "f" token)
                   (parse-face (read-line s) vert-index normal-index))
                  ((string= "v" token)
                   (parse-vertex (read-line s) verts))
                  ((string= "vn" token)
                   (parse-normal (read-line s) normals))
                  (t (read-line s nil))))))))
  '(wo lo lo))

(defun parse-face (line normals verts)
  (format t "FACE ~A~%" line))

(defun parse-vertex (line verts)
  (format t "VERT ~A~%" line))

(defun parse-normal (line normals)
  (format t "NORMAL ~A~%" line))
