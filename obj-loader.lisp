(in-package :obj-loader)

"http://www.fileformat.info/format/wavefrontobj/egff.htm"

(defun load-mesh (mesh filename)
  (destructuring-bind (verts normals indices)
      (load-obj-file filename))
  (mesh:load-mesh mesh verts normals))

(defun load-obj-file (filename)
  (let ((verts nil)
        (normals nil))
    (with-open-file (stream filename)
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (with-input-from-string (s line)
          (do ((token (read s nil :eof)
                      (read s nil :eof)))
              ((eql token :eof))
            (cond ((string= "f" token)
                   (parse-face (read-line s) normals verts))
                  ((string= "v" token)
                   (parse-vertex (read-line s) verts))
                  ((string= "vn" token)
                   (parse-normal (read-line s) normals)
                  
      
