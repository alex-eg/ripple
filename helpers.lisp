(in-package :helpers)

(defun radians (a)
  "Converts degrees to radians"
  (* pi (/ a 180.0)))

(defun make-keyword (name)
  (values (intern (string-upcase name) "KEYWORD")))

(defmacro make-regular-symbol (symbol-name package-name)
  `(values (intern (string-upcase ,symbol-name) ,package-name)))

(defmacro delimited-read (raw-stream delimiter)
  (let ((char (gensym))
        (token (gensym))
        (stream (or (and (atom raw-stream) raw-stream)
                    (eval raw-stream))))
    `(with-output-to-string (,token)
       (do ((,char (read-char ,stream nil :eof)
                   (read-char ,stream nil :eof)))
           ((or (eql ,char :eof)
                (char= ,char ,delimiter)))
         (princ ,char ,token)))))
       
       
