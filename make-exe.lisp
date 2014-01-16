(format t "Current path is ~a, hope compiling will go smoothly~%" *default-pathname-defaults*)
(let ((asdf:*central-registry* (list *default-pathname-defaults*)))
  (format t "Loading system~%")
  (asdf:load-system :ripple))

(defun make-exe ()
  (sb-ext:save-lisp-and-die "ripple-core" 
                            :toplevel #'ripple::main-loop
                            :executable t))
