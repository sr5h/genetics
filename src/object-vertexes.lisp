;;;; object-vertexes.lisp

(in-package :genetics)

(defun make-object-vertexes ()
  (let ((%super-class (make-root))

	(%vxs nil)
	(%fns nil)
	(%attns '(3)))

    (lambda (message)
      (case message

	((assemblef-vertexes)
	 (lambda (self pts)
	   (declare (ignore self))
	   (labels ((iter (points num-of-points-vertex vertex acc)
		      (cond ((null points) (values acc vertex))
			    ((= 3 num-of-points-vertex)
			     (let ((attrs (mapcan #'(lambda (fn) (funcall fn vertex))
						  %fns)))
			       (iter points 0 nil (append acc attrs))))
			    (t (iter (cdr points)
				     (+ 1 num-of-points-vertex)
				     (append vertex (list (car points)))
				     (append acc (list (car points))))))))
	     (multiple-value-bind (vertexes last-vertex) (iter pts 0 nil nil)
	       (setf %vxs
		     (append vertexes
			     (mapcan #'(lambda (fn)
					 (let ((attr (funcall fn last-vertex)))
					   (setf %attns
						 (append %attns
							 (list (length attr))))
					   attr))
				     %fns)))))))

	((addf-fns) (lambda (self &rest fns)
		      (declare (ignore self))
		      (mapc #'(lambda (fn)
				(setf %fns
				      (append %fns (list fn))))
			    fns)))

	((get-vertexes) (lambda (self)
			  (declare (ignore self))
			  %vxs))

	((get-attributes) (lambda (self)
			    (declare (ignore self))
			    %attns))

	((type) (lambda (self)
		  (declare (ignore self))
		  (extend-type 'object-attribute %super-class)))

	((is-a) (lambda (self type)
		  (member type (ask self 'type))))

	(t (get-method message %super-class))))))
