(in-package :genetics)

(defun make-indices ()
  (let ((%super-class (make-root))
	(%fn nil)
	(%indices nil))

    (labels ((generate-rect-indices (x l)
	       (if (<= l (+ x 2))
		   nil
		   (append (list x (+ x 1) (+ x 2)  
				 (+ x 2) (+ x 1) (+ x 3)) 
			   (generate-rect-indices (+ x 2) l)))))

      (lambda (message)
	(case message

	  ((type) (lambda (self)
		    (declare (ignore self))
		    (extend-type 'indices %super-class)))
	  
	  ((is-a) (lambda (self type)
		    (member type (ask self 'type))))

	  ((set-rect-function) (lambda (self)
					 (declare (ignore self))
					 (setf %fn #'generate-rect-indices)))

	  ((get-indices) (lambda (self &rest args)
			   (declare (ignore self))
			   (if %indices
			       %indices
			       (setf %indices (apply %fn args)))
			       
			   %indices))
	  
	  (t (get-method message %super-class)))))))
