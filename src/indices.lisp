(in-package :genetics)

(defun generate-rect-indices (x l)
  (if (<= l (+ x 2))
		   nil
		   (append (list x (+ x 1) (+ x 2)
				 (+ x 2) (+ x 1) (+ x 3))
			   (generate-rect-indices (+ x 2) l))))

(defun generate-cube-indices (x l)
  (let ((insufficient-indices (generate-rect-indices x l)))
		 (append insufficient-indices
			 (list 6 7 0 0 7 1
			       1 7 3 3 7 5
			       0 6 2 2 6 4))))

(defun make-indices ()
  (let ((%super-class (make-root))
	(%fn nil)
	(%indices nil))

    (lambda (message)
      (case message

	((set-function) (lambda (self fn)
			       (declare (ignore self))
			       (setf %fn fn)))

	((get-indices) (lambda (self &rest args)
			 (declare (ignore self))
			 (if %indices
			     %indices
			     (setf %indices (apply %fn args)))
			 %indices))

	((type) (lambda (self)
		  (declare (ignore self))
		  (extend-type 'indices %super-class)))

	((is-a) (lambda (self type)
		  (member type (ask self 'type))))

	(t (get-method message %super-class))))))



(defun make-vertex (&key (point nil) (attrs nil))
  (flet ((%%verify (arg type)
	   (cond ((eq type 'point)
		  (assert (or (null arg) (ask arg 'is-a 'vector)))
		  arg)
		 ((eq type 'attribute)
		  (assert (or (null arg) (and (consp arg)
					      (consp (car arg))
					      (symbolp (caar arg)))))
		  arg)
		 (t (error "failed to verify vertex")))))
    (let ((%super-class (make-root))
	  ;; TODO: naming :L
	  (%point (%%verify point 'point))
	  (%attrs (%%verify attrs 'attribute)))

      (lambda (message)
	(case message

	  ((type) (lambda (self)
		    (declare (ignore self))
		    (extend-type 'vertex %super-class)))

	  ((is-a) (lambda (self type)
		    (member type (ask self 'type))))

	  (t (get-method message %super-class)))))))
