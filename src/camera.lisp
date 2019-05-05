(in-package :genetics)

(defun make-camera ()
  (let ((%super-class (make-root))

	(%speed	 0.5)
	(%pos	 (make-vector 0.0 0.0 10.0))
	(%front	 (make-vector 0.0 0.0 -1.0))
	(%up	 (make-vector 0.0 1.0 0.0))
	(%yaw	 0.0)
	(%pitch	 0.0))

    (lambda (message)
      (case message

	((get-speed) (lambda (self)
		      (declare (ignore self))
		      %speed))

	((setf-speed) (lambda (self speed)
			(declare (ignore self))
			(setf %speed speed)))
	
	((get-pos) (lambda (self)
		      (declare (ignore self))
		      %pos))
	
	((setf-pos) (lambda (self v)
		      (declare (ignore self))
		      (setf %pos v)))

	((get-front) (lambda (self)
		      (declare (ignore self))
		      %front))
	
	((setf-front) (lambda (self v)
		      (declare (ignore self))
		      (setf %front v)))

	((get-up) (lambda (self)
		      (declare (ignore self))
		      %up))
	
	((setf-up) (lambda (self v)
		      (declare (ignore self))
		      (setf %up v)))

	((get-yaw) (lambda (self)
		      (declare (ignore self))
		      %yaw))
	
	((setf-yaw) (lambda (self p)
		      (declare (ignore self))
		      (setf %yaw p)))

	((get-pitch) (lambda (self)
		      (declare (ignore self))
		      %pitch))
	
	((setf-pitch) (lambda (self p)
		      (declare (ignore self))
		      (setf %pitch p)))

	((setf-camera) (lambda (self s p f u y pit)
			 (setf %speed s
			       %pos p
			       %front f
			       %up u
			       %yaw y
			       %pitch pit)))

	((get-camera) (lambda (self)
			(list %speed %pos %front %up %yaw %pitch)))
	
	((look-at) (lambda (self)
		     (declare (ignore self))
		     (look-at (make-matrix)
			      %pos
			      (vec+ %pos %front)
			      %up)))
	
	((type) (lambda (self)
		  (declare (ignore self))	
		  (extend-type 'camera %super-class)))
	
	((is-a) (lambda (self type)
		  (member type (ask self 'type))))
	
	(t (get-method message %super-class))))))
