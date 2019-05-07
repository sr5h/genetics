;;;; camera.lisp

(in-package :genetics)

(defun make-camera ()
  (let* ((%super-class (make-root))

	 (%speed 0.5)
	 (%pos (make-vector 0.0 0.0 10.0))
	 (%t-pos (make-vector 0.0 0.0 0.0))
	 (%world-up (make-vector 0.0 1.0 0.0))
	 ;; opposite direction of camera direction
	 ;; %front is %t-pos - %pos
	 (%front (normalize (vec- %t-pos %pos)))
	 (%right (normalize (cross %front %world-up)))
	 (%up (normalize (cross %right %front)))


	 (%yaw -90.0)
	 (%pitch 0.0))

    (labels ((update-camera-vector ()
	       (setf %front (normalize (make-vector (coerce (* (cos (rad %yaw))
							       (cos (rad %pitch)))
							    'single-float)
						    (coerce (sin (rad %pitch))
							    'single-float)
						    (coerce (* (sin (rad %yaw))
							       (cos (rad %pitch)))
							    'single-float)))
		     %right (normalize (cross %front %world-up))
		     %up (normalize (cross %right %front)))))

    (lambda (message)
      (case message

	((update-by-key-up) (lambda (self keyword)
			      (declare (ignore self))
			      (case keyword			;TODO:
				((:scancode-w)
				 (setf %pos (vec+ %pos (vec* %front %speed))))
				((:scancode-s)
				 (setf %pos (vec- %pos (vec* %front %speed))))
				((:scancode-a)
				 (setf %pos (vec- %pos
						  (vec* (normalize (cross %front %up))
							%speed))))
				((:scancode-d)
				 (setf %pos (vec+ %pos
						  (vec* (normalize (cross %front %up))
							%speed)))))
			      (format t
				     "~a ~a ~a~%"
				     (ask %pos 'to-list)
				     (ask %front 'to-list)
				     (ask (vec+ %pos %front) 'to-list))))

	((update-by-mouse) (lambda (self offset-x offset-y)
			     (declare (ignore self))
			     (setf %yaw (+ %yaw offset-x)
				   %pitch (+ %pitch offset-y))

			     (if (> %pitch 89.0)
				 (setf %pitch 89.0))
			     (if (< %pitch -89.0)
				 (setf %pitch -89.0))

			     (update-camera-vector)

			     (format t
				     "~a ~a ~a~%"
				     (ask %pos 'to-list)
				     (ask %front 'to-list)
				     (ask (vec+ %pos %front) 'to-list)
				     )))

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

    ((get-%yaw) (lambda (self)
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
		     (declare (ignore self))
		     (setf %speed s
			   %pos p
			   %front f
			   %up u
			   %yaw y
			   %pitch pit)))

    ((get-camera) (lambda (self)
		    (declare (ignore self))
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

    (t (get-method message %super-class)))))))
