;;;; manage-input.lisp

(in-package :genetics)

(defun make-manage-input ()
  (let ((%super-class (make-root))

	(%camera-speed 0.5)
	(%mouse-sensitivity 0.05)
	(%mouse-pitch-sensitivity 5.0)
	(%camera-pos (make-vector 0.0 0.0 10.0))
	(%target-pos (make-vector 0.0 0.0 -2.0)) ; TODO: change name. This is not position
	(%up-vector (make-vector 0.0 1.0 0.0))

	(%yaw 0.0)
	(%pitch 0.0)

	(%mouse-x nil)
	(%mouse-y nil)
	(%mouse-state nil))

    (lambda (message)
      (case message

	((initialize) (lambda (self)
			(declare (ignore self))
	                (ask %super-class 'initialize)))
	
	;; TODO:
	((get-view-by-key) (lambda (self keyword)
		 (declare (ignore self))
		 (case keyword			;TODO:
		   ((:scancode-w)
		    (setf %camera-pos (vec+ %camera-pos (vec* %target-pos %camera-speed))))
		   ((:scancode-s)
		    (setf %camera-pos (vec- %camera-pos (vec* %target-pos %camera-speed))))
		   ((:scancode-a)
		    (setf %camera-pos (vec- %camera-pos
					    (vec* (normalize (cross %target-pos %up-vector))
						  %camera-speed))))
		   ((:scancode-d)
		    (setf %camera-pos (vec+ %camera-pos
					    (vec* (normalize (cross %target-pos %up-vector))
						  %camera-speed)))))
		 (look-at (make-matrix)
			  %camera-pos (vec+ %camera-pos %target-pos) %up-vector)))
	
	;; TODO: pitch is strange!
	((get-view-by-mouse) (lambda (self state x y xr yr)
		   (declare (ignore self))
		   (let ((offset-x (* %mouse-sensitivity xr))
			 (offset-y yr))
		     (setf %mouse-x x %mouse-y y %mouse-state state
			   %yaw (+ %yaw offset-x) %pitch (+ %pitch offset-y))

		     (if (> %pitch 89.0)
			 (setf %pitch 89.0))
		     (if (< %pitch -89.0)
			 (setf %pitch -89.0))

		     (setf %target-pos
			   (normalize (make-vector (coerce (* (cos (rad %yaw))
							      (cos (rad %pitch)))
							   'single-float)
						   (coerce (sin (rad %pitch)) 'single-float)
						   (coerce (* (sin (rad %yaw))
							      (cos (rad %pitch)))
							   'single-float)))))
		   (look-at (make-matrix)
			  %camera-pos (vec+ %camera-pos %target-pos) %up-vector)))

	((idle) (lambda (self)
		  (declare (ignore self))
		  (look-at (make-matrix)
			  %camera-pos (vec+ %camera-pos %target-pos) %up-vector)))
	
	((type) (lambda (self)
		  (declare (ignore self))	
		  (extend-type 'manage-input %super-class)))

	((is-a) (lambda (self type)
		  (member type (ask self 'type))))
	
	((destroy) (lambda (self)
                     (declare (ignore self))
		     (ask %super-class 'destroy)))

	(t (get-method message %super-class))))))
