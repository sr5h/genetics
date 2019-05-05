;;;; manage-input.lisp

(in-package :genetics)

(defun make-manage-input ()
  (let ((%super-class (make-root))

	(%mouse-sensitivity 0.05)
	(%mouse-pitch-sensitivity 5.0)

	(%mouse-x nil)
	(%mouse-y nil)
	(%mouse-state nil))

    (lambda (message)
      (case message

	;; TODO:
	((get-view-by-key) (lambda (self camera keyword)
		 (declare (ignore self))
		 (let* ((camera-setting (ask camera 'get-camera))
			(speed (pop camera-setting))
			(camera-pos (pop camera-setting))
			(front (pop camera-setting))
			(up (pop camera-setting))
			(yaw (pop camera-setting))
			(pitch (pop camera-setting)))
		   
		   (case keyword			;TODO:
		     ((:scancode-w)
		      (setf camera-pos (vec+ camera-pos (vec* front speed))))
		     ((:scancode-s)
		      (setf camera-pos (vec- camera-pos (vec* front speed))))
		     ((:scancode-a)
		      (setf camera-pos (vec- camera-pos
					     (vec* (normalize (cross front up)) speed))))
		     ((:scancode-d)
		      (setf camera-pos (vec+ camera-pos
					     (vec* (normalize (cross front up)) speed)))))

		   (ask camera 'setf-camera speed camera-pos front up yaw pitch))))
	
	;; TODO: pitch is strange!
	((get-view-by-mouse)
	 (lambda (self camera state x y xr yr)
	   (declare (ignore self))
	   (let* ((camera-setting (ask camera 'get-camera))
		  (speed (pop camera-setting))
		  (camera-pos (pop camera-setting))
		  (front (pop camera-setting))
		  (up (pop camera-setting))
		  (yaw (pop camera-setting))
		  (pitch (pop camera-setting)))
	     (let ((offset-x (* %mouse-sensitivity xr))
		   (offset-y yr))
	       (setf %mouse-x x %mouse-y y %mouse-state state
		     yaw (+ yaw offset-x) pitch (+ pitch offset-y))

	       (if (> pitch 89.0)
		   (setf pitch 89.0))
	       (if (< pitch -89.0)
		   (setf pitch -89.0))

	       (setf front
		     (normalize (make-vector (coerce (* (cos (rad yaw))
							(cos (rad pitch)))
						     'single-float)
					     (coerce (sin (rad pitch)) 'single-float)
					     (coerce (* (sin (rad yaw))
							(cos (rad pitch)))
						     'single-float))))
	       (format t "front : ~a~%" (ask front 'to-list))
	       (ask camera 'setf-camera speed camera-pos front up yaw pitch)))))

	;; ((idle) (lambda (self)
	;; 	  (declare (ignore self))
	;; 	  (look-at (make-matrix)
	;; 		  %camera-pos (vec+ %camera-pos %target-pos) %up-vector)))
	
	((type) (lambda (self)
		  (declare (ignore self))	
		  (extend-type 'manage-input %super-class)))

	((is-a) (lambda (self type)
		  (member type (ask self 'type))))
	
	(t (get-method message %super-class))))))
