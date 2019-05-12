;;;; camera.lisp

(in-package :genetics)

(define-class camera (root)
  ((%speed 0.5)
   (%pos (make-vector 0.0 0.0 40.0))
   (%t-pos (make-vector 0.0 0.0 0.0))
   (%world-up (make-vector 0.0 1.0 0.0))
   ;; opposite direction of camera direction
   ;; %front is %t-pos - %pos
   (%front (normalize (vec- %t-pos %pos)))
   (%right (normalize (cross %front %world-up)))
   (%up (normalize (cross %right %front)))

   (%yaw -90.0)
   (%pitch 0.0))
  ((update-camera-vector ()
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

  ((update-by-key-up) (lambda (self keyword)
			(declare (ignore self))
			(case keyword			;TODO:
			  ((:scancode-w)
			   (setf %pos (vec+ %pos (vec*s %front %speed))))
			  ((:scancode-s)
			   (setf %pos (vec- %pos (vec*s %front %speed))))
			  ((:scancode-a)
			   (setf %pos (vec- %pos
					    (vec*s (normalize (cross %front %up))
						   %speed))))
			  ((:scancode-d)
			   (setf %pos (vec+ %pos
					    (vec*s (normalize (cross %front %up))
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

  ((look-at) (lambda (self)
	       (declare (ignore self))
	       (look-at (make-matrix)
			%pos
			(vec+ %pos %front)
			%up))))
