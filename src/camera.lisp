(in-package :genetics)

(defun make-camera ()
  (let ((%pos nil)
	(%target nil)
	(%direction nil)
	(%world-up nil)
	(%right nil)
	(%up nil)
	(%look-at nil))
    
    (lambda (message)
      (case message
	
	((initializef) (lambda (self)
			 (setf %pos (make-vector 0.0 0.0 3.0))
			 (setf %target (make-vector 0.0 0.0 0.0))
			 (setf %direction (normalize
					   (vec- %pos %target))) ;TODO:
			 (setf %world-up (make-vector 0.0 1.0 0.0))
			 (setf %right (normalize
				       (ask %world-up 'cross %direction)))
			 (setf %up (ask %direction 'cross %right))
			 ;; TODO: to-list naming 
			 (let* ((r (append (to-list %right) 0.0))
				(u (append (to-list %up) 0.0))
				(d (append (to-list %direction) 0.0))
				(n (list 0.0 0.0 0.0 1.0))
				(larr (append (list r)
					      (list u)
					      (list d)
					      (list n)))
				(type 'single-float))
			   (setf %look-at
				 (mat* (make-matrix
					:array (make-array '(4 4)
						:initial-contents larr))
				       (translate (set-identity (make-matrix))
						  (coerce (* -1.0
							     (car %pos))
							  type)
						  (coerce (* -1.0
							     (cadr %pos))
							  type)
						  (coerce (* -1.0
							     (caddr %pos))
							  type)))))))))))

