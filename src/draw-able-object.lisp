(in-package :genetics)

(defun make-draw-able-object ()
  (let ((%vertex-array-object-id nil)
	(%vertex-buffer-object-id nil)
	(%element-buffer-object-id nil))

    (lambda (message)
      (case message

	((initialize) (lambda (self)
			(declare (ignore self))
			;; TODO: make selectively
			;; VAO
			(if (eq %vertex-array-object-id nil)
			    (setf %vertex-array-object-id
				  (car (gl:gen-vertex-array))))
			
			;; VBO		
			(if (eq %vertex-buffer-object-id nil)
		       	    (setf %vertex-buffer-object-id
				  (car (gl:gen-buffer))))

			;; EBO
			(if (eq %element-buffer-object-id nil)
		       	    (setf %element-buffer-object-id
		       		  (car (gl:gen-buffer))))

			(if (or (> 0 %vertex-array-object-id)
				(> 0 %vertex-buffer-object-id)
				(> 0 %element-buffer-object-id))
			    (error
			     "didn't initialize draw able objects"))))

	((type (lambda (self)
		 (expand-type 'draw-able-object super-class))))

	((is-a) (lambda (self type)
		  (member type (ask self 'type))))
	
	((get-vao) (lambda (self)
		     (declare (ignore self))
		     %vertex-array-object-id))

	((get-vbo) (lambda (self)
		     (declare (ignore self))
		     %vertex-buffer-object-id))

	((get-ebo) (lambda (self)
		     (declare (ignore self))
		     %element-buffer-object-id))
	 
	((destroy) (lambda (self)
		      (declare (ignore self))
		      (if (not (eq %element-buffer-object-id nil))
			  (gl:delete-buffers %element-buffer-object-id))
		      (if (not (eq %vertex-buffer-object-id nil))	
			  (gl:delete-buffers %vertex-buffer-object-id))
		      (if (not (eq %vertex-array-object-id nil))	
			  (gl:delete-vertex-arrays
			   %vertex-array-object-id))))))))
