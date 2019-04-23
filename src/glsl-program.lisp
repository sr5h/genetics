(in-package :genetics)

(defun make-glsl-program ()
  (let ((%vertex-shader-id nil)
	(%fragment-shader-id nil)
	(%program-id nil)
	;; (%index-attribute 0)
	)
    (labels ((%read-sequence-from-file (file-path)
	       (with-open-file (file-stream file-path :direction :input)
		 (let ((contents (make-string (file-length file-stream))))
		   (read-sequence contents file-stream)
		   contents)))
	     (%set-uniform-matrix-4fv (variable-name lst)
	       (print lst)
	       (let* ((loc (gl:get-uniform-location
			    %program-id
			    variable-name)))
		 (%gl:uniform-matrix-4fv loc
					 1
					 :false (cffi:foreign-alloc
						 :float
						 :initial-contents lst))))
	     )
	     
      (lambda (message)
	(case message
	  ((compile) (lambda (self vertex-shader-path fragment-shader-path)
		       (let ((v-str (%read-sequence-from-file
				     vertex-shader-path))
			     (f-str (%read-sequence-from-file
				     fragment-shader-path)))
			 (setf %vertex-shader-id
			       (gl:create-shader :vertex-shader))
			 (setf %fragment-shader-id
			       (gl:create-shader :fragment-shader))
		 
			 ;; compile vertex shader
			 (gl:shader-source %vertex-shader-id v-str)
			 (gl:compile-shader %vertex-shader-id)

			 ;; verify vertex shader
			 (if (gl:get-shader %vertex-shader-id
					    :compile-status)
			     (format t "~a compilation successful~%"
				     :vertex-shader)
			     (format t "Compilation failure in ~a:~% ~a~%"
				     :vertex-shader
				     (gl:get-shader-info-log
				      %vertex-shader-id)))
			 ;; pass
			 
			 ;; compile fragment shader
			 (gl:shader-source %fragment-shader-id f-str)
			 (gl:compile-shader %fragment-shader-id)

			 (setf %program-id (gl:create-program)))))
	  
	  ((link) (lambda (self)
		    (gl:attach-shader %program-id %vertex-shader-id)
		    (gl:attach-shader %program-id %fragment-shader-id)
		    
		    (gl:link-program %program-id)

		    (gl:detach-shader %program-id %vertex-shader-id)
		    (gl:detach-shader %program-id %fragment-shader-id)

		    (gl:delete-shader %vertex-shader-id)
		    (gl:delete-shader %Fragment-Shader-id)))

	  ((use) (lambda (self)
		   (gl:use-program %program-id)

		   (%set-uniform-matrix-4fv
		    "model"
		    (to-list
		     (rotate (make-matrix) (sdl2:get-ticks) 1.0 0.0 0.0)))
		   
		   (%set-uniform-matrix-4fv
		    "view"
		    (to-list
		     (translate (make-matrix) 0.0 0.0 -5.0)))
		   
		   (%set-uniform-matrix-4fv
		    "projection"
		    (to-list
		     (perspective (make-matrix)
				  45.0
				  (/ 800.0 600.0)
				  0.1
				  100.0)))))
	  
	  ((unuse) (lambda (self)
		     (gl:use-program 0)))

	  ((destroy) (lambda (self)
		       (if (/= %program-id 0)
			   (gl:delete-program %program-id)))))))))
