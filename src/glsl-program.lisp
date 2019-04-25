(in-package :genetics)

(defun set-uniform-4fv (glsl-id uniform-name lst)
  (ask glsl-id 'set-uniform-4fv uniform-name lst))

(defun set-uniform-3f (glsl-id uniform-name v0 v1 v2)
  (ask glsl-id 'set-uniform-3f uniform-name v0 v1 v2))

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

	  ((set-uniform-4fv) (lambda (self variable-name lst)
			       (let* ((loc (gl:get-uniform-location
					    %program-id
					    variable-name)))
				 (%gl:uniform-matrix-4fv
				  loc
				  1
				  :false (cffi:foreign-alloc
					  :float
					  :initial-contents lst)))))

	  ((set-uniform-3f) (lambda (self variable-name v0 v1 v2)
			       (let* ((loc (gl:get-uniform-location
					    %program-id
					    variable-name)))
				 (%gl:uniform-3f 
				  loc
				  v0 v1 v2))))
	  
	  ((use) (lambda (self)
		   (gl:use-program %program-id)))
	  
	  ((unuse) (lambda (self)
		     (gl:use-program 0)))

	  

	  ((destroy) (lambda (self)
		       (if (/= %program-id 0)
			   (gl:delete-program %program-id)))))))))
