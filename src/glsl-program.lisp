(in-package :genetics)

(defun make-glsl-program ()
  (let ((%vertex-shader-id nil)
	(%fragment-shader-id nil)
	(%program-id nil)
	(%index-attribute 0))
    (labels ((%%read-sequence-from-file (file-path)
	       (with-open-file (file-stream file-path :direction :input)
		 (let ((contents (make-string (file-length file-stream))))
		   (read-sequence contents file-stream)
		   contents)))
	     (%compile-shaders (vertex-file-path fragment-file-path)
	       (setf %vertex-shader-id (gl:create-shader :vertex-shader))
	       (setf %fragment-shader-id (gl:create-shader :fragment-shader))
	       
	       (let ((v-str (%%read-sequence-from-file vertex-file-path))
		     (f-str (%%read-sequence-from-file fragment-file-path)))
		 
		 ;; compile vertex shader
		 (gl:shader-source %vertex-shader-id v-str)
		 (gl:compile-shader %vertex-shader-id)

		 ;; verify vertex shader
		 (if (gl:get-shader %vertex-shader-id :compile-status)
		     (format t "~a compilation successful~%" :vertex-shader)
		     (format t "Compilation failure in ~a:~% ~a~%"
			     :vertex-shader
			     (gl:get-shader-info-log %vertex-shader-id)))
		 ;; pass

		 ;; compile fragment shader
		 (gl:shader-source %fragment-shader-id f-str)
		 (gl:compile-shader %fragment-shader-id)

		 (setf %program-id (gl:create-program))))

	     (%add-attribute (name)
	       (gl:bind-attrib-location %program-id %index-attribute name)
	       (setf %index-attribute (+ 1 %index-attribute)))

	     (%link-shaders ()
	       
	       (gl:attach-shader %program-id %vertex-shader-id)
	       (gl:attach-shader %program-id %fragment-shader-id)

	       (gl:link-program %program-id)

	       (gl:detach-shader %program-id %vertex-shader-id)
	       (gl:detach-shader %program-id %fragment-shader-id)

	       (gl:delete-shader %vertex-shader-id)
	       (gl:delete-shader %Fragment-Shader-Id)))


      (lambda (message)
	(case message
	  ((compile) (lambda (self vertex-shader-path fragment-shader-path)
		       (%compile-shaders vertex-shader-path fragment-shader-path)))
	  
	  ((add-attribute) (lambda (self name)
			     (%add-attribute name)))
	  
	  ((link) (lambda (self)
		    (%link-shaders)))
	  
	  ((use) (lambda (self)
		   (gl:use-program %program-id)
		   (loop for i from 0 below %index-attribute
		      do (gl:enable-vertex-attrib-array i))))
	  
	  ((unuse) (lambda (self)
		     (loop for i from 0 below %index-attribute
		     	do (gl:disable-vertex-attrib-array i))
		     (gl:use-program 0)))

	  ((destroy) (lambda (self)
		       (if (/= %program-id 0)
			   (gl:delete-program %program-id)))))))))
