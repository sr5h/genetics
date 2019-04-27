(in-package :genetics)

(defun make-sprites ()
  (let ((%x nil)
	(%y nil)
	(%width nil)
	(%height nil)
	(%vbos nil)			; 0 or nil?
	(%vaos nil)
	(%ebos nil))

    (labels ((%init (x y width height)
	       (setf %x x)
	       (setf %y y)
	       (setf %width width)
	       (setf %height height)

	       ;; VAO
	       (if (eq %vaos nil)
		   (setf %vaos (gl:gen-vertex-arrays 1)))

	       (gl:bind-vertex-array (car %vaos))

	       ;; VBO
	       (if (eq %vbos nil)
		   (setf %vbos (gl:gen-buffers 1)))

	       ;; EBO
	       (if (eq %ebos nil)
		   (setf %ebos (gl:gen-buffers 1)))
	       (format t  "vaos ~a vbos ~a element ~a~%" %vaos %vbos %ebos)
	       ;; make vertex data
	       (let* ((verts
		       #(0.0 0.5 1.0 1.0 0.0 0.0
			 -0.5 -0.5 0.0 0.0 1.0 0.0
			 0.5 -0.5 0.2 0.0 0.0 1.0
			 0.0 -1.0 0.8 0.5 0.5 0.5
			 ))
		      (indices #(0 1 2 2 3 0
				 ))
		      (arr (gl:alloc-gl-array :float (length verts)))
		      (arr1 (gl:alloc-gl-array
			     :unsigned-int (length indices))))
		 
		 (dotimes (i (length verts))
		   (setf (gl:glaref arr i) (aref verts i)))
		 (dotimes (i (length indices))
		   (setf (gl:glaref arr1 i) (aref indices i)))
		 
		 ;; (print (gl:gl-array-byte-size arr))
		 ;; (print (cffi:foreign-type-size :float))

	       	 (gl:bind-buffer :array-buffer (car %vbos))
	       	 (gl:buffer-data :array-buffer :static-draw arr)

		 (gl:bind-buffer :element-array-buffer (car %ebos))
		 (gl:buffer-data :element-array-buffer
				 :static-draw
				 arr1)					

		 (gl:vertex-attrib-pointer
		  0 3 :float :false (* (cffi:foreign-type-size :float) 6) 0) 
		 (gl:enable-vertex-attrib-array 0)

		 (gl:vertex-attrib-pointer
		  1 3 :float :false (* (cffi:foreign-type-size :float) 6)
		  (* 3 (cffi:foreign-type-size :float)))
		 (gl:enable-vertex-attrib-array 1)

		 (gl:free-gl-array arr)
	       	 (gl:bind-buffer :array-buffer 0))

	       (gl:bind-vertex-array 0))
	     
	     (%draw ()
	       (gl:bind-vertex-array (car %vaos))
	       ;; element type, indices length, indices type, offset
	       (%gl:draw-elements :triangles 1998 :unsigned-int 0)
	       (gl:bind-vertex-array 0))

	     (%destroy ()
	       (if (not (eq %vbos nil))	
		   (gl:delete-buffers %vbos))
	       (if (not (eq %vaos nil))	
		   (gl:delete-vertex-arrays %vaos))
	       (if (not (eq %ebos nil))
		   (gl:delete-buffers %ebos))))

      (lambda (message)
	(case message

	  ((initialize) (lambda (self x y width height)
			  (%init x y width height)))
	  
	  ((draw) (lambda (self)
		    (%draw)))
	  
	  ((destroy) (lambda (self)
		       (%destroy))))))))
