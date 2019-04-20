(in-package :genetics)

(defun make-sprites ()
  (let ((%x nil)
	(%y nil)
	(%width nil)
	(%height nil)
	(%vbos nil)			; 0 or nil?
	(%vaos nil))
    (labels ((%init (x y width height)
	       (setf %x x)
	       (setf %y y)
	       (setf %width width)
	       (setf %height height)

	       (if (eq %vaos nil)
		   (setf %vaos (gl:gen-vertex-arrays 1)))

	       (gl:bind-vertex-array (car %vaos))

	       (if (eq %vbos nil)
		   (setf %vbos (gl:gen-buffers 1)))

	       ;; make vertex data
	       (let* ((verts
		       #(0.0 0.0
			 -1.0 0.0
			 -1.0 -1.0
			 -1.0 -1.0
			 0.0 -1.0
			 0.0 0.0))
		      (arr (gl:alloc-gl-array :float (length verts))))
		 (dotimes (i (length verts))
		   (setf (gl:glaref arr i) (aref verts i)))
	       	 (gl:bind-buffer :array-buffer (car %vbos))
	       	 (gl:buffer-data :array-buffer :static-draw arr)
		 (gl:free-gl-array arr)
	       	 (gl:bind-buffer :array-buffer 0)
		 ))
	     (%draw ()
	       (gl:bind-buffer :array-buffer (car %vbos))
	       (gl:vertex-attrib-pointer 0 2 :float :false 0 0)
	       (gl:draw-arrays :triangles 0 6)
	       (gl:bind-buffer :array-buffer 0)
	       )
	     (%destroy ()
	       (if (not (eq %vbos nil))	; TODO check whether %vbos value is nil or 0
		   (gl:delete-buffers %vbos))
	       (if (not (eq %vaos nil))	; TODO *
		   (gl:delete-vertex-arrays %vaos))
	       ))
      (lambda (message)
	(case message
	  ((init) (lambda (self x y width height)
		    (%init x y width height)))
	  
	  ((draw) (lambda (self)
		    (%draw)))
	  
	  ((destroy) (lambda (self)
		       (%destroy))))))))
