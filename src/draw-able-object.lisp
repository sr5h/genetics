(in-package :genetics)

(defun make-draw-able-object ()
  (let ((%super-class (make-root))
	(%vertex-array-object-id nil)
	(%vertex-buffer-object-id nil)
	(%element-buffer-object-id nil)
	(%indices nil))

    (labels ((%set-attr-pointers (cnt l stride offset)
	       (cond ((null l))
		     ;; TODO:
		     (t
		      (gl:vertex-attrib-pointer cnt
						(car l)
						:float
						:false
						(* (cffi:foreign-type-size :float) stride)
						(* (cffi:foreign-type-size :float) offset))

		      (gl:enable-vertex-attrib-array cnt)
		      (%set-attr-pointers (+ cnt 1) (cdr l) stride (+ offset (car l)))))))

      (lambda (message)
	(case message

	  ((initialize-gl) (lambda (self)
			  (declare (ignore self))
			  ;; TODO: make selectively
			  ;; VAO
			  (if (eq %vertex-array-object-id nil)
			      (setf %vertex-array-object-id (gl:gen-vertex-array)))

			  (gl:bind-vertex-array %vertex-array-object-id)

			  ;; VBO
			  (if (eq %vertex-buffer-object-id nil)
			      (setf %vertex-buffer-object-id (gl:gen-buffer)))

			  ;; EBO
			  (if (eq %element-buffer-object-id nil)
			      (setf %element-buffer-object-id (gl:gen-buffer)))

			  (if (or (> 0 %vertex-array-object-id)
				  (> 0 %vertex-buffer-object-id)
				  (> 0 %element-buffer-object-id))
			      (error "didn't initialize draw able objects"))

			  ))

	  ((initialize-obj)
	   (lambda (self obj i-fn)
	     (declare (ignore self))
	     (let* ((verts (ask obj 'get-vertexes))
		    (verts-length (length verts))
		    (vert-attrs (ask obj 'get-attributes))

		    (indices (funcall i-fn obj))
		    (inds-length (length indices))

		    (verts-gl-array (gl:alloc-gl-array :float verts-length))
		    (indices-gl-array (gl:alloc-gl-array :unsigned-int inds-length))
		    (index 0))

	       (setf %indices indices)
	       (mapc #'(lambda (x)
			 (setf (gl:glaref verts-gl-array index) x)
			 (setf index (+ index 1)))
		     verts)

	       (setf index 0)
	       (mapc #'(lambda (x)
			 (setf (gl:glaref indices-gl-array index) x)
			 (setf index (+ index 1)))
		     indices)

	       (gl:bind-buffer :array-buffer %vertex-buffer-object-id)
	       (gl:buffer-data :array-buffer :static-draw verts-gl-array)
	       (gl:bind-buffer :element-array-buffer %element-buffer-object-id)
	       (gl:buffer-data :element-array-buffer :static-draw indices-gl-array)

	       (%set-attr-pointers 0 vert-attrs (reduce #'+ vert-attrs) 0)

	       (gl:free-gl-array verts-gl-array)
	       (gl:free-gl-array indices-gl-array)
	       (gl:bind-buffer :array-buffer 0)
	       (gl:bind-vertex-array 0))))

	  ;; TODO: if type message doesn't exist,

	  ((type) (lambda (self)
		    (declare (ignore self))
		    (extend-type 'draw-able-object %super-class)))

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

	  ((draw) (lambda (self)		;delegated
		    (declare (ignore self))
		    (gl:bind-vertex-array %vertex-array-object-id)
		    (%gl:draw-elements :triangles
				       ;; 1998 ; TODO: how to get indices length?
				       (length %indices)
				       :unsigned-int
				       0)
		    (gl:bind-vertex-array 0)))

	  ((destroy) (lambda (self)
		       (declare (ignore self))

		       (if (not (eq %element-buffer-object-id nil))
			   (gl:delete-buffers (list %element-buffer-object-id)))

		       (if (not (eq %vertex-buffer-object-id nil))
			   (gl:delete-buffers (list %vertex-buffer-object-id)))

		       (if (not (eq %vertex-array-object-id nil))
			   (gl:delete-vertex-arrays (list %vertex-array-object-id)))))

	  (t (get-method message %super-class)))))))
