;;;; sphere.lisp

(in-package :genetics)

;;                      indices index
;; 0 1 2 c3 c4 c5   =>        0
;; 6 7 8 c9 c10 c11 =>        1
;; ...                        ... 
;;; drawing order for EBO :  0 1 2 2 1 3 2 3 4 4 3 5
;;; x (+ x 1) (+ x 2) (+ x 2) (+ x 1) (+ x 3) , (+ x 2)
(defun generate-indices (x l)
  (if (<= l (+ x 2))
      nil
      (append (list x (+ x 1) (+ x 2)
		       (+ x 2) (+ x 1) (+ x 3))
	      (generate-indices (+ x 2) l))))

(defmacro rad (ang)
  `(* pi (/ ,ang 180.0)))


;; x y z r g b
(defmacro %sphere-coords (r a0 a1 type)
  `(list
    ,(coerce (* r
	       (sin (rad a1))
	       (cos (rad a0)))
	     type)
    ,(coerce (* r
	       (sin (rad a1))
	       (sin (rad a0)))
	    type)
    ,(coerce (* r
	       (cos (rad a1)))
	    type)
    ,(coerce (* r
	       (sin (rad a1))
	       (cos (rad a0)))
	     type)
    ,(coerce (* r
	       (sin (rad a1))
	       (sin (rad a0)))
	    type)
    ,(coerce (* r
	       (cos (rad a1)))
	    type)))

(defmacro generate-vertex (radius angle0 angle1 step0 step1
			   gen-fn
			   &key
			     (state 0)
			     (type 'single-float))
  (if (< 90 angle1)
      nil
      (if (< 360 angle0)
	  `(generate-vertex ,radius 0.0 ,(+ step1 angle1) ,step0 ,step1
			    ,gen-fn)
	  (cond ((= state 0) `(append (,gen-fn ,radius ,angle0 ,angle1 ,type)
				      (generate-vertex ,radius
						       ,angle0
						       ,(+ angle1 step1)
						       ,step0
						       ,step1
						       ,gen-fn
						       :state 1)))
		((= state 1) `(append (,gen-fn ,radius ,angle0 ,angle1 ,type)
				      (generate-vertex ,radius
						       ,(+ angle0 step0)
						       ,(- angle1 step1)
						       ,step0
						       ,step1
						       ,gen-fn
						       :state 0)))))))
(defun make-sphere ()
  (let ((%vaos nil)
	(%vbos nil)
	(%ebos nil)
	(%sphere nil))
      (lambda (message)
      (case message
	
	((initialize) (lambda (self)
			(declare (ignore self))
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

			(setf %sphere (generate-vertex
				       1.0
				       0.0
				       0.0
				       10.0
				       10.0
				       %sphere-coords))

			(let* ((verts-length (length %sphere))
			       (indices (generate-indices
					 0 (/ verts-length 6)))
			       (indices-length (length indices))
			       (verts-gl-array (gl:alloc-gl-array
			       			:float verts-length))
			       (indices-gl-array (gl:alloc-gl-array
			       			  :unsigned-int
						  indices-length))
			       (index 0))

			  (mapc #'(lambda (x)
				    (setf (gl:glaref verts-gl-array index)
					  x)
				    (setf index (+ index 1)))
				%sphere)
			  (setf index 0)
			  (mapc #'(lambda (x)
				    (setf (gl:glaref indices-gl-array index)
					  x)
				    (setf index (+ index 1)))
				indices)			  

			  (gl:bind-buffer :array-buffer (car %vbos))
			  (gl:buffer-data :array-buffer
					:static-draw
					verts-gl-array)
			  (gl:bind-buffer :element-array-buffer (car %ebos))
			  (gl:buffer-data :element-array-buffer
					:static-draw
					indices-gl-array)

			  (gl:vertex-attrib-pointer
			   0
			   3
			   :float
			   :false
			   (* (cffi:foreign-type-size :float) 6) 0)
			  (gl:enable-vertex-attrib-array 0)

			  (gl:vertex-attrib-pointer
			   1
			   3
			   :float
			   :false
			   (* (cffi:foreign-type-size :float) 6)
			   (* (cffi:foreign-type-size :float) 3))
			  (gl:enable-vertex-attrib-array 1)

			  (gl:free-gl-array verts-gl-array)
			  (gl:free-gl-array indices-gl-array)
			  (gl:bind-buffer :array-buffer 0)
			  )
			(gl:bind-vertex-array 0)
			))

	((draw) (lambda (self)
		  (declare (ignore self))
		  (gl:bind-vertex-array (car %vaos))
		  ;; set indices to member variable?
		  (%gl:draw-elements :triangles
				     1998 ; TODO: how to get indices length? 
		  		     :unsigned-int
				     0)
		  (gl:bind-vertex-array 0)))

	((destroy) (lambda (self)
		     (declare (ignore self))
		     (if (not (eq %vbos nil))	
			 (gl:delete-buffers %vbos))
		     (if (not (eq %vaos nil))	
			 (gl:delete-vertex-arrays %vaos))
		     (if (not (eq %ebos nil))
			 (gl:delete-buffers %ebos))))))))
