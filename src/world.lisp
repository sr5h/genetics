(in-package :genetics)

(defun make-world (glsl-id)
  (let (;; objects
	(%coords nil)
	(%sphere nil)
	(%cube nil)
	(%tet nil)
	(%light nil))

    (lambda (message)
      (case message

	((initialize)
	 (lambda (self)
	   (declare (ignore self))
	   ;; sphere
	   (setf %sphere (make-draw-able-object))
	   (ask %sphere 'initialize-gl)
	   (let ((obj (make-default-sphere)))
	     (print (ask obj 'set-vertexes))
	     (ask %sphere 'initialize-obj obj #'generate-rect-indices))

	   ;; cube
	   (setf %cube (make-draw-able-object))
	   (ask %cube 'initialize-gl)
	   (let ((obj (make-default-cube)))
	     (ask obj 'set-vertexes)
	     (ask %cube 'initialize-obj obj #'generate-cube-indices))
	   ;; ;; tetrahedron
	   ;; (setf %tet (make-tetrahedron))
	   ;; (ask %tet 'initialize)
	   ;; light
	   (setf %light (make-draw-able-object))
	   (ask %light 'initialize-gl)
	   (let ((obj (make-light)))
	     (ask obj 'set-vertexes)
	     (ask %light 'initialize-obj obj #'generate-rect-indices))


	   ;; objects coordinates
	   (setf %coords `((,(random 5.0) ,(random 5.0) ,(random 5.0))
			   (,(random 5.0) ,(random 5.0) ,(random 5.0))
			   (,(random 5.0) ,(random 5.0) ,(random 5.0))
			   (,(random 5.0) ,(* -1.0 (random 5.0)) ,(random 5.0))
			   (,(random 5.0) ,(* -1.0 (random 5.0)) ,(random 5.0))
			   (,(random 5.0) ,(* -1.0 (random 5.0)) ,(random 5.0))
			   (,(random 5.0) ,(random 5.0) ,(random 5.0))
			   (,(random 5.0) ,(random 5.0) ,(random 5.0))
			   (,(random 5.0) ,(random 5.0) ,(random 5.0))
			   (,(* -1.0 (random 5.0)) ,(random 5.0) ,(random 5.0))
			   (,(* -1.0 (random 5.0)) ,(random 5.0) ,(random 5.0))
			   (,(* -1.0 (random 5.0)) ,(random 5.0) ,(random 5.0))
			   (,(random 5.0) ,(random 5.0) ,(* -1.0 (random 5.0)))
			   (,(random 5.0) ,(random 5.0) ,(* -1.0 (random 5.0)))
			   (,(random 5.0) ,(random 5.0) ,(* -1.0 (random 5.0)))
			   (,(random 5.0) ,(random 5.0) ,(random 5.0))
			   (,(random 5.0) ,(random 5.0) ,(random 5.0))
			   (,(random 5.0) ,(random 5.0) ,(random 5.0))
			   (,(random 5.0) ,(random 5.0) ,(* -1.0 (random 5.0)))
			   (,(random 5.0) ,(random 5.0) ,(* -1.0 (random 5.0)))
			   (,(random 5.0) ,(random 5.0) ,(* -1.0 (random 5.0)))
			   (,(random 5.0) ,(random 5.0) ,(random 5.0))
			   (,(random 5.0) ,(random 5.0) ,(random 5.0))
			   (,(random 5.0) ,(random 5.0) ,(random 5.0))
			   (,(random 5.0) ,(* -1.0 (random 5.0)) ,(random 5.0))
			   (,(random 5.0) ,(* -1.0 (random 5.0)) ,(random 5.0))
			   (,(random 5.0) ,(* -1.0 (random 5.0)) ,(random 5.0))
			   (-3.5 3.0 10.0) ; light position
			   ))))

	;; TODO: put-object
	

	((draw)
	 (lambda (self view objects)
	   (declare (ignore self))
	   (labels ((iter (c o)
		      (cond ((null o) t)
			    ((null c)
			     (error "coords not sufficient"))
			    (t (let ((l (mat-to-list (translate (make-matrix)
								(caar c)
								(cadar c)
								(caddar c))))
				 ;; (tick (sdl2:get-ticks))
				     )

			     (set-uniform-4fv glsl-id "model" l)
			     (set-uniform-4fv glsl-id "view" (mat-to-list view))
			     (set-uniform-4fv glsl-id
					      "projection"
					      (mat-to-list
					       (perspective (make-matrix)
							    45.0
							    (/ 800.0 600.0)
							    0.1
							    100.0)))
			     
			     ;; (set-uniform-3f
			     ;;  glsl-id
			     ;;  "ranColor"
			     ;;  (coerce
			     ;;   (sin (sdl2:get-ticks))
			     ;;   'single-float)
			     ;;  (coerce
			     ;;   (cos (sdl2:get-ticks))
			     ;;   'single-float)
			     ;;  (coerce
			     ;;   (+ 2.0 (sin (sdl2:get-ticks)))
			     ;;   'single-float))
			     )
			   (ask (car o) 'draw)
			   (iter (cdr c) (cdr o))))))
	     (iter %coords objects))))

	((get-sphere) (lambda (self) (declare (ignore self)) %sphere))
	((get-cube) (lambda (self) (declare (ignore self)) %cube))
	((get-tet) (lambda (self) (declare (ignore self)) %tet))
	((get-light) (lambda (self) (declare (ignore self)) %light))

	((get-coords) (lambda (self) (declare (ignore self)) %coords))

	((destroy) (lambda (self)
		     (declare (ignore self))
		     (ask %sphere 'destroy)
		     (ask %cube 'destroy)
		     ;; (ask %tet 'destroy)
		     (ask %light 'destroy)
		     (destroy)
		     ))))))


;; for modeling interactively
(let ((o nil))
  (defun draw (world view)
    (let ((sphere (ask world 'get-sphere))
	  (cube (ask world 'get-cube))
	  ;; (tet (ask world 'get-tet))
	  (light (ask world 'get-light))

	  (coords (length (ask world 'get-coords))))

      (if (null o)
	  (labels ((iter (c acc n)
		     (cond ((= c coords) acc)
			   ((= c (- coords 1)) (iter (+ c 1)
						     (append acc (list light))
						     (random 3)))
			   (t (iter (+ c 1)
				    (append acc
					    (list
						  (case n
						  ;;   ((0) sphere)
						    ((1) cube)
						  ;;   (t tet)
						    (t sphere)
						    )
						  ))
				    (random 3))))))
	    (setf o (iter 0 nil (random 3)))))

	(ask world 'draw view o)))


  ;; (ask world 'draw (list sphere sphere))
  ;; (let ((cur-tick (sdl2:get-ticks)))
  ;;   (let ((diff (- cur-tick *tick*))
  ;;	    (term 0))
  ;;	(if (> term diff)
  ;;	    (sdl2:delay (- term diff)))))

  (defun destroy ()
    (loop :for i :in o :do
      (ask i 'destroy))
    (setf o nil)))
