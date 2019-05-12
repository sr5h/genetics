(in-package :genetics)

(defmacro define-polygon-object ((object) generate-polygon-fn generate-indices-fn
				 &body body)
  (let ((obj (gensym)))
    `(let ((,object (make-draw-able-object))
	   (,obj (funcall ,generate-polygon-fn)))
       (ask ,object 'initialize-gl)
       (ask ,obj 'set-vertexes)
       (ask ,object 'initialize-obj
	    ,obj ,generate-indices-fn)
       ,@body
       ,object)))

(define-class world (root)
    ((%objects nil) glsl-id)
    nil
  ((initialize)
   (lambda (self)
     ;; sphere
     (define-polygon-object (sphere) #'make-default-sphere #'generate-rect-indices
       (loop :for i :from 0 :to 10
	  :do (ask self 'add-object
		   sphere (random 5.0) (random 5.0) (random 5.0))))
     ;; cube
     (define-polygon-object (cube) #'make-default-cube #'generate-cube-indices
       (ask self 'add-object
	    cube (random 5.0) (random 5.0) (random 5.0)))
     ;; light
     (define-polygon-object (light) #'make-light #'generate-rect-indices
       (ask self 'add-object
	    light -3.5 4.0 10.0))))

  ((add-object) (lambda (self obj &optional (x 0.0) (y 0.0) (z 0.0))
		  (declare (ignore self))
		  (assert (ask obj 'is-a 'root) nil "It's is not ~a type" obj)
		  (setf %objects
			(append %objects (list (cons obj (make-vector x y z)))))))

  ((draw)
   (lambda (self view)
     (declare (ignore self))
     (labels ((iter (o)
		;; o is ((obj1 x1 y1 z1) (obj2 x2 y2 z2) ...)
		(cond ((null o) t)
		      (t (let ((l (mat-to-list (translate (make-matrix)
							  (ask (cdar o) 'get 1)
							  (ask (cdar o) 'get 2)
							  (ask (cdar o) 'get 3))))
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
			 (ask (caar o) 'draw)
			 (iter (cdr o))))))
       (iter %objects))))

  ((get-object) (lambda (self index)
		  (declare (ignore self))
		  (labels ((iter (n l)
			     (cond ((null l) (error "didn't find object of index."))
				   ((= n index) (car l))
				   (t (iter (+ n 1) (cdr l))))))
		    (iter 0 %objects))))
  ;; TODO:
  ((modify-object-position) (lambda (self index v)
			      (declare (ignore self))
			      (labels ((iter (n l)
					 (cond ((null l)
						(error "didn't find object of index."))
					       ((= n index)
						(let ((object (car l)))
						  (setf (cdr object) v)))
					       (t (iter (+ n 1) (cdr l))))))
				(iter 0 %objects))))

  ((destroy) (lambda (self)
	       (declare (ignore self))
	       (if (not (null %objects))
		   (loop :for obj :in %objects :do
		     (ask (car obj) 'destroy))))))


;; for modeling interactively
(defun draw (world view)
  ;; (let* ((r 10)
  ;;	 (v (cdr (ask world 'get-object r)))
  ;;	 (l (sqrt (+ (expt (ask v 'get 1) 2)
  ;;		     (expt (ask v 'get 2) 2)
  ;;		     (expt (ask v 'get 3) 2))))
  ;;	 (new-v (make-vector (* l (cos (/ (sdl2:get-ticks) 1000)))
  ;;			     (ask v 'get 2)
  ;;			     (* l (sin (/ (sdl2:get-ticks) 1000))))))
  ;;   (ask world 'modify-object-position
  ;;	 r new-v))
  (ask world 'add-object (define-polygon-object (sphere)
			     #'make-default-sphere #'generate-rect-indices)
       (random 10.0)
       (random 10.0)
       (random 10.0))
  (ask world 'draw view))

  ;; (let ((cur-tick (sdl2:get-ticks)))
  ;;   (let ((diff (- cur-tick *tick*))
  ;;	    (term 0))
  ;;	(if (> term diff)
  ;;	    (sdl2:delay (- term diff)))))
