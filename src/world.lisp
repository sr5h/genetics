(in-package :genetics)

(defun make-world (glsl-id)
  (let (;; objects
	(%coords nil)
	(%sphere nil)
	(%cube nil)
	(%tet nil))
    
    (lambda (message)
      (case message
	
	((initialize)
	 (lambda (self)
	   (declare (ignore self))
	   ;; sphere
	   (setf %sphere (make-sphere))
	   (ask %sphere 'initialize)
	   ;; cube
	   (setf %cube (make-cube))
	   (ask %cube 'initialize)
	   ;; tetrahedron
	   (setf %tet (make-tetrahedron))
	   (ask %tet 'initialize)

	   ;; objects coordinates
	   (setf %coords
		 `((,(random 5.0) ,(random 5.0) ,(random 5.0))
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
		   (,(random 5.0) ,(* -1.0 (random 5.0)) ,(random 5.0))))))

	;; TODO: put-object
	

	((draw)
	 (lambda (self objects)
	   (declare (ignore self))
	   (labels
	       ((iter (c o)
		  (cond
		    ((null o) t)
		    ((null c)
		     (error "coords not sufficient"))
		    (t (let ((l (to-list (translate (make-matrix)
						    (caar c)
						    (cadar c)
						    (caddar c))))
			     (tick (sdl2:get-ticks)))
			 
			 (set-uniform-4fv glsl-id "model" l)
			 (set-uniform-4fv glsl-id
					  "view"
					  (to-list (translate
						    (make-matrix)
						    (* 3.0
(cos (* pi (/ (rem (/ (sdl2:get-ticks) 50) 360.0) 180))))
						    
						    (* 3.0
(sin (* pi (/ (rem (/ (sdl2:get-ticks) 50) 360.0) 180))))
						    -20.0


						    )))
			 (set-uniform-4fv glsl-id
					  "projection"
					  (to-list
					   (perspective
					    (make-matrix)
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
	
	((get-coords) (lambda (self) (declare (ignore self)) %coords))
	
	((destroy) (lambda (self)
		     (declare (ignore self))
		     (ask %sphere 'destroy)
		     (ask %cube 'destroy)
		     (ask %tet 'destroy)
		     (destroy)
		     ))))))


;; for modeling interactively
(let ((o nil))
  
  (defun draw (world)
    (let ((sphere (ask world 'get-sphere))
	  (cube (ask world 'get-cube))
	  (tet (ask world 'get-tet))

	  (coords (length (ask world 'get-coords))))

      

      ;; TODO: make uniform matrix interactively
      ;; (set-uniform-4fv
      ;; 		     glsl-id
      ;; 		     "view"
      ;; 		     (to-list
      ;; 		      (translate (make-matrix) 0.0 0.0 1.0)))

      (if (null o)
	  (labels ((iter (c acc n)
    		     (cond ((= c coords) acc)
    			   (t (iter (+ c 1) (append acc (list (case n
								((0) sphere)
								((1) cube)
								(t tet)
								)))
				    (random 3))))))
	    (setf o (iter 0 nil (random 3))))))
      

	(ask world 'draw o)
	)
      

      ;; (ask world 'draw (list sphere sphere))
      ;; (let ((cur-tick (sdl2:get-ticks)))
      ;;   (let ((diff (- cur-tick *tick*))
      ;; 	    (term 0))
      ;; 	(if (> term diff)
      ;; 	    (sdl2:delay (- term diff)))))

  (defun destroy ()
    (loop :for i :in o :do
	     (ask i 'destroy))
    (setf o nil)))
