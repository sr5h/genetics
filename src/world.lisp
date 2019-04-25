;;;; world.lisp

(in-package :genetics)

(defun make-world (glsl-id)
  (let (;; objects
	(%coords nil)
	(%sphere nil)
	(%sprites nil))
    
    (lambda (message)
      (case message
	
	((initialize) (lambda (self)
			;; sphere
			(setf %sphere (make-sphere))
			(ask %sphere 'initialize)
			;; sprites
			(setf %sprites (make-sprites))
			(ask %sprites 'initialize 1 1 1 1)
			;; objects coordinates
			(setf %coords
  `((,(random 3.0) ,(random 3.0) ,(random 3.0))
    (,(random 3.0) ,(random 3.0) ,(random 3.0))
    (,(random 3.0) ,(random 3.0) ,(random 3.0))
    (,(random 3.0) ,(* -1.0 (random 3.0)) ,(random 3.0))
    (,(random 3.0) ,(* -1.0 (random 3.0)) ,(random 3.0))
    (,(random 3.0) ,(* -1.0 (random 3.0)) ,(random 3.0))
    (,(random 3.0) ,(random 3.0) ,(random 3.0))
    (,(random 3.0) ,(random 3.0) ,(random 3.0))
    (,(random 3.0) ,(random 3.0) ,(random 3.0))
    (,(* -1.0 (random 3.0)) ,(random 3.0) ,(random 3.0))
    (,(* -1.0 (random 3.0)) ,(random 3.0) ,(random 3.0))
    (,(* -1.0 (random 3.0)) ,(random 3.0) ,(random 3.0))
    (,(random 3.0) ,(random 3.0) ,(* -1.0 (random 3.0)))
    (,(random 3.0) ,(random 3.0) ,(* -1.0 (random 3.0)))
    (,(random 3.0) ,(random 3.0) ,(* -1.0 (random 3.0)))
    (,(random 3.0) ,(random 3.0) ,(random 3.0))
    (,(random 3.0) ,(random 3.0) ,(random 3.0))
    (,(random 3.0) ,(random 3.0) ,(random 3.0))
    (,(random 3.0) ,(random 3.0) ,(* -1.0 (random 3.0)))
    (,(random 3.0) ,(random 3.0) ,(* -1.0 (random 3.0)))
    (,(random 3.0) ,(random 3.0) ,(* -1.0 (random 3.0)))
    (,(random 3.0) ,(random 3.0) ,(random 3.0))
    (,(random 3.0) ,(random 3.0) ,(random 3.0))
    (,(random 3.0) ,(random 3.0) ,(random 3.0))
    (,(random 3.0) ,(* -1.0 (random 3.0)) ,(* -1.0 (random 3.0)))
    (,(random 3.0) ,(* -1.0 (random 3.0)) ,(* -1.0 (random 3.0)))
    (,(random 3.0) ,(* -1.0 (random 3.0)) ,(* -1.0 (random 3.0)))))))

	;; TODO: put-object


	((draw) (lambda (self objects)
		  (labels ((iter (c o)
			     (cond ((null o) t)
				   ((null c)
				    (error "coords not sufficient"))
				   (t (let ((l (to-list
						(translate
						 (make-matrix)
						 (caar c)
						 (cadar c)
						 (caddar c)))))
					
					(set-uniform-4fv glsl-id "model" l)
					(set-uniform-4fv
					 glsl-id
					 "view"
					 (to-list
					  (translate (make-matrix)
						     0.0
						     0.0
						     -13.0)))
					(set-uniform-4fv
					 glsl-id
					 "projection"
					 (to-list
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
	
	((get-sphere) (lambda (self)
			%sphere))
	
	((get-sprites) (lambda (self)
			 %sprites))
	
	((get-coords) (lambda (self)
			%coords))
	
	((destroy) (lambda (self)
		     (ask %sphere 'destroy)
		     (ask %sprites 'destroy)))))))

;; for modeling interactively
(defun draw (world glsl-id)
  (let ((sphere (ask world 'get-sphere))
	(sprites (ask world 'get-sprites))
	(objects nil)
	(coords (length (ask world 'get-coords))))

    ;; TODO: make uniform matrix interactively
    ;; (set-uniform-4fv
    ;; 		     glsl-id
    ;; 		     "view"
    ;; 		     (to-list
    ;; 		      (translate (make-matrix) 0.0 0.0 1.0)))
    (labels ((iter (c acc)
	       (cond ((= c coords) acc)
		     (t (iter (+ c 1) (append acc (list sphere)))))))
      (setf objects (iter 0 nil))

      (ask world 'draw objects))))
