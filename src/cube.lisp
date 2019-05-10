(in-package :genetics)

(defvar *cube*
  '( -0.5 -0.5  0.5 -0.5 -0.5  0.5	; left-down +z
    -0.5  0.5  0.5 -0.5  0.5  0.5	; left-up +z
    0.5 -0.5  0.5  0.5 -0.5  0.5	; right-down +z
    0.5  0.5  0.5  0.5  0.5  0.5	; right-up +z
    0.5 -0.5 -0.5  0.5 -0.5 -0.5	; right-down -z
    0.5  0.5 -0.5  0.5  0.5 -0.5	; right-up -z
    -0.5 -0.5 -0.5 -0.5 -0.5 -0.5	; left-down -z
    -0.5  0.5 -0.5 -0.5  0.5 -0.5       ; left-up -z
    ;; TODO: duplicated vertexes . so make cube indices order
   -0.5 -0.5  0.5 -0.5 -0.5  0.5	; left-down +z
    -0.5  0.5  0.5 -0.5  0.5  0.5	; left-up +z

    -0.5  0.5  0.5 -0.5  0.5  0.5	; left-up +z
    0.5  0.5  0.5  0.5  0.5  0.5	; right-up +z
    -0.5  0.5 -0.5 -0.5  0.5 -0.5       ; left-up -z
    0.5  0.5 -0.5  0.5  0.5 -0.5	; right-up -z
					;
    -0.5 -0.5  0.5 -0.5 -0.5  0.5	; left-down +z
    0.5 -0.5  0.5  0.5 -0.5  0.5	; right-down +z
    -0.5 -0.5 -0.5 -0.5 -0.5 -0.5	; left-down -z
    0.5 -0.5 -0.5  0.5 -0.5 -0.5	; right-down -z
))

(defun generate-cube-vertex (w h l)
  (let ((left (* -1.0 (/ w 2.0)))
	(right (/ w 2.0))
	(up (/ h 2.0))
	(down (* -1.0 (/ h 2.0)))
	(near (/ l 2.0))
	(far (* -1.0 (/ l 2.0))))

    (list left down near		; 0
	  left up near			; 1
	  right down near		; 2
	  right up near			; 3
	  right down far		; 4
	  right up far			; 5
	  left down far			; 6
	  left up far)))		; 7

(defun make-pure-cube (&key
		    (width 1.5)
		    (height 1.5)
		    (length 1.5))
  (let ((%super-class (make-root))
	(%points (generate-cube-vertex width height length)))

    (lambda (message)
      (case message

	((get-points) (lambda (self)
			(declare (ignore self))
			%points))

	((type) (lambda (self)
		  (declare (ignore self))
		  (extend-type 'cube %super-class)))

	((is-a) (lambda (self type)
		  (member type (ask self 'type))))

	(t (get-method message %super-class))))))
;; TODO: fix argument dependency
(defun cube-default-attributes (&optional l)
  (list (first l)
	(second l)
	(third l)))

(defun make-default-cube ()
  (let ((%super-class (make-pure-cube))

	(%vertexes (make-object-vertexes)))

    (lambda (message)
      (case message

	((set-vertexes) (lambda (self)
			  (declare (ignore self))
			  (ask %vertexes 'addf-fns #'cube-default-attributes)
			  (ask %vertexes 'assemblef-vertexes
			       (ask %super-class 'get-points))))

	((get-vertexes) (lambda (self)
			  (declare (ignore self))
			  (ask %vertexes 'get-vertexes)))

	((get-attributes) (lambda (self)
			    (declare (ignore self))
			    (ask %vertexes 'get-attributes)))

	((type) (lambda (self)
		  (declare (ignore self))
		  (extend-type 'default-cube %super-class)))

	((is-a) (lambda (self type)
		  (member type (ask self 'type))))

	((destroy) (lambda (self)
		     (declare (ignore self))
		     (ask %super-class 'destroy)))

	(t (get-method message %super-class))))))
