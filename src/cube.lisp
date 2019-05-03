(in-package :genetics)

(defvar *cube*
  '( -0.5 -0.5  0.5 -0.5 -0.5  0.5	; left-down +z
    -0.5  0.5  0.5 -0.5  0.5  0.5	; left-up +z
    0.5 -0.5  0.5  0.5 -0.5  0.5	; right-down +z
    0.5  0.5  0.5  0.5  0.5  0.5	; right-up +z
    0.5 -0.5 -0.5  0.5 -0.5 -0.5	; right-down -z
    0.5  0.5 -0.5  0.5  0.5 -0.5	; right-up -z
    -0.5 -0.5 -0.5 -0.5 -0.5 -0.5 	; left-down -z
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
    -0.5 -0.5 -0.5 -0.5 -0.5 -0.5 	; left-down -z
    0.5 -0.5 -0.5  0.5 -0.5 -0.5	; right-down -z
))

(defun make-cube ()
  (let ((%super-class (make-draw-able-object))
	(%vertextes nil)
	(%vertex-attributes nil))

    (lambda (message)
      (case message

	((initialize) (lambda (self)
			(ask %super-class 'initialize)

			(setf %vertex-attributes '(3 3))
			(setf %vertextes *cube*)

			(delegate self %super-class 'initialize-obj)))
	
	((get-vertexes) (lambda (self)
			  (declare (ignore self))
			  (if %vertextes
			      %vertextes
			      (error "Initialize vertexes, first!"))))

	((get-attributes) (lambda (self)
			    (declare (ignore self))
			    (if %vertex-attributes
				%vertex-attributes
				(error
				 "Initialize vertex-attributes, too!"))))

	((draw) (lambda (self)
		  (delegate self %super-class 'draw)))
	
	((type) (lambda (self)
		  (declare (ignore self))
		  (extend-type 'cube %super-class)))
	
	((is-a) (lambda (self type)
		  (member type (ask self 'type))))

	((destroy) (lambda (self)
		     (declare (ignore self))
                     (ask %super-class 'destroy)))

	(t (get-method message %super-class))))))



