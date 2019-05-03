(in-package :genetics)

(defvar *tet*
  '(-0.5 -0.5  0.5 -0.5 -0.5  0.5
     0.0  0.5  0.0  0.0  0.5  0.0
     0.5 -0.5  0.5  0.5 -0.5  0.5
    0.0 -0.5 -0.5  0.5 -0.5 -0.5
    -0.5  -0.5  0.5  0.0  0.5  0.0
    -0.5 -0.5  0.5 -0.5 -0.5  0.5
    ))

;; (defvar *tet* (iter 10 nil))

(defun make-tetrahedron ()
  (let ((%super-class (make-draw-able-object))
	(%vertextes nil)
	(%vertex-attributes nil))

    (lambda (message)
      (case message

	((initialize) (lambda (self)
	                (ask %super-class 'initialize)

			(setf %vertex-attributes '(3 3))
			(setf %vertextes *tet*)

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
		  (extend-type 'tetrahedron %super-class)))
	
	((is-a) (lambda (self type)
		  (member type (ask self 'type))))
	
	((destroy) (lambda (self)
		     (declare (ignore self))
                     (ask %super-class 'destroy)))

	(t (get-method message %super-class))))))

