;;;; light.lisp

(in-package :genetics)

;; TODO: Implement Hierarchy. This is a cube.
(defun make-light ()
  (let ((%super-class (make-draw-able-object))
	(%vertexes nil)
	(%vertex-attributes nil)
	(%light nil))

    (lambda (message)
      (case message

	((initialize) (lambda (self)
			;; (declare (ignore self))
			(ask %super-class 'initialize)

			(setf %vertexes (generate-vertex 1.0 -90.0 0.0 10.0 20.0
							 %coordinate-sphere))
			(setf %vertex-attributes '(3 3))

			(ask self 'initialize-obj)

			;; (delegate self %super-class 'initialize)
			;; (delegate self %super-class 'initialize-obj)
			))

	((get-vertexes) (lambda (self)
			  (declare (ignore self))
			  (if %vertexes
			      %vertexes
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
		  (extend-type 'light %super-class)))

	((is-a) (lambda (self type)
		  (member type (ask self 'type))))

	((destroy) (lambda (self)
		     (declare (ignore self))
		     (ask %super-class 'destroy)))

	(t (get-method message %super-class))))))
