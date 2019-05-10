;;;; light.lisp

(in-package :genetics)


(defun make-light ()
  (let ((%super-class (make-pure-sphere))

	(%vertexes (make-object-vertexes)))

    (lambda (message)
      (case message

	((set-vertexes) (lambda (self)
			  (declare (ignore self))
			  ;; TODO: think of lamda's argument
			  (ask %vertexes 'addf-fns #'(lambda (&optional l)
						       (list 1.0 1.0 1.0))
			       ;; sphere-default-attributes
			       )
			  ;; assemble vertextes and attributes
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
		  (extend-type 'light %super-class)))

	((is-a) (lambda (self type)
		  (member type (ask self 'type))))

	(t (get-method message %super-class))))))
