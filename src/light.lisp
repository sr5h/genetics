;;;; light.lisp

(in-package :genetics)

(define-class light (default-sphere) ((%vertexes (make-object-vertexes))) nil
  ((set-vertexes) (lambda (self)
		    ;; TODO: think of lamda's argument
		    (ask %vertexes 'addf-fns #'(lambda (&optional l)
						 (declare (ignore l))
						 (list 1.0 1.0 1.0)))
		    ;; assemble vertextes and attributes
		    (ask %vertexes 'assemblef-vertexes
			 (ask self 'get-points))))
  ((get-vertexes) (lambda (self)
		    (declare (ignore self))
		    (ask %vertexes 'get-vertexes)))
  ((get-attributes) (lambda (self)
		      (declare (ignore self))
		      (ask %vertexes 'get-attributes))))
