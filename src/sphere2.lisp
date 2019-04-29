(in-package :genetics)

(defun make-sphere2 ()
  (let ((super-class (make-draw-able-object)))

    (lambda (message)
      (case message

	((initialize) (lambda (self)
			(ask super-class 'initialize)))
	
	((type) (lambda (self)
		  (expand-type 'sphere super-class)))
	
	;; TODO:
	((is-a) (lambda (self type)
		  (member type (ask self 'type))))

	((destroy) (lambda (self)
		     ;; destroy

		     (ask super-class 'destroy)))
	
	(t (get-method message super-class))))))
