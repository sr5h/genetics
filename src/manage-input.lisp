;;;; manage-input.lisp

(in-package :genetics)

(defun make-manage-input (&key (camera nil))
  (let ((%super-class (make-root))

	(%mouse-sensitivity 0.05)
	(%mouse-pitch-sensitivity 5.0)

	(%mouse-x nil)
	(%mouse-y nil)
	(%mouse-state nil)

	(%camera camera))

    (lambda (message)
      (case message

	;; TODO:
	((key-up) (lambda (self keyword)
		    (declare (ignore self))
		    (ask %camera 'update-by-key-up keyword)))

	;; TODO: pitch is strange! :FIXME:
	((mouse-motion)	 (lambda (self state x y
				  ;; xr yr
				  )
			   (declare (ignore self))
			   (if (or (null %mouse-x) (null %mouse-y))
			       (setf %mouse-x x
				     %mouse-y y))
			   (if (= state sdl2-ffi:+sdl-button-lmask+)
			       (let ((offset-x (- x %mouse-x))
				     (offset-y (- %mouse-y y)))
				 (ask %camera 'update-by-mouse
				      (* %mouse-sensitivity offset-x)
				      (* %mouse-sensitivity
					 %mouse-pitch-sensitivity
					 offset-y))))
			   ;; (format t "~a ~a ~a~%" x y state)
			   (setf %mouse-x x
				 %mouse-y y
				 %mouse-state state)))

      ((type) (lambda (self)
		(declare (ignore self))
		(extend-type 'manage-input %super-class)))

      ((is-a) (lambda (self type)
		(member type (ask self 'type))))

      (t (get-method message %super-class))))))
