;;;; manage-input.lisp

(in-package :genetics)

(define-class manage-input (root)
  ((%mouse-sensitivity 0.10)
   (%mouse-pitch-sensitivity 5.0)

   (%mouse-x nil)
   (%mouse-y nil)
   (%mouse-state nil)

   (%camera camera)
   &key (camera nil))
  nil

  ;; TODO:
  ((key-up) (lambda (self keyword)
	      (declare (ignore self))
	      (ask %camera 'update-by-key-up keyword)))

  ((mouse-motion) (lambda (self state x y
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

		    (setf %mouse-x x
			  %mouse-y y
			  %mouse-state state))))
