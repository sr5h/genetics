(in-package :genetics)

(defmacro rad (ang)
  `(* pi (/ ,ang 180.0)))


(defmacro %coordinate-sphere-with-color (r a0 a1 type)
  `(list ,(coerce (* r (cos (rad a0)) (sin (rad a1))) type)   ; x
	 ,(coerce (* r (sin (rad a0))) type)                  ; y
	 ,(coerce (* r (cos (rad a0)) (cos (rad a1))) type)   ; z

	 ,(coerce (* r (cos (rad a0)) (sin (rad a1))) type)   ; r
	 ,(coerce (* r (sin (rad a0))) type)                  ; g
	 ,(coerce (* r (cos (rad a0)) (cos (rad a1))) type))) ; b

(defmacro %coordinate-sphere (r a0 a1 type)
  `(list ,(coerce (* r (cos (rad a0)) (sin (rad a1))) type)   ; x
	 ,(coerce (* r (sin (rad a0))) type)                  ; y
	 ,(coerce (* r (cos (rad a0)) (cos (rad a1))) type)
	 1.0
	 1.0
	 1.0))   ; z

(defmacro generate-vertex (radius angle0 angle1 step0 step1
			   gen-fn
			   &key
			     (state 0)
			     (type 'single-float))
  (if (< 90 angle0)
      nil
      (if (< 360 angle1)
	  `(generate-vertex ,radius ,(+ step0 angle0) 0.0 ,step0 ,step1 ,gen-fn)
	  (cond ((= state 0)
		 `(append (,gen-fn ,radius ,angle0 ,angle1 ,type)
			  (generate-vertex ,radius
					   ,(+ step0 angle0) ,angle1 ,step0 ,step1
					   ,gen-fn
					   :state 1)))
		((= state 1)
		 `(append
		   (,gen-fn ,radius ,angle0 ,angle1 ,type)
		   (generate-vertex ,radius
				    ,(- angle0 step0) ,(+ angle1 step1) ,step0 ,step1
				    ,gen-fn
				    :state 0)))))))

(defun make-sphere ()
  (let ((%super-class (make-draw-able-object))
	(%vertexes nil)			; point
	(%vertex-attributes nil)
	)

    (lambda (message)
      (case message

	((initialize) (lambda (self)

			(ask %super-class 'initialize)

			(setf %vertexes (generate-vertex 1.0 -90.0 0.0 10.0 20.0
							 %coordinate-sphere-with-color))
			(setf %vertex-attributes '(3 3))

			(ask self 'initialize-obj)
			;; (delegate self %super-class 'initialize-obj)
			))

	((type) (lambda (self)
		  (declare (ignore self))
		  (extend-type 'sphere %super-class)))

	;; TODO:
	((is-a) (lambda (self type)
		  (member type (ask self 'type))))

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

	((destroy) (lambda (self)
		     (declare (ignore self))
		     ;; destroy

		     (ask %super-class 'destroy)))

	(t (get-method message %super-class))))))
