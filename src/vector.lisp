(in-package :genetics)

;; aux functions
(defun normalize (vector)
  (ask vector 'normalize))

(defun vec- (v1 v2)
  (ask v1 'sub v2))

(defun vec+ (v1 v2)
  (ask v1 'add v2))

(defun vec* (v1 v2)
  (ask v1 'mul v2))

(defun vec*s (v1 s)
  (ask v1 'smul s))

(defun cross (v1 v2)
  (ask v1 'cross v2))

(defun vec-to-list (v)
  (ask v 'to-list))

(defun make-vector (a1 a2 &optional (a3 0.0) (type 'single-float))
  (let ((%super-class (make-root)))
    (lambda (message)
      (case message
	;; vector => vector
	((cross) (lambda (self v)
		   (declare (ignore self))
		     (let ((b1 (ask v 'get 1))
			   (b2 (ask v 'get 2))
			   (b3 (ask v 'get 3)))

		       (make-vector (coerce (- (* a2 b3) (* a3 b2)) type)
				    (coerce (- (* a3 b1) (* a1 b3)) type)
				    (coerce (- (* a1 b2) (* a2 b1)) type)))))
	;; NONE => vector
	((normalize) (lambda (self)
		       (declare (ignore self))
		       (let ((div (sqrt (+ (expt a1 2)
					   (expt a2 2)
					   (expt a3 2)))))
			 (make-vector (coerce (/ a1 div) type)
				      (coerce (/ a2 div) type)
				      (coerce (/ a3 div) type)))))

	((get) (lambda (self i)
		 (declare (ignore self))
		 (case i
		   ((1) a1)
		   ((2) a2)
		   ((3) a3)
		   (t (error "It is not a index of vector.")))))

	;; vector => single-float
	((dot) (lambda (self v)
		 (declare (ignore self))
		 (let ((b1 (ask v 'get 1))
		       (b2 (ask v 'get 2))
		       (b3 (ask v 'get 3)))

		   (coerce (+ (* a1 b1) (* a2 b2) (* a3 b3)) type))))
	;; vector => vector
	((add) (lambda (self v)
		 (declare (ignore self))
		 (let ((b1 (ask v 'get 1))
		       (b2 (ask v 'get 2))
		       (b3 (ask v 'get 3)))

		   (make-vector (coerce (+ a1 b1) type)
				(coerce (+ a2 b2) type)
				(coerce (+ a3 b3) type)))))
	;; vector => vector
	((sub) (lambda (self v)
		 (declare (ignore self))
		 (let ((b1 (ask v 'get 1))
		       (b2 (ask v 'get 2))
		       (b3 (ask v 'get 3)))

		   (make-vector (coerce (- a1 b1) type)
				(coerce (- a2 b2) type)
				(coerce (- a3 b3) type)))))

	((mul) (lambda (self v)
		 (let ((a1 (ask self 'get 1))
		       (a2 (ask self 'get 2))
		       (a3 (ask self 'get 3))
		       (b1 (ask v 'get 1))
		       (b2 (ask v 'get 2))
		       (b3 (ask v 'get 3)))
		   (make-vector (coerce (* a1 b1) type)
				(coerce (* a2 b2) type)
				(coerce (* a3 b3) type)))))

	((smul) (lambda (self s)
		 (let ((a1 (ask self 'get 1))
		       (a2 (ask self 'get 2))
		       (a3 (ask self 'get 3)))

		   (make-vector (coerce (* a1 s) type)
				(coerce (* a2 s) type)
				(coerce (* a3 s) type)))))
	;;  NONE => list
	((to-list) (lambda (self)
		     (declare (ignore self))
		     (list a1 a2 a3)))

	((type) (lambda (self)
		  (declare (ignore self))
		  (extend-type 'vector %super-class)))

	;; TODO:
	((is-a) (lambda (self type)
		  (member type (ask self 'type))))

	(t (get-method message %super-class))))))
