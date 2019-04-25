(in-package :genetics)

;; aux functions
(defun normalize (vector)
  (ask vector 'normalize))

(defun vec- (v1 v2)
  (ask v1 'sub v2))

(defun vec+ (v1 v2)
  (ask v1 'add v2))

(defun to-list (v)
  (ask v 'to-list))

(defun make-vector (a1 a2 &optional (a3 0.0) (type 'single-float))
  (lambda (message)
    (case message
      ;; vector => vector
      ((cross) (lambda (self v)
		 (let ((vec nil))
		   (if (/= 3 (length v))
		       (error "It is not vector of length 3."))
		   (let ((b1 (car v))
			 (b2 (cadr v))
			 (b3 (caddr v)))

		     (make-vector (coerce (- (* a2 b3) (* a3 b2)) type) 
				  (coerce (- (* a3 b1) (* a1 b3)) type)
				  (coerce (- (* a1 b2) (* a2 b1)) type))))))
      ;; NONE => vector
      ((normalize) (lambda (self)
		     (let ((div (sqrt (+ (expt a1 2)
					 (expt a2 2)
					 (expt a3 2)))))
		       (make-vector (coerce (/ a1 div) type)
				    (coerce (/ a2 div) type)
				    (coerce (/ a3 div) type)))))
      ;; vector => single-float
      ((dot) (lambda (self v)
	       (let ((b1 (car v))
		     (b2 (cadr v))
		     (b3 (caddr v)))
		 (coerce (+ (* a1 b1) (* a2 b2) (* a3 b3)) type))))
      ;; vector => vector
      ((add) (lambda (self v)
	       (let ((b1 (car v))
		     (b2 (cadr v))
		     (b3 (caddr v)))
		 (make-vector (coerce (+ a1 b1) type)
			      (coerce (+ a2 b2) type)
			      (coerce (+ a3 b3) type)))))
      ;; vector => vector
      ((sub) (lambda (self v)
	       (let ((b1 (car v))
		     (b2 (cadr v))
		     (b3 (caddr v)))
		 (make-vector (coerce (- a1 b1) type)
			      (coerce (- a2 b2) type)
			      (coerce (- a3 b3) type)))))
      ;;  NONE => list
      (to-list (lambda (self)
		 (list a1 a2 a3))))))
