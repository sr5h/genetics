(in-package :genetics)

(defun generate-rect-indices (obj)
  (let ((vertex-length (/ (length (ask obj 'get-vertexes))
			  (reduce #'+ (ask obj 'get-attributes)))))
    (labels ((iter (x l)
	       (if (<= l (+ x 2))
		   nil
		   (append (list x (+ x 1) (+ x 2)
				 (+ x 2) (+ x 1) (+ x 3))
			   (iter (+ x 2) l)))))
      (iter 0 vertex-length))))

(defun generate-cube-indices (obj)
  (let ((insufficient-indices (generate-rect-indices obj)))
    (append insufficient-indices
	    (list 6 7 0 0 7 1
		  1 7 3 3 7 5
		  0 6 2 2 6 4))))

;; (defun make-indices ()
;;   (let ((%super-class (make-root))
;;	(%fn nil)
;;	(%indices nil))

;;     (lambda (message)
;;       (case message

;;	((set-function) (lambda (self fn)
;;			       (declare (ignore self))
;;			       (setf %fn fn)))

;;	((get-indices) (lambda (self &rest args)
;;			 (declare (ignore self))
;;			 (if %indices
;;			     %indices
;;			     (setf %indices (apply %fn args)))
;;			 %indices))

;;	((type) (lambda (self)
;;		  (declare (ignore self))
;;		  (extend-type 'indices %super-class)))

;;	((is-a) (lambda (self type)
;;		  (member type (ask self 'type))))

;;	(t (get-method message %super-class))))))
