;;;; matrix.lisp

(in-package :genetics)

;;; matrix aux functions

;; TODO: naming
(defun set-identity (matrix)
  (ask matrix 'setf-identity))

(defun translate (matrix x y z)
  (ask matrix 'translatef x y z))

;; TODO: to apply multiple values of (x y z)
(defun rotate (matrix angle x y z)
  (ask matrix 'rotatef angle x y z))

(defun perspective (matrix fov ratio front back)
  (ask matrix 'perspectivef fov ratio front back))

(defun mat-to-list (matrix)
  (ask matrix 'to-list))

(defun mat* (mat1 mat2)
  (ask mat1 'mulf mat2))

(defun look-at (matrix camera-pos target-pos up-vector)
  (ask matrix 'look-at camera-pos target-pos up-vector))

;; array, element-type, row, col => array , nil or ERROR
(defun %verify-array (array element-type row col)
  (if (null array)
      nil
      (if (and (typep array 'array)
	       (eq element-type (array-element-type array))
	       (equal (list row col) (array-dimensions array)))
	  array
	  (progn (format t
			 "~a ~a ~a~%"
			 (typep array 'array)
			 (eq element-type (array-element-type array))
			 (equal (list row col) (array-dimensions array)))
		 (error "Arguments of matrix is insufficient or wrong.")))))

;; matrix class
(define-class matrix (root)
    ((%array (%verify-array array type row col))
     &key (array nil) (row 4) (col 4) (type 'single-float))
    ;; => setf %array
    ((%initializef ()
       ;; (format t "WARNING! Initialized matrix of ~a~%" type)
       (setf %array (make-array (list row col)
				:element-type type
				:initial-element (coerce 0 type))))

     ;; matrix => boolean
     (%equal-dimensions (mat)
       (equal (array-dimensions %array) (array-dimensions (ask mat 'get-array))))

     ;; => boolean
     (%equal-dimension-last-first-p (obj)	;^ naming
       (cond ((typep obj 'vector)
	      (eq (length obj) col))
	     (t (eq col (ask obj 'get-dimension 0)))))

     (%perspective (left right bottom top near far)
       (make-array (list row col)
		   :element-type type
		   :initial-contents
		   `((,(coerce (/ (* 2.0 near) (- right left)) type)
		       0.0
		       ,(coerce (/ (+ right left) (- right left)) type)
		       0.0)
		     (0.0
		      ,(coerce (/ (* 2.0 near) (- top bottom)) type)
		      ,(coerce (/ (+ top bottom) (- top bottom)) type)
		      0.0)
		     (0.0
		      0.0
		      ,(coerce (/ (* -1.0 (+ far near)) (- far near)) type)
		      ,(coerce (/ (* -1.0 2.0 far near) (- far near)) type))
		     (0.0 0.0 -1.0 0.0))))

     (%ortho (left right bottom top near far)
       (make-array (list row col)
		   :element-type type
		   :initial-contents
		   `((,(coerce (/ 2.0 (- right left)) type)
		       0.0
		       0.0
		       ,(coerce (* -1.0 (/ (+ right left) (- right left))) type))
		     (0.0
		      ,(coerce (/ 2.0 (- top bottom)) type)
		      0.0
		      ,(coerce (* -1.0 (/ (+ top bottom) (- top bottom))) type))
		     (0.0
		      0.0
		      ,(coerce (/ -2.0 (- far near)) type)
		      ,(coerce (* -1.0 (/ (+ far near) (- far near))) type))
		     (0.0 0.0 0.0 1.0)))))

  ;; => matrix
  ((setf-zero) (lambda (self)
		 (%initializef)
		 self))

  ;; => matrix
  ((setf-identity) (lambda (self)
		     (assert (= row col))
		     (%initializef)
		     (labels ((iter (r c)
				(cond ((= r row) self)
				      ((= r c)
				       (setf (aref %array r c) (coerce 1 type))
				       (iter (+ r 1) (+ c 1)))
				      (t (error "something")))))
		       (iter 0 0))))

  ;;   ((is-identity-p) (lambda (self)
  ;;		     (equal-identity-matrix)))

  ;; matrix => matrix
  ((addf) (lambda (self mat)
	    (if (not (%verify-array %array type row col)) ;^CHECK
		(%initializef))
	    (assert (%equal-dimensions mat))
	    (let ((mat-arr (ask mat 'get-array)))
	      (loop for i from 0 below row collect
		(loop for j from 0 below col collect
		  (setf (aref %array i j)
			(coerce (+ (aref %array i j) (aref mat-arr i j)) type))))
	      self)))

  ;; col dimension can be changed! (ex-dimensions: 2x3 3x2 => 2x2)
  ;; so %array is binded to new array.
  ;; matrix => matrix
  ((mulf)
   (lambda (self mat)
     ;; can use (ask (make-matrix) 'scalef 2.0 2.0 2.0). useful?
     (if (not (%verify-array %array type row col)) ;^CHECK
	 (progn (%initializef)
		(ask self 'setf-identity)))
     (assert (%equal-dimension-last-first-p mat))

     (let ((mat-arr (ask mat 'get-array)))
       (labels ((iter (lrow lcol rrow rcol sum acc acc1)
		  (cond ((= lrow row)
			 ;; environment of col is
			 ;; parent. danger code!  ;^HOWTO ?
			 ;; using member variable %row %col?
			 (setf col (ask mat 'get-dimension 1)) ;^
			 (setf %array (make-array (list row
							(ask mat 'get-dimension 1))
						  :element-type type
						  :initial-contents acc))
			 self)
			((= lcol col)
			 (iter lrow 0 0 (+ 1 rcol) 0 acc (append acc1 (list sum))))
			((= rcol (ask mat 'get-dimension 1))
			 (iter (+ 1 lrow) 0 0 0 0 (append acc (list acc1)) nil))
			(t
			 (iter lrow (+ 1 lcol) (+ 1 rrow) rcol
			       (+ sum (* (aref %array lrow lcol)
					 (aref mat-arr rrow rcol)))
			       acc acc1)))))
	 (iter 0 0 0 0 0 nil nil)))))

  ;; coords => matrix
  ((translatef) (lambda (self &rest coords)
		  (assert (> row (length coords)))
		  ;; (if (not (equal-identity-matrix))
		  ;;     (format
		  ;;      t
		  ;;      "WARNING! It is not IDENTITY
		  ;;      MATRIX. But, this translation will be
		  ;;      applied.~%"))

		  ;; make transform matrix array
		  (let ((arr (make-array (list row col)
					 :element-type type
					 :initial-element (coerce 0 type))))
		    (labels ((iter (r c cds)
			       (cond ((null cds) arr)
				     (t (setf (aref arr r c)
					      (coerce (+ (aref arr r c) (car cds))
						      type))
					(iter (+ r 1) c (cdr cds))))))
		      ;; apply transform matrix to self
		      (if (null %array)
			  (ask self 'setf-identity))
		      (ask self 'addf (make-matrix :array (iter 0 (- col 1) coords)
						   :row row
						   :col col
						   :type type))))))

  ;; coords => matrix
  ((scalef) (lambda (self &rest coords)
	      (assert (> row (length coords)))
	      ;; (if (not (equal-identity-matrix))
	      ;;	  (format
	      ;;	   t
	      ;;	   "WARNING! It is not IDENTITY MATRIX. But,
	      ;;	   this scale will be applied.~%"))

	      ;; make transform matrix array
	      (let ((arr (make-array (list row col)
				     :element-type type
				     :initial-element
				     (coerce 0 type))))
		(setf (aref arr (- row 1) (- col 1)) (coerce 1.0 type))
		(labels ((iter (r c cds)
			   (cond ((null cds) arr)
				 (t
				  (setf (aref arr r c)
					(coerce (+ (aref arr r c) (car cds)) type))
				  (iter (+ r 1) (+ c 1) (cdr cds))))))
		  ;; apply transform matrix to self
		  (ask self 'mulf (make-matrix :array (iter 0 0 coords)
					       :row row
					       :col col
					       :type type))))))


  ;; 4x4
  ;; 1  0  0  0    C  0  S  0   C -S  0  0
  ;; 0  C -S  0    0  1  0  0   S  C  0  0
  ;; 0  S  C  0   -S  0  C  0   0  0  1  0
  ;; 0  0  0  1    0  0  0  1   0  0  0  1
  ;;
  ;; 3x3 correct ?
  ;; C -S  0
  ;; S  C  0
  ;; 0  0  1
  ;;
  ;; radian, coords => matrix
  ((rotatef)
   (lambda (self angle &rest coords)
     ;; 3x3, 4x4 available
     (let* ((rad (coerce (* pi (/ angle 180)) type))
	    (co (coerce (cos rad) type))
	    (si (coerce (sin rad) type))
	    (arr nil))
       ;; make transform matrix array
       (case row
	 ((3) (setf arr (make-array (list row col)
				    :element-type type
				    :initial-contents `((,co ,(* -1 si) 0.0)
							(,si ,co 0.0)
							(0.0 0.0 1.0)))))
	 ((4)
	  (cond
	    ((eq 1.0 (car coords))
	     (setf arr (make-array (list row col)
				   :element-type type
				   :initial-contents `((1.0 0.0 0.0 0.0)
						       (0.0 ,co ,(* -1.0 si) 0.0)
						       (0.0 ,si ,co 0.0)
						       (0.0 0.0 0.0 1.0)))))
	    ((eq 1.0 (cadr coords))
	     (setf arr (make-array (list row col)
				   :element-type type
				   :initial-contents `((,co 0.0 ,si 0.0)
						       (0.0 1.0 0.0 0.0)
						       (,(* -1.0 si) 0.0 ,co 0.0)
						       (0.0 0.0 0.0 1.0)))))
	    ((eq 1.0 (caddr coords))
	     (setf arr (make-array (list row col)
				   :element-type type
				   :initial-contents `((,co ,(* -1.0 si) 0.0 0.0)
						       (,si ,co 0.0 0.0)
						       (0.0 0.0 1.0 0.0)
						       (0.0 0.0 0.0 1.0)))))
	    (t (error "rotating is failed."))))
	 (t (error "It is not 3x3 or 4x4 matrix.")))
       (ask self 'mulf (make-matrix :array arr :row row :col col :type type)))))

  ((perspectivef) (lambda (self fov ratio front back)
		    (let* ((rad (* pi (coerce (/ fov 180) type)))
			   (half-tangent (tan (/ rad 2.0)))
			   (height (* front half-tangent))
			   (width (* height ratio)))
		      ;; left right bottom top near far => array
		      (ask self 'mulf
			   (make-matrix :array (%perspective (* -1.0 width)
							     width
							     (* -1.0 height)
							     height
							     front
							     back)
					:row row
					:col col
					:type type)))))

  ((orthof) (lambda (self left right bottom top near far)
	      (ask self 'mulf
		   (make-matrix :array (%ortho left
					       right
					       bottom
					       top
					       near
					       far)
				:row row
				:col col
				:type type))))

  ((look-at) (lambda (self vec-camera-pos vec-target-pos vec-up)
	       (if (not (and (ask vec-camera-pos 'is-a 'vector)
			     (ask vec-target-pos 'is-a 'vector)
			     (ask vec-up 'is-a 'vector)))
		   (error "use (make-vector) for arguments"))
	       ;; (if (null %array)
	       ;;	   (ask self 'setf-identity))
	       (let* ((z-vector (normalize (vec- vec-camera-pos vec-target-pos)))
		      (x-vector (normalize (cross vec-up z-vector)))
		      (y-vector (normalize vec-up))
		      (arr (make-array '(4 4)
				       :element-type 'single-float
				       :initial-contents
				       (list
					(append (ask x-vector 'to-list) (list 0.0))
					(append (ask y-vector 'to-list) (list 0.0))
					(append (ask z-vector 'to-list) (list 0.0))
					(list 0.0 0.0 0.0 1.0))))
		      (m1 (make-matrix :array arr))
		      (m2 (ask (make-matrix) 'translatef ; is right? self? ;^
			       (* -1.0 (ask vec-camera-pos 'get 1))
			       (* -1.0 (ask vec-camera-pos 'get 2))
			       (* -1.0 (ask vec-camera-pos 'get 3)))))
		 (ask self 'mulf (mat* m1 m2)))))

  ;; => number
  ((get-dimension) (lambda (self axis)
		     (declare (ignore self))
		     (if (= axis 0) row col)))

  ;; => list
  ((to-list) (lambda (self)
	       (declare (ignore self))
	       (labels ((iter (r c acc)
			  (cond ((= r row) acc)
				((= c col) (iter (+ r 1) 0 acc))
				(t (iter r
					 (+ c 1)
					 (append acc (list (aref %array r c))))))))
		 (iter 0 0 nil))))

  ;; => array
  ((get-array) (lambda (self) (declare (ignore self)) %array)))
