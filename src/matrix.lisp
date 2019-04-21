(in-package :genetics)

;; (loop for i from 0 below dimension
;; collect (loop for j from 0 below dimension
;;	   collect (if (= i j) 1.0s0 0.0s0)))

;; '((0 0 0 t1)
;;   (0 0 0 t2)
;;   (0 0 0 t3)
;;   (0 0 0 1))

(defun make-matrix (array)
  (let ((dims-0 (array-dimension array 0))
	(dims-1 (array-dimension array 1)))
    
    (labels ((equal-dimensions (mat)
	       (equal (array-dimensions array) (array-dimensions (ask mat 'get-array))))
	     
	     ;; => matrix
	     (add (mat)
	       (assert (equal-dimensions mat))
	       (let ((mat-arr (ask mat 'get-array)))
		 (make-matrix (make-array (list dims-0 dims-1)
					  :initial-contents (loop for i from 0 below dims-0
							       collect (loop for j from 0 below dims-1
									  collect (+ (aref array i j) (aref mat-arr i j))))))))

	     ;;  => matrix
	     (mul (mat)
	       (assert (equal-dimension-last-first-p mat))
	       (let ((mat-arr (ask mat 'get-array)))
		 (make-matrix (make-array (list dims-0 (ask mat 'get-dimension 1))
		 			  :initial-contents
					  (labels ((iter (lrow lcol rrow rcol sum acc acc1)
						     (cond ((= lrow dims-0) acc)
							   ((= lcol dims-1) (iter lrow 0
										  0 (+ 1 rcol)
										  0
										  acc (append acc1 (list sum))))
							   ((= rcol (ask mat 'get-dimension 1)) (iter (+ 1 lrow) 0
												      0 0
												      0
												      (append acc (list acc1)) nil))
							   (t (iter lrow (+ 1 lcol)
								    (+ 1 rrow) rcol
								    (+ sum (* (aref array lrow lcol) (aref mat-arr rrow rcol)))
								    acc acc1)))))
					    (iter 0 0 0 0 0 nil nil))))))

	     ;; => matrix
	     (set-identity ()
	       (assert (= dims-0 dims-1))
	       (make-matrix (make-array (list dims-0 dims-1)
					:initial-contents
					(labels ((iter (r c acc acc1)
						   (cond ((= r dims-0) acc)
							 ((= c dims-1) (iter (+ r 1) 0 (append acc (list acc1)) nil))
							 ((= r c) (iter r (+ c 1) acc (append acc1 (list 1.0s0))))
							 (t (iter r (+ c 1) acc (append acc1 (list 0.0s0)))))))
					  (iter 0 0 nil nil)))))

	     ;; => symbol
	     (type () 'matrix)

	     ;; => boolean
	     (equal-identity-matrix ()
	       (labels ((iter (row col)
			  (cond ((= row dims-0) t)
				((= col dims-1) (iter (+ row 1) 0))
				((= row col) (if (/= 1.0s0 (aref array row col))
						 nil
						 (iter row (+ 1 col))))
				(t (if (/= 0.0s0 (aref array row col))
				       nil
				       (iter row (+ 1 col)))))))
		 (iter 0 0)))

	     ;; => number
	     (get-dimension (axis)
	       (if (= axis 0) dims-0 dims-1))
	     
	     ;; => boolean
	     (equal-dimension-last-first-p (obj)	;
	       (cond ((typep obj 'vector)
		      (eq (length obj) dims-1))
		     (t (eq dims-1 (ask obj 'get-dimension 0)))))
	     
	     ;; => setf
	     (translatef (coords)
	       (assert (> dims-0 (length coords)))
	       ;; (if (not (equal-identity-matrix))
	       ;; 	   (format t "WARNING! It is not IDENTITY MATRIX. But, this translation will be applied.~%"))
	       
	       (labels ((iter (row col cds)
			  (cond ((null cds) nil) ;^
				(t (setf (aref array row col) (+ (aref array row col)
								 (car cds)))
				   (iter (+ row 1) col (cdr cds))))))
		 (iter 0 (- dims-1 1) coords)))
	     
	     
	     ;; => matrix
	     (translate (coords)
	       (assert (> dims-0 (length coords)))
	       ;; (if (not (equal-identity-matrix))
	       ;; 	   (format t "WARNING! It is not IDENTITY MATRIX. But, this translation will be applied.~%"))
	       
	       (labels (;; (iter (row col cds)
			;;   (cond ((null cds) nil) ;^
			;; 	(t (setf (aref array row col) (+ (aref array row col)
			;; 					 (car cds)))
			;; 	   (iter (+ row 1) col (cdr cds)))))
			(iter (row col cds acc acc1)
			  (cond ((= row dims-0) (make-matrix (make-array (list dims-0 dims-1) :initial-contents acc)))
				((and (not (null cds)) (= col (- dims-1 1))) (iter row (+ col 1) (cdr cds) acc (append acc1 (list (+ (car cds) (aref array row col))))))
				((= col dims-1) (iter (+ row 1) 0 cds (append acc (list acc1)) nil))
				(t (iter row (+ col 1) cds acc (append acc1 (list (aref array row col))))))))
		 (iter 0 0 coords nil nil)
		 ;; (iter 0 (- dims-1 1) coords)
		 ))

	     ;; => matrix
	     (scale (coords)
	       (assert (> dims-0 (length coords)))
	       ;; (if (not (equal-identity-matrix))
	       ;; 	   (format t "WARNING! It is not IDENTITY MATRIX. But, this scale will be applied.~%"))
	       (labels ((iter (row col cds acc acc1)
			  (cond ((= row dims-0) (make-matrix (make-array (list dims-0 dims-1) :initial-contents acc)))
				((= col dims-1) (iter (+ row 1) 0 cds (append acc (list acc1)) nil))
				((and (not (null cds)) (= col row)) (iter row (+ col 1) (cdr cds) acc (append acc1 (list (* (car cds) (aref array row col))))))
				(t (iter row (+ col 1) cds acc (append acc1 (list (aref array row col))))))))
		 (iter 0 0 coords nil nil)))

	     ;; => setf
	     (scalef (coords)
	       (assert (> dims-0 (length coords)))
	       ;; (if (not (equal-identity-matrix))
	       ;; 	   (format t "WARNING! It is not IDENTITY MATRIX. But, this scale will be applied.~%"))
	       (labels ((iter (row col cds acc acc1)
			  (cond ((= row dims-0) (make-matrix (make-array (list dims-0 dims-1) :initial-contents acc)))
				((= col dims-1) (iter (+ row 1) 0 cds (append acc (list acc1)) nil))
				((and (not (null cds)) (= col row)) (iter row (+ col 1) (cdr cds) acc (append acc1 (list (* (car cds) (aref array row col))))))
				(t (iter row (+ col 1) cds acc (append acc1 (list (aref array row col))))))))
		 (iter 0 0 coords nil nil)))

	     ;; radian, coords => matrix
	     (rotate (rad coords)
	       ;; 3x3, 4x4 available
	       (let* ((co (cos rad))
		      (si (sin rad))
		      (seq (list co (* -1.0s0 si) si co)))
		 (labels ((which-axis (n cds)
			    (cond ((null coords) (error "Can't determine the axis rotated around."))
				  ((= 0.0s0 (car cds)) (which-axis (+ n 1) (cdr cds)))
				  (t (if (= 1.0s0 (car cds))
					 n
					 (error "Wrong axis-coordinates.")))))
			  (iter (row col axis acc acc1 seq)
			    (cond ((= row dims-0) acc)
				  ((= col dims-1) (iter (+ row 1) 0 axis (append acc (list acc1)) nil seq))
				  ((and (= row col) (= axis row)) (iter row (+ col 1) axis acc (append acc1 (list 1.0s0)) seq))
				  ((and (/= row col) (= axis row)) (iter row (+ col 1) axis acc (append acc1 (list 0.0s0)) seq))
				  ((and (/= row col) (= axis col)) (iter row (+ col 1) axis acc (append acc1 (list 0.0s0)) seq))

				  ((and (= row col) (= col (length coords))) (iter row (+ col 1) axis acc (append acc1 (list 1.0s0)) seq))
				  ((= col (length coords)) (iter row (+ col 1) axis acc (append acc1 (list 0.0s0)) seq))
				  
				  ((and (= row col) (= row (length coords))) (iter row (+ col 1) axis acc (append acc1 (list 1.0s0)) seq))
				  ((= row (length coords)) (iter row (+ col 1) axis acc (append acc1 (list 0.0s0)) seq))
				  
				  (t ;; (format t "row : ~a col : ~a acc : ~a acc1 : ~a seq : ~a~%" row col acc acc1 seq)
				   (iter row (+ col 1) axis acc (append acc1 (list (car seq))) (cdr seq))
				   ))))
		   (let ((axis (which-axis 0 coords)))
		     (if (= axis 1)
			 (setf seq (list co si (* -1.0s0 si) co)))
		     (mul (make-matrix (make-array (list dims-0 dims-1)
					      :initial-contents (iter 0 0 axis nil nil seq))))))))
	     
	     ;; => array
	     (get-array ()
	       array))

      (lambda (message)
	(case message

	  ((get-dimension) (lambda (self axis)
			     (get-dimension axis)))

	  ((equal-dimension-last-first-p) (lambda (self mat)
					    (equal-dimension-last-first-p mat)))

	  ((get-array) (lambda (self)
			 (get-array)))
	  ((is-identity-p) (lambda (self)
			     (equal-identity-matrix)))
	  
	  ((translate) (lambda (self &rest args)
			 (translate args)))

	  ((translatef) (lambda (self &rest args)
			  (translatef args)))

	  ((scale) (lambda (self &rest args)
		     (scale args)))

	  ((scalef) (lambda (self &rest args)
		      (scalef args)))

	  ((rotate) (lambda (self angle &rest args)
		      (let ((rad (* pi (/ angle 180))))
			(rotate rad args))))
	  
	  ((type) (lambda (self)
		    (type)))
	  
	  ((add) (lambda (self mat)
		   (add mat)))
	  
	  ((mul) (lambda (self mat)
		   (mul mat)))
	  
	  ((set-identity) (lambda (self)
			    (set-identity))))))))
