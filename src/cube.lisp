(in-package :genetics)

(defvar *cube*
  '( -0.5 -0.5  0.5 -0.5 -0.5  0.5	; left-down +z
    -0.5  0.5  0.5 -0.5  0.5  0.5	; left-up +z
    0.5 -0.5  0.5  0.5 -0.5  0.5	; right-down +z
    0.5  0.5  0.5  0.5  0.5  0.5	; right-up +z
    0.5 -0.5 -0.5  0.5 -0.5 -0.5	; right-down -z
    0.5  0.5 -0.5  0.5  0.5 -0.5	; right-up -z
    -0.5 -0.5 -0.5 -0.5 -0.5 -0.5	; left-down -z
    -0.5  0.5 -0.5 -0.5  0.5 -0.5       ; left-up -z
    ;; TODO: duplicated vertexes . so make cube indices order
   -0.5 -0.5  0.5 -0.5 -0.5  0.5	; left-down +z
    -0.5  0.5  0.5 -0.5  0.5  0.5	; left-up +z

    -0.5  0.5  0.5 -0.5  0.5  0.5	; left-up +z
    0.5  0.5  0.5  0.5  0.5  0.5	; right-up +z
    -0.5  0.5 -0.5 -0.5  0.5 -0.5       ; left-up -z
    0.5  0.5 -0.5  0.5  0.5 -0.5	; right-up -z
					;
    -0.5 -0.5  0.5 -0.5 -0.5  0.5	; left-down +z
    0.5 -0.5  0.5  0.5 -0.5  0.5	; right-down +z
    -0.5 -0.5 -0.5 -0.5 -0.5 -0.5	; left-down -z
    0.5 -0.5 -0.5  0.5 -0.5 -0.5	; right-down -z
))

(defun generate-cube-vertex (w h l)
  (let ((left (* -1.0 (/ w 2.0)))
	(right (/ w 2.0))
	(up (/ h 2.0))
	(down (* -1.0 (/ h 2.0)))
	(near (/ l 2.0))
	(far (* -1.0 (/ l 2.0))))

    (list left down near		; 0
	  left up near			; 1
	  right down near		; 2
	  right up near			; 3
	  right down far		; 4
	  right up far			; 5
	  left down far			; 6
	  left up far)))		; 7


(define-class pure-cube (root)
    ((%points (generate-cube-vertex width height length))
     &key (width 1.5) (height 1.5) (length 1.5))
    nil
  ((get-points) (lambda (self)
		  (declare (ignore self))
		  %points)))

;; TODO: fix argument dependency
(defun cube-default-attributes (&optional l)
  (list (first l)
	(second l)
	(third l)))

(define-class default-cube (pure-cube) ((%vertexes (make-object-vertexes))) nil
  ((set-vertexes) (lambda (self)
		    (declare (ignore self))
		    (ask %vertexes 'addf-fns #'cube-default-attributes)
		    (ask %vertexes 'assemblef-vertexes
			 (ask self 'get-points))))

  ((get-vertexes) (lambda (self)
		    (declare (ignore self))
		    (ask %vertexes 'get-vertexes)))

  ((get-attributes) (lambda (self)
		      (declare (ignore self))
		      (ask %vertexes 'get-attributes))))
