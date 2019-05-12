(in-package :genetics)

;; (defclass asdf ((c a)  ((a nil)   f   c))
;;   ((a ()))
;;   )
;; TODO: implement member variables
;; (define-class test (root)
;;     (v a d)
;;     :member-function ()
;;   )


;; ((m1 v1) (m2 v2) &key (k1 v1) (k2 k2))
(defun generate-member-variables (mvs)
  (loop :for v :on mvs :while (not (or (eq '&optional (car v))
				       (eq '&key (car v))))
     :if (consp (car v))
     :collect (car v) :into m-variables
     :else
     :collect (car v) :into args
    :finally (return (values m-variables (append args v)))))


;; (defune-class test-class nil nil :member-fns (...)) ?
;; TODO: many nested
(defmacro define-class (name super-classes member-variables member-fns
			&body body)
  (let ((class-name (intern (concatenate 'string
					 (symbol-name 'make-)
					 (symbol-name name))))
	(super-classes-syms (mapcar #'(lambda (s) (cons s (gensym))) super-classes)))

    (multiple-value-bind
	  (member-vars keyword-vars) (generate-member-variables member-variables)

    `(defun ,class-name ,keyword-vars

       (let* (,@(mapcar #'(lambda (s)
			    `(,(cdr s)
			       (,(intern (concatenate 'string
						      (symbol-name 'make-)
						      (symbol-name (car s)))))))
			super-classes-syms)
	      ,@member-vars
		)
	 (labels ,member-fns
	   (lambda (message)
	     (case message
	       ,@body
	       ((type) (lambda (self)
			 (declare (ignore self))
			 (extend-type ',name ,@(mapcar #'(lambda (s) (cdr s))
						       super-classes-syms))))
	       ((is-a) (lambda (self type)
			 (member type (ask self 'type))))
	       (t (get-method message ,@(mapcar #'(lambda (s) (cdr s))
						super-classes-syms)))))))))))
;; TODO: many nested
;; (defmacro define-class (name (super-classes member-variables &optional member-fns)
;;			&body body)
;;   (let ((class-name (intern (concatenate 'string
;;					 (symbol-name 'make-)
;;					 (symbol-name name))))
;;	(super-classes-syms (mapcar #'(lambda (s) (cons s (gensym))) super-classes))
;;	)

;;     `(defun ,class-name ()

;;        (let* (,@(mapcar #'(lambda (s)
;;			    `(,(cdr s)
;;			       (,(intern (concatenate 'string
;;						      (symbol-name 'make-)
;;						      (symbol-name (car s)))))))
;;			super-classes-syms)
;;	      ,@(generate-member-variables member-variables)
;;		)
;;	 (labels ,member-fns
;;	   (lambda (message)
;;	     (case message
;;	       ,@body
;;	       ((type) (lambda (self)
;;			 (declare (ignore self))
;;			 (extend-type ',name ,@(mapcar #'(lambda (s) (cdr s))
;;						       super-classes-syms))))
;;	       ((is-a) (lambda (self type)
;;			 (member type (ask self 'type))))
;;	       (t (get-method message ,@(mapcar #'(lambda (s) (cdr s))
;;						super-classes-syms))))))))))

;; fn => fn
(defun is-method-p (fn)
  (cond ((not (functionp fn)) ; nil or wrong type
	 nil)
	(t fn)))

(defun no-method ()
  (error "ERROR! It's not method."))

(defun get-method (message &rest objects)

  (labels ((iterate-objects (objs)
	     (cond ((null objs) (no-method))
		   (t (let ((method (funcall (car objs) message)))
			(if (is-method-p method)
			    method
			    (iterate-objects (cdr objs))))))))
    (iterate-objects objects)))

;; (defun search-method (message &rest objects)
;;   (labels ((try (objs)
;;	     (if (null objs)
;;		 nil
;;		 (let ((method (get-method message (car objs))))
;;		   (if (not (eq method (no-method))) ;TODO:
;;		       method
;;		       (try (cdr objs)))))))
;;     (try objects)))

(defun delegate (from to message &rest args)
  (let ((method (get-method message to)))
    (if (is-method-p method)
	(apply method from args)
	(no-method))))

(defun ask (object message &rest args)
  (let ((method (get-method message object)))
    (if (is-method-p method)
	(apply method object args)
	(no-method))))

(defun extend-type (object-type &rest super-classes)
  (labels ((iterate-super-classes (classes acc)
	     (cond ((null classes) acc)
		   (t
		    (iterate-super-classes (cdr classes)
		      (append acc (ask (car classes) 'type)))))))
    (append (list object-type) (iterate-super-classes super-classes nil))))
