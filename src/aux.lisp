(in-package :genetics)

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
;; 	     (if (null objs)
;; 		 nil
;; 		 (let ((method (get-method message (car objs))))
;; 		   (if (not (eq method (no-method))) ;TODO:
;; 		       method
;; 		       (try (cdr objs)))))))
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

(defun expand-type (object-type &rest super-classes)
  (labels ((iterate-super-classes (classes acc)
		(cond ((null classes) acc)
		      (t       
		       (iterate-super-classes (cdr classes)
			     (append acc (ask (car classes) 'type)))))))
    (append (list object-type) (iterate-super-classes super-classes nil))))
                                                                                

