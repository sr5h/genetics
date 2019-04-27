(in-package :genetics)

;; TODO
(defun is-method-p (x)
  (cond ((functionp x) t)
	((eq x '(no-method)) nil)
	(t
	 (error "Object returned that there is not a function related to MESSAGE!!"))))

;; TODO:
(defun no-method ()
  (let ((tag (list 'no-method)))
    tag))

;; TODO:
(defun get-method (message object)
  (funcall object message))

(defun search-method (message &rest objects)
  (labels ((try (objs)
	     (if (null objs)
		 nil
		 (let ((method (get-method message (car objs))))
		   (if (not (eq method (no-method)))
		       method
		       (try (cdr objs)))))))
    (try objects)))

(defun delegate (from to message &rest args)
  (let ((method (get-method message to)))
    (if (is-method-p method)
	(apply method from args)
	(error "no method"))))

(defun ask (object message &rest args)
  (let ((method (get-method message object)))
    (if (is-method-p method) 		;??
	(apply method object args)
	(error "No method for message"))))


(defun expand-type (object-type &rest super-classes)
  (labels ((iterate-super-classes (classes acc)
		(cond ((null classes) acc)
		      (t       
		       (iterate-super-classes (cdr classes)
			     (append acc (ask (car classes) 'type)))))))
    (append (list object-type) (iterate-super-classes super-classes nil))))
                                                                                
