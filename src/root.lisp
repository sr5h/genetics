(in-package :genetics)

;; TODO:
(defun make-root ()
  (lambda (message)
      (case message
	((type) (lambda (self)
		  (declare (ignore self))
		  (list 'root)))
	((is-a) (lambda (self type)
		  (member type (ask self 'type))))
	(t (no-method)))))
