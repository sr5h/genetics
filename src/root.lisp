(in-package :genetics)

;; TODO:
(defun make-root ()
  (lambda (message)
    (case message

      ((type) (lambda (self)
		(declare (ignore self))
		(error "didn't make virtual funtions.")))

      ((is-a) (lambda (self)
		(declare (ignore self))
		(error "didn't make virtual funtions."))))))
