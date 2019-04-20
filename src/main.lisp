(in-package :genetics)

(defun main ()
    (bt:interrupt-thread
     (sb-thread:main-thread)
     (lambda ()
       (sdl2:make-this-thread-main
	  (lambda ()
	    (let ((model (make-model)))
  	      (run model)))))))

