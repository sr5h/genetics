(in-package :genetics)

;; (defparameter *tick* nil)

(deftype model-state () '(member PLAY STOP))

(defun run (model)
  (ask model 'run))


;; model
(defun make-model (&key
		     (x 0)
		     (y 0)
		     (width 800)
		     (height 600))
  (let ((%x x)
	(%y y)
	(%screen-width width)
	(%screen-height height)
	(%window nil)
	(%context nil)
	(%camera (make-camera))
	(%manage-input (make-manage-input))
	(%glsl-program (make-glsl-program))
	(%world nil)
	(%state 'PLAY))
    
    (labels ((%initialize-system ()
	       (sdl2:init :video)
	       ;; (sdl2:gl-set-attrs :context-major-version 3)
	       ;; (sdl2:gl-set-attrs :context-minor-version 3)
	       ;; (sdl2:gl-set-attrs :forward-compatible-flag 1)
	       (sdl2:gl-set-attrs
		:context-profile-mask
		sdl2-ffi:+sdl-gl-context-profile-core+)	

	       (setf %window (sdl2:create-window :x %x :y %y
						 :w %screen-width
						 :h %screen-height
						 :flags '(:opengl)))

	       (setf %context (sdl2:gl-create-context %window))

	       (print (gl:get-string :version))
	       ;; (print (gl:get-string :extensions))

	       (sdl2:gl-set-attrs :doublebuffer 1)
	       (gl:enable :depth-test))
	     
	     (%%initialize-shaders ()
	       (ask %glsl-program 'compile "sphere.vert" "sphere.frag")
	       (ask %glsl-program 'link))

	     (%initialize-objects ()
	       (%%initialize-shaders)
	       (setf %world (make-world %glsl-program))
	       (ask %world 'initialize))
	     
	     (%draw-model (view)
	       (gl:clear-color 0.0 0.0 0.0 1.0)
	       (gl:clear :color-buffer-bit :depth-buffer-bit)
	       
	       (ask %glsl-program 'use)

	       (draw %world view)

	       (ask %glsl-program 'unuse)
	       (sdl2:gl-swap-window %window))
	     
	     (%destroy-objects ()
	       (ask %world 'destroy)
	       (ask %glsl-program 'destroy))
	     
	     (%quit-system ()
	       (sdl2:gl-delete-context %context)
	       (sdl2:destroy-window %window)
	       (sdl2:quit)))
      
      (lambda (message)
	(case message
	  ((run) (lambda (self)
		   (declare (ignore self))
		   (%initialize-system)
		   (%initialize-objects)
		   

		   (loop :until (eq %state 'STOP)
	       	      :do (sdl2:with-event-loop (:method :poll)
			    
			    	    
			    (:keyup (:keysym keysym)
				    (when (sdl2:scancode= (sdl2:scancode-value keysym)
							  :scancode-escape)
				      (sdl2:push-event :quit))
				    (ask %manage-input 'get-view-by-key
					 %camera (sdl2:scancode keysym))
				    (%draw-model (ask %camera 'look-at)))
			    
			    ;; TODO: check when first left button
			    (:mousemotion (:x x :y y :state state :xrel xr :yrel yr)
					  (format t
						  "(~a, ~a): (~a, ~a) ~a~%"
						  x y xr yr state)
					  ;; TODO: how to control bit?
					  (if (= state sdl2-ffi:+sdl-button-lmask+)
					      (ask %manage-input 'get-view-by-mouse
						   %camera state x y xr yr))
					  (%draw-model (ask %camera 'look-at)))

			    (:idle () (%draw-model (ask %camera 'look-at)))
			    (:quit () (setf %state 'STOP))))
 
		   (%destroy-objects)
		   (%quit-system))))))))



