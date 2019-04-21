(in-package :genetics)

(deftype model-state () '(member PLAY STOP))

;; for modeling interactively
(defun draw (sprites)
  (ask sprites 'draw))

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
	(%glsl-program (make-glsl-program))
	(%sprites (make-sprites))
	(%state 'PLAY)
	)
    
    (labels ((%initialize-system ()
	       (sdl2:init :video)
	       ;; (sdl2:gl-set-attrs :context-major-version 3)
	       ;; (sdl2:gl-set-attrs :context-minor-version 3)
	       ;; (sdl2:gl-set-attrs :forward-compatible-flag 1)
	       (sdl2:gl-set-attrs :context-profile-mask
	       			  sdl2-ffi:+sdl-gl-context-profile-core+)	       

	       (setf %window (sdl2:create-window
			      :x %x
			      :y %y
			      :w %screen-width
			      :h %screen-height
			      :flags '(:opengl)))

	       (setf %context (sdl2:gl-create-context %window))

	       (print (gl:get-string :version))
	       ;; (print (gl:get-string :extensions))

	       (sdl2:gl-set-attrs :doublebuffer 1)
	       (gl:clear-color 0.0 0.0 0.0 1.0)
	       (gl:clear-depth 1.0)
	       (gl:clear :color-buffer-bit
			 :depth-buffer-bit))
	     
	     (%%initialize-shaders ()
	       (ask %glsl-program 'compile "sprites.vert" "sprites.frag")
	       ;; (ask %glsl-program 'add-attribute "vertexPosition_modelspace")
	       (ask %glsl-program 'link))

	     (%initialize-objects ()
	       (%%initialize-shaders)
	       (ask %sprites 'init -1.0 -1.0 1.0 1.0)) ; TODO
	     
	     (%%%draw-model ()
	       (gl:clear :color-buffer-bit :depth-buffer-bit)
	       (ask %glsl-program 'use)

	       (draw %sprites)

	       (ask %glsl-program 'unuse)
	       (sdl2:gl-swap-window %window))
	     
	     (%%event-loop ()
	       (sdl2:with-event-loop (:method :poll)
		 (:quit ()
			(setf %state 'STOP)
			t)
		 (:idle ()
			(let ((cur-tick (sdl2:get-ticks)))
			
		 	  (%%%draw-model)

			  (let ((diff (- (sdl2:get-ticks) cur-tick)))
			    (if (> 16 diff)
				(sdl2:delay (- 16 diff))))))))
	     
	     (%modeling ()
	       (loop until (eq %state 'STOP)
	       	  do (%%event-loop)))
	     
	     (%destroy-objects ()
	       (ask %glsl-program 'destroy)
	       (ask %sprites 'destroy))
	     
	     (%quit-system ()
	       (sdl2:gl-delete-context %context)
	       (sdl2:destroy-window %window)
	       (sdl2:quit)))
      
      (lambda (message)
	(case message
	  ((run) (lambda (self)
		   (%initialize-system)
		   (%initialize-objects)

		   (%modeling)

		   (%destroy-objects)
		   (%quit-system))))))))

(defun run (model)
  (ask model 'run))