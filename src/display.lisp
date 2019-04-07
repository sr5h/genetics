(in-package :genetics)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defun main (&key (delay 5000))
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :title "SDL2 Window"
			      :w *screen-width* :h *screen-height*)
      
      (let ((screen-surface (sdl2:get-window-surface window)))
      	(sdl2:fill-rect screen-surface
      			(sdl2:make-rect
			 0 0 300 300)
      			(sdl2:map-rgb
      			 (sdl2:surface-format screen-surface) 0 0 255))
	(sdl2:update-window window)
	(sdl2:delay delay)))))
