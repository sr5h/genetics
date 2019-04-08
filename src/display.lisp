(in-package :genetics)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defun sdl-main (&key (delay 5000))
  (sdl2:with-init (:video)
    (sdl2:with-window (window :title "SDL2 Window" :w *screen-width* :h *screen-height*)
      (let ((screen-surface (sdl2:get-window-surface window)))
        (sdl2:fill-rect screen-surface
                        nil
                        (sdl2:map-rgb (sdl2:surface-format screen-surface) 255 255 255))
	(sdl2:with-event-loop (:method :poll)
	  (:quit () t)
	  (:idle ()
	(sdl2:update-window window)
	))))))

(defun main ()
  (bt:interrupt-thread
   (sb-thread:main-thread)
   (lambda ()
     (sdl2:make-this-thread-main
      (lambda ()
	(sdl-main))))))
