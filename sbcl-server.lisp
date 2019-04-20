(asdf:clear-system :genetics)
(ql:quickload :swank)
(ql:quickload :genetics)
(in-package :genetics)
(bt:make-thread (lambda () (swank:create-server :port 4005 :dont-close t)))
(sdl2:make-this-thread-main (lambda () (main)))

