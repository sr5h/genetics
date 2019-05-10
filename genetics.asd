;;;; genetics.asd

(asdf:defsystem #:genetics
  :description "solve problems using genetic algorithm"
  :author "sr5h srolisp@gmail.com"
  :maintainer "sr5h srolisp@gmail.com"
  :license "MIT"
  :source-control (:git "https://github.com/sr5h/genetics.git")
  :bug-tracker "https://github.com/sr5h/genetics/issues"
  :version "0.0.1"
  ;; :encoding :utf-8
  :depends-on (#:sdl2
	       #:cl-opengl
	       ;; #:oop
	       )
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "aux")
   (:file "root")
   (:file "vector")
   (:file "matrix")
   (:file "indices")
   (:file "draw-able-object")
   (:file "object-vertexes")
   (:file "cube")
   (:file "sphere")
   (:file "tetrahedron")
   (:file "light")
   (:file "camera")
   (:file "manage-input")
   (:file "glsl-program")
   (:file "world")
   (:file "model")
   (:file "main")
   ))
