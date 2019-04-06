;;;; genetics.asd

(asdf:defsystem #:genetics
  :description "solve problems using genetic algorithm"
  :author "sr5h srolisp@gmail.com"
  :maintainer "sr5h srolisp@gmail.com"
  :license "MIT"
  :source-control (:git "https://github.com/sr5h/genetics.git")
  :bug-tracker "https://github.com/sr5h/genetics/issues"
  :version "0.0.1"
  :encoding :utf-8
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "display")   
   ))
