(defpackage #:sudoku-play
  (:use :cl :asdf :cffi :lispbuilder-sdl)
  (:export
   #:run-sudoku
   #:solve-dfs
   #:main
   #:build))
  
(in-package :sudoku-play)

(defsystem sudoku-play
  :name "sudoku"
  :depends-on (lispbuilder-sdl)
  :components 
  ((:file "sudoku-solve")
   (:file "sudoku-gui" :depends-on ("sudoku-solve"))
   (:file "main" :depends-on ("sudoku-gui"))))
