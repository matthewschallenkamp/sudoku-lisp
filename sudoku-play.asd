(defpackage #:sudoku-play
  (:use :cl :asdf :cffi :lispbuilder-sdl)
   (:export
   	#:run-sudoku
   	))
(in-package :sudoku-play)

(defsystem sudoku-play
  :name "sudoku"
  :depends-on (lispbuilder-sdl lispbuilder-sdl-ttf)
  :components
	((:file "sudoku")))
