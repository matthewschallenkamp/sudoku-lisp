;this project needs to have lispbuilder-sdl installed and loaded into quicklisp
;to run:
;place both files in quicklisp\local-projects\sudoku
;echo sudoku\sudoku-play.asd > system-index.txt
;load into sbcl with (ql:quickload "sudoku-play")

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
