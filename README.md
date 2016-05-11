# sudoku-lisp
This is a small side project in which I am using lisp to make a sudoku player and solver.
This project is FAR from finished, and is only built to run on my machine.

## Installing
This project runs on [sbcl](http://www.sbcl.org/) or another lisp with 
[quicklisp](https://www.quicklisp.org/beta/) installed and loaded and
[lispbuilder-sdl](https://github.com/lispbuilder/lispbuilder) installed and loaded through quicklisp.

That done, you need to do three things:

place both files in quicklisp\local-projects\sudoku
```
echo sudoku\sudoku-play.asd > system-index.txt
```
load into sbcl with `(ql:quickload "sudoku-play")`

## Running
```
(sudoku-play:run-sudoku)
```
