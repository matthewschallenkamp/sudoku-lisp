(in-package :sudoku-play)

(defun main ()
  (cffi:define-foreign-library sdl
    (t (:default "SDL")))
  (cffi:use-foreign-library sdl)
  (run-sudoku))

(defun build (absolute-path)
  (load (compile-file (concatenate 'string absolute-path "main.lisp")))
  (sb-ext:save-lisp-and-die
   (concatenate 'string absolute-path "bin\\main.exe") 
   :toplevel #'sudoku-play:main :executable t :application-type :gui))
