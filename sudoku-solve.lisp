(in-package :sudoku-play)

(defmacro s-incf (item)
  `(progn
     (incf ,item)
     (when (> ,item 9) (setf ,item 0))
     ,item))


(defun square (x y)
  (let ((sx (- x (mod x 3))) (sy (- y (mod y 3))))
    (loop for ox from 0 upto 2
          appending (loop for oy from 0 upto 2 collect (list (+ sx ox) (+ sy oy))))))


(defun possibles (board x y)
  (let ((possible '(1 2 3 4 5 6 7 8 9)))
    (loop for oy from 0 upto 8
          do (setf possible (delete (aref board x oy) possible)))
    (loop for ox from 0 upto 8
          do (setf possible (delete (aref board ox y) possible)))
    (loop for (ox oy) in (square x y)
          do (setf possible (delete (aref board ox oy) possible)))
    possible))

