(in-package :sudoku-play)

(defmacro s-incf (item)
  "increments, then loops at 10"
  `(progn
     (incf ,item)
     (when (> ,item 9) (setf ,item 0))
     ,item))


(defun square (x y)
  "returns a list of (x y) spots that is the square around the given spot"
  (let ((sx (- x (mod x 3))) (sy (- y (mod y 3))))
    (loop for ox from 0 upto 2
          appending (loop for oy from 0 upto 2 collect (list (+ sx ox) (+ sy oy))))))


(defun possibles (board x y)
  "gets possible numbers for that square, note that this excludes the number that is there"
  (let ((possible '(1 2 3 4 5 6 7 8 9)))
    (loop for oy from 0 upto 8
          do (setf possible (remove (aref board x oy) possible)))
    (loop for ox from 0 upto 8
          do (setf possible (remove (aref board ox y) possible)))
    (loop for (ox oy) in (square x y)
          do (setf possible (remove (aref board ox oy) possible)))
    possible))

(defun next-possible (board x y)
  "returns the next possible number, or 0 if there is none"
  (let ((next 0) (spot (aref board x y)) (poss (possibles board x y)))
    (when poss 
      (loop for num in poss
          when (< spot num) do (return (setf next num)))
    	(when (= 0 next)
        (setf next (car poss))))
    next))

(defun validate (board)
  "checks that the board is valid"
  (loop for x from 0 to 8 do 
    (loop for y from 0 to 8 do
      (let ((spot (aref board x y)))
				(setf (aref board x y) 0)
				(assert (or (= spot 0) (member spot (possibles board x y))))
    		(setf (aref board x y) spot)))))