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
    (loop for ox from 0 upto 2 appending
         (loop for oy from 0 upto 2 collect
              (list (+ sx ox) (+ sy oy))))))

(defun possibles (board x y)
  "gets possible numbers for that square, 
note that this excludes the number that is there"
  (let ((possible '(1 2 3 4 5 6 7 8 9)))
    (dotimes (oy 9)
         (setf possible (remove (aref board x oy) possible)))
    (dotimes (ox 9)
         (setf possible (remove (aref board ox y) possible)))
    (loop for (ox oy) in (square x y) do
         (setf possible (remove (aref board ox oy) possible)))
    possible))

(defun next-possible (board x y)
  "returns the next possible number, or 0 if there is none"
  (let ((next 0) (old (aref board x y)) (poss (possibles board x y)))
    (when poss 
      (if (= 0 old)
          (setf next (car poss))
          (loop for num in poss
             when (< old num) do (return (setf next num)))))
    next))

(defun validate (board)
  "checks that the board is valid"
  (dotimes (x 9) 
    (dotimes (y 9)
      (let ((spot (aref board x y)))
        (setf (aref board x y) 0)
        (assert (or (zerop spot) (member spot (possibles board x y))))
        (setf (aref board x y) spot)))))



(defun solve-dfs (board &optional (x 0) (y 0))
  "solves a sudoku board using a simple dfs"
  (if (= 9 y) 
      '(board t)
      (let ((spot (aref board x y)) 
            (nx (if (= x 8) 0 (1+ x))) 
            (ny (if (= x 8) (1+ y) y)))
        (if (zerop spot)
            (let ((sub 
                    (loop for next in (possibles board x y) do
                      (setf (aref board x y) next)
                      (let ((res (solve-dfs board nx ny)))
                        (when (second res)
                          (return res)))
                      (setf (aref board x y) 0))))
              (if (second sub) sub '(board nil)))
            (solve-dfs board nx ny)))))
