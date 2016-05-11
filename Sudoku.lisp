;(asdf:operate 'asdf:load-op 'sudoku-play)

(in-package :sudoku-play)

(defparameter *bsize* 50)
(defparameter *width* (* 11 *bsize*))
(defparameter *height* (* 11 *bsize*))

(defmacro new-draw-string-solid-* (string &rest rest)
  (let ((mstr string))
    `(if (string= "" ,mstr)
        (draw-string-solid-* " " ,@rest)
        (draw-string-solid-* ,mstr ,@rest))))

(defmacro add-draw (funct)
  `(setf draws (cons ,funct draws)))

(defmacro s-incf (item)
  `(progn
     (incf ,item)
     (when (> ,item 9) (setf ,item 0))
     ,item))

(defmacro sudoku-button ((ix iy l h num) &body body)
  `(let ((bcolor *white*) (scolor *black*) (tcolor *black*))
    (add-draw (lambda () 
                (let ((nnum ,num))
                (new-draw-string-solid-* ;(write-to-string ,text)
                  (if (= 0 nnum) "" (write-to-string nnum))
                  (+ 13 ,ix) (+ 6 ,iy) :color tcolor :font *default-font*))))
    (add-draw (lambda () (draw-box-* ,ix ,iy ,l ,h :color bcolor :stroke-color scolor)))
    (setf button-events (cons (lambda (button state x y) ,@body) button-events))))

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

(defun run-sudoku ()
  ;basic data
  (let ((button-events '()) (draws '()) 
        (board (make-array '(9 9) :initial-contents 
            (loop for x from 1 upto 9 collect (loop for y from 1 upto 9 collect 0))))
        (board-info nil)) ;board info will include the rest of the board data
    (sdl:with-init ()
      (sdl:initialise-default-font sdl:*ttf-font-vera*)
     	(sdl:window *width* *height*
	      :title-caption "Sudoku" :icon-caption "Sudoku")
      
      ;add the buttons for each spot
      (loop for ix from 1 upto 9
        do (loop for iy from 1 upto 9
          do (let ((sx (* ix *bsize*)) (sy (* iy *bsize*)) (tx (1- ix)) (ty (1- iy)) (count 0))
              (sudoku-button (sx sy *bsize* *bsize* count)
                (when (and (and (> x sx) (< x (+ sx *bsize*)))
                         (and (> y sy) (< y (+ sy *bsize*))))
                  ;(setf bcolor (sdl:color :r (random 255) :g (random 255) :b (random 255)))
                  (s-incf count)
                  ;later pull out to get next-possible
                  (setf (aref board tx ty) count))))))
      
   		(sdl:with-events ()
        (:quit-event () t)
		  	(:key-down-event (:key key)
          (when (eq key :sdl-key-escape)
		       (sdl:push-quit-event)))
     		(:mouse-button-down-event (:button button :state state :x x :y y)
          (loop for funct in button-events
            do (funcall funct button state x y)))
	    	(:idle () 
	    		(sdl:clear-display sdl:*black*)
					(loop for funct in draws
            do (funcall funct))
   			  (sdl:update-display))))))