(in-package :sudoku-play)

(defparameter *bsize* 50)
(defparameter *width* (* 11 *bsize*))
(defparameter *height* (* 11 *bsize*))
(defun *empty* ()
  (make-array '(9 9) :initial-contents 
    (loop repeat 9 collect (loop repeat 9 collect 0))))

(defmacro alias ((name thing) &rest body)
  "makes name a function that returns thing, and is setf-able"
  `(flet 
     ((,name () ,thing)
      ((setf ,name) (val) (setf ,thing val)))
     ,@body))

(defmacro new-draw-string-solid-* (string &rest rest)
  "safely draws strings, replacing empties with a space"
  (let ((mstr string))
    `(if (string= "" ,mstr)
        (draw-string-solid-* " " ,@rest)
        (draw-string-solid-* ,mstr ,@rest))))

(defmacro add-draw (draw)
  "adds to the draws list"
  `(setf draws (cons (lambda () ,draw) draws)))

(defmacro add-button ((ix iy l h text) &body body)
  "adds the necessary parts for a button, including a background, text, and the button event"
  `(let ((bcolor *white*) (scolor *black*) (tcolor *black*))
    (add-draw (new-draw-string-solid-* ,text (+ 13 ,ix) (+ 6 ,iy) :justify :center :color tcolor))
    (add-draw (draw-box-* ,ix ,iy ,l ,h :color bcolor :stroke-color scolor))
    (setf buttons (cons (lambda (button state x y) 
        (when (and (> x ,ix) (< x (+ ,ix ,l))
                   (> y ,iy) (< y (+ ,iy ,h))) 
          ,@body))
      buttons))))

(defun run-sudoku ()
  "runs the program"
  ;basic data
  (let ((buttons '()) (draws '()) (board (*empty*)))
    (sdl:with-init ()
      (sdl:initialise-default-font sdl:*ttf-font-vera*)
     	(sdl:window *width* *height*
	      :title-caption "Sudoku" :icon-caption "Sudoku")
      
      ;add the buttons for each spot
      (loop for ix from 1 upto 9
        do (loop for iy from 1 upto 9
          do (let ((sx (* ix *bsize*)) (sy (* iy *bsize*)) (tx (1- ix)) (ty (1- iy)))
              (alias (spot (aref board tx ty))
                (add-button (sx sy *bsize* *bsize* (if (= 0 (spot)) "" (write-to-string (spot))))
                  (when (eq state sdl-cffi::sdl-pressed)
                    (cond 
                      ((eq button sdl-button-left)
                        (setf (spot) (next-possible board tx ty)))
                      ((eq button sdl-button-right)
                       (setf (spot) 0))
                      (t nil))
                    (validate board)))))))
      
      (add-button (*bsize* 0 (* 3 *bsize*) *bsize* "solve")
        (when (eq state sdl-cffi::sdl-pressed)
          (cond 
            ((eq button sdl-button-left)
              (solve-dfs board))
            (t nil))))
      (add-button ((* 4 *bsize*) 0 (* 3 *bsize*) *bsize* "clear")
        (when (eq state sdl-cffi::sdl-pressed)
          (print board)
          (cond 
            ((eq button sdl-button-left)
              (setf board (*empty*)))
            (t nil))))
      (add-button ((* 7 *bsize*) 0 (* 3 *bsize*) *bsize* "quit")
        (when (eq state sdl-cffi::sdl-pressed)
          (cond 
            ((eq button sdl-button-left)
              (sdl:push-quit-event))
            (t nil))))
      
      (sdl:with-events ()
        (:quit-event () t)
		  	(:key-down-event (:key key)
          (when (eq key :sdl-key-escape)
		       (sdl:push-quit-event)))
     		(:mouse-button-down-event (:button mouse-button :state state :x x :y y)
          (loop for button in buttons
            do (funcall button mouse-button state x y)))
        (:mouse-button-up-event (:button mouse-button :state state :x x :y y)
          (loop for button in buttons
            do (funcall button mouse-button state x y)))
	    	(:idle () 
	    		(sdl:clear-display sdl:*black*)
					(loop for draw in draws
            do (funcall draw))
   			  (sdl:update-display))))))