(in-package :sudoku-play)

(defparameter *bsize* 50)
(defparameter *width* (* 11 *bsize*))
(defparameter *height* (* 11 *bsize*))

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

(defmacro add-draw (funct)
  "adds to the draws list"
  `(setf draws (cons ,funct draws)))

(defmacro add-button ((ix iy l h text) &body body)
  "adds the necessary parts for a button, including a background, text, and the button event"
  `(let ((bcolor *white*) (scolor *black*) (tcolor *black*))
    (add-draw (lambda () 
                (new-draw-string-solid-*
                  ,text (+ 13 ,ix) (+ 6 ,iy) :justify :center :color tcolor :font *default-font*)))
    (add-draw (lambda () (draw-box-* ,ix ,iy ,l ,h :color bcolor :stroke-color scolor)))
    (setf button-events (cons (lambda (button state x y) ,@body) button-events))))

(defun run-sudoku ()
  "runs the program"
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
          do (let ((sx (* ix *bsize*)) (sy (* iy *bsize*)) (tx (1- ix)) (ty (1- iy)))
              (alias (spot (aref board tx ty))
                (add-button (sx sy *bsize* *bsize* (if (= 0 (spot)) "" (write-to-string (spot))))
                  (when (and (> x sx) (< x (+ sx *bsize*))
                          (> y sy) (< y (+ sy *bsize*)))
                    (cond 
                      ((eq button sdl-button-left)
                        (setf (spot) (next-possible board tx ty)))
                      ((eq button sdl-button-right)
                       (setf (spot) 0))
                      (t nil))
                    (validate board)))))))
      
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