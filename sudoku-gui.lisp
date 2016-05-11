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

(defmacro add-button ((ix iy l h text) &body body)
  `(let ((bcolor *white*) (scolor *black*) (tcolor *black*))
    (add-draw (lambda () 
                (new-draw-string-solid-*
                  ,text (+ 13 ,ix) (+ 6 ,iy) :justify :center :color tcolor :font *default-font*)))
    (add-draw (lambda () (draw-box-* ,ix ,iy ,l ,h :color bcolor :stroke-color scolor)))
    (setf button-events (cons (lambda (button state x y) ,@body) button-events))))

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
          do (let ((sx (* ix *bsize*)) (sy (* iy *bsize*)) (tx (1- ix)) (ty (1- iy)) (spot 0))
              (add-button (sx sy *bsize* *bsize* (if (= 0 spot) "" (write-to-string spot)))
                (when (and (and (> x sx) (< x (+ sx *bsize*)))
                         (and (> y sy) (< y (+ sy *bsize*))))
                  ;(setf bcolor (sdl:color :r (random 255) :g (random 255) :b (random 255)))
                  (s-incf spot)
                  ;later pull out to get next-possible
                  (setf (aref board tx ty) spot))))))
      
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