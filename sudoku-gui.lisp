(in-package :sudoku-play)

(defparameter *bsize* 50)
(defparameter *width* (* 11 *bsize*))
(defparameter *height* (+ 15 (* 11 *bsize*)))

(defun *empty* ()
  "an empty sudoku board"
  (make-array '(9 9) :initial-contents 
              (loop repeat 9 collect (loop repeat 9 collect 0))))

(defmacro alias ((name thing) &body body)
  "makes name a function that returns thing, and is setf-able"
  `(flet 
       ((,name () ,thing)
        ((setf ,name) (val) (setf ,thing val)))
     ,@body))

(defmacro new-draw-string-solid-* (string &rest rest)
  "safely draws strings, replacing empty strings with a space"
  (let ((mstr string))
    `(if (string= "" ,mstr)
         (draw-string-solid-* " " ,@rest)
         (draw-string-solid-* ,mstr ,@rest))))

(defmacro add-button ((ix iy l h text 
                      &key (button 'button) (state 'state) (x 'x) (y 'y)) 
                      &body body)
  "adds the necessary parts for a button to draws and buttons, 
including a background, text, and the button event"
  `(let ((bcolor *white*) (scolor *black*) (tcolor *black*)
         (px ,ix) (py ,iy) (pl ,l) (ph ,h))
     (push (lambda () 
             (new-draw-string-solid-* ,text (+ 13 px) (+ 6 py) 
                                    :justify :center :color tcolor)) 
           draws)
     (push (lambda () 
             (draw-box-* px py pl ph :color bcolor :stroke-color scolor))
            draws)
     (push (lambda (,button ,state ,x ,y) 
             (when (and (> ,x px) (< ,x (+ px pl))
                        (> ,y py) (< ,y (+ py ph))) 
               ,@body))
        buttons)))

(defun run-sudoku ()
  "opens a window and runs the sudoku game"
  (let ((buttons '()) (draws '()) (board (*empty*)))
    (sdl:with-init ()
      (sdl:initialise-default-font sdl:*ttf-font-vera*)
      (sdl:window *width* *height* :title-caption "Sudoku")
      ;; Add the buttons for each spot.
      (dotimes (tx 9) 
        (dotimes (ty 9) 
          (let* ((x tx) (y ty)
                 (gx (+ (* 3 (truncate x 3)) (* (1+ x) *bsize*))) 
                 (gy (+ (* 3 (truncate y 3)) (* (1+ y) *bsize*) 15)))
            (alias (spot (aref board x y))
              (add-button (gx gy *bsize* *bsize* 
                            (if (zerop (spot)) "" (write-to-string (spot)))
                            :x _gx :y _gy)
                (when (eq state sdl-cffi::sdl-pressed)
                  (cond 
                    ((eq button sdl-button-left)
                     (setf (spot) (next-possible board x y)))
                    ((eq button sdl-button-right)
                     (setf (spot) 0))
                    (t nil))
                  (validate board)))))))
      ;; Add the top buttons.
      ;; solve button
      (add-button (*bsize* 1 (* 3 *bsize*) *bsize* "solve")
        (when (eq state sdl-cffi::sdl-pressed)
          (cond 
            ((eq button sdl-button-left)
             (solve-dfs board))
            (t nil))))
      ;; clear button
      (add-button ((+ 3 (* 4 *bsize*)) 1 (* 3 *bsize*) *bsize* "clear")
        (when (eq state sdl-cffi::sdl-pressed)
          (cond 
            ((eq button sdl-button-left)
             (setf board (*empty*)))
            (t nil))))
      ;; quit button
      (add-button ((+ 6 (* 7 *bsize*)) 1 (* 3 *bsize*) *bsize* "quit")
        (when (eq state sdl-cffi::sdl-pressed)
          (cond 
            ((eq button sdl-button-left)
             (sdl:push-quit-event))
            (t nil))))
      ;; Run the game.
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event
         (:key key)
         (when (eq key :sdl-key-escape)
           (sdl:push-quit-event)))
        (:mouse-button-down-event
         (:button mouse-button :state state :x x :y y)
         (loop for button in buttons do
               (funcall button mouse-button state x y)))
        (:mouse-button-up-event
         (:button mouse-button :state state :x x :y y)
         (loop for button in buttons do
               (funcall button mouse-button state x y)))
        (:idle
         () 
         (sdl:clear-display sdl:*black*)
         (loop for draw in draws do
               (funcall draw))
         (sdl:update-display))))))
