(let* ((*standard-output* (make-broadcast-stream)) (*error-output* *standard-output*))
	(ql:quickload "lispbuilder-sdl")
)
;;global vars
(defvar *window-w* 1024)
(defvar *window-h* 768)

(defvar *cur-cellsize* 16)
(defvar *grid-width*)
(defvar *grid-height*)
(defvar *field-x* 0) ;the upper left corner of grid
(defvar *field-y* 0)
(defvar *drag-mode* nil)
(defvar *grid*)

;;         ================== CELL CALCULATIONS =================
(defun alife-cell (x y)
	(setf (aref *grid* y x) (- 1 (aref *grid* y x)))
	(format t "~a~%" *grid*)
)

(defun create_grid ()
	(setq *grid* (make-array (list (+ *grid-height* 2) (+ *grid-width* 2))))
	(dotimes (i (+ *grid-height* 2))
		(setf (aref *grid* i 0) (- 1))
		(setf (aref *grid* i (+ 1 *grid-width*)) (- 1))
	)
	(dotimes (i (+ *grid-width* 2))
		(setf (aref *grid* 0 i) (- 1))
		(setf (aref *grid* (+ 1 *grid-height*) i) (- 1))
	)
	(dotimes (i *grid-height*)
	   (dotimes (j *grid-width*)
	      (setf (aref *grid* (+ i 1) (+ j 1)) 0)
	   )
	)
	(format t "~a~%" *grid*)
)

(defun define_cell_by_coords(xx yy)
	(let (x y)
		;(format t "fx ~a fy ~a x ~a y ~a~%" *field-x* *field-y* xx yy)
		(setq xx (- xx *field-x*))
		(setq yy (- yy *field-y*)) ;нормирование сетки до (0, 0)
		;; if x < 0 || x > grid-width * cellsize
		(setq x (truncate (/ xx *cur-cellsize*)))
		(setq y (truncate (/ yy *cur-cellsize*))) ;деление на размер клетки до индексов
		(if (< xx 0) t (incf x))
		(if (< yy 0) t (incf y)) ;бо отрицательные округляются до большего
		;(format t "Clicked on [~a ~a]~%" x y)
		(alife-cell x y)
	)
)

;;drawing grid
(defun draw_grid ()
	(loop for i from 0 to *grid-width* by 1 do
		(sdl:draw-line-* (+ (* *cur-cellsize* i) *field-x*)
					(+ 0 *field-y*)
					(+ (* *cur-cellsize* i) *field-x*)
					(+ (* *cur-cellsize* *grid-height*) *field-y*)
					:color (sdl:color :r 30 :g 30 :b 30))
	)
	(loop for i from 0 to *grid-height* by 1 do
		(sdl:draw-line-* (+ 0 *field-x*)
					(+ (* *cur-cellsize* i) *field-y*)
					(+ (* *cur-cellsize* *grid-width*) *field-x*)
					(+ (* *cur-cellsize* i) *field-y*)
					:color (sdl:color :r 30 :g 30 :b 30))
	)
)


;;rendering
(defun render ()
	(sdl:clear-display (sdl:color))
	(draw_grid)
	(sdl:update-display)
)
;;              ======== ZOOMING ========
(defun zoom-in ()
	(if (< *cur-cellsize* 256)
		(progn
			(setq *cur-cellsize* (* *cur-cellsize* 2))
			(let (half)
				(setq half (/ *window-w* 2))
				(setq *field-x* (+ (* (- *field-x* half) 2) half))
			)
			(let (half)
				(setq half (/ *window-h* 2))
				(setq *field-y* (+ (* (- *field-y* half) 2) half))
			)
		)
	))
(defun zoom-out ()
	(if (> *cur-cellsize* 2)
		(progn
			(setq *cur-cellsize* (/ *cur-cellsize* 2))
			(let (half)
				(setq half (/ *window-w* 2))
				(setq *field-x* (truncate (+ (/ (- *field-x* half) 2) half)))
			)
			(let (half)
				(setq half (/ *window-h* 2))
				(setq *field-y* (truncate (+ (/ (- *field-y* half) 2) half)))
			)
		)
	))

;;         ============== MAIN THREAD ============
(defun draw ()
	(sdl:with-init ()
		(sdl:window *window-w* *window-h* :title-caption "Game of Life")
		(setf (sdl:frame-rate) 60)
		(setf *field-x* (- (/ *window-w* 2) (/ (* *grid-width* *cur-cellsize*) 2)))
		(setf *field-y* (- (/ *window-h* 2) (/ (* *grid-height* *cur-cellsize*) 2)))
		(sdl:with-events ()
			(:quit-event ()
				(format t "~%Quitting the program.~%")
				(exit)
				T
			)
			(:mouse-button-down-event (:button b :x x :y y)
				(case b
					(1 (define_cell_by_coords x y))
					(3 (setf *drag-mode* T))
					(4 (zoom-in))
					(5 (zoom-out))						
				)
			)
			(:mouse-button-up-event (:button b)
				(if (eq b 3)
					(setf *drag-mode* nil)
				)
			)
			(:mouse-motion-event (:x-rel x-rel :y-rel y-rel)
				(if *drag-mode*
					(progn
						(if (> (* *cur-cellsize* *grid-width*) *window-w*)
							(setq *field-x* (+ *field-x* x-rel)))
						(if (> (* *cur-cellsize* *grid-height*) *window-h*)
							(setq *field-y* (+ *field-y* y-rel)))
					)
				)
			)
			(:key-down-event (:key key)
				(case key
					(:sdl-key-equals
						(zoom-in))
					(:sdl-key-minus
						(zoom-out))
					(:sdl-key-w
						(if (> (* *cur-cellsize* *grid-height*) *window-h*)
							(setq *field-y* (- *field-y* (/ *cur-cellsize* 2))))
					)
					(:sdl-key-a
						(if (> (* *cur-cellsize* *grid-width*) *window-w*)
							(setq *field-x* (- *field-x* (/ *cur-cellsize* 2))))
					)
					(:sdl-key-s
						(if (> (* *cur-cellsize* *grid-height*) *window-h*)
							(setq *field-y* (+ *field-y* (/ *cur-cellsize* 2))))
					)
					(:sdl-key-d
						(if (> (* *cur-cellsize* *grid-width*) *window-w*)
							(setq *field-x* (+ *field-x* (/ *cur-cellsize* 2))))
					)
					(:sdl-key-escape (sdl:push-quit-event))
				)
			)
			(:idle ()
				(render)
			)
		)

	)
)

;;; ======= input maintaining =======

;;usage
(defun print-usage ()
	(format t "usage: sbcl --load game_of_life.lsp [-h] width height~%~%")
	(format t "positional arguments:~%~2twidth~24twidth of the grid~%~%")
	(format t "~2theight~24theight of the grid~%~%")
	(format t "optional arguments:")
	(format t "~%~2t-h, --help~24tshow this help message and exit~%")
	(exit)
)

;;input error
(defun inp-err(fname val)
	(format t "[ERROR]Invalid ~a: \"~a\"~%" fname val)
	(exit)
)

;;main
(defun main(args)
	(if (null args) (print-usage))
	;checking for help flag
	(if	(or (string= "-h" (first args)) (string= "--help" (first args)))
		(print-usage))
	;getting width
	(setf *grid-width*
		(or (parse-integer (first args) :junk-allowed t) (or (inp-err "width" (first args)) 0))
	)
	;getting height
	(setf *grid-height*
		(or (parse-integer (first (rest args)) :junk-allowed t) (or (inp-err "height" (first (rest args))) 0))
	)
	(if (< *grid-width* 1) (inp-err "width" (first args)))
	(if (< *grid-height* 1) (inp-err "height" (first args)))
	(create_grid)
	(draw)
)

;;maintaining error
(sb-int:with-float-traps-masked (:invalid :inexact :overflow)
	(main (rest *posix-argv*))
)
