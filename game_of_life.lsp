(let* ((*standard-output* (make-broadcast-stream)) (*error-output* *standard-output*))
	(ql:quickload "lispbuilder-sdl")
)
;;global vars
(defvar *window-w* 1024)
(defvar *window-h* 768)

(defvar *cur-cellsize* 16)
(defvar *grid-width*)
(defvar *grid-height*)
(defvar *field-x* 0) ;move the upper left corner of grid
(defvar *field-y* 0)
(defvar *drag-mode* nil)

;;drawing grid
(defun draw_grid ()
	(loop for i from 0 to *grid-width* by 1 do
		(sdl:draw-line-* (+ (* *cur-cellsize* i) *field-x*)
					(+ 0 *field-y*)
					(+ (* *cur-cellsize* i) *field-x*)
					(+ (* *cur-cellsize* *grid-height*) *field-y*)
					:color (sdl:color :r 50 :g 50 :b 50))
	)
	(loop for i from 0 to *grid-height* by 1 do
		(sdl:draw-line-* (+ 0 *field-x*)
					(+ (* *cur-cellsize* i) *field-y*)
					(+ (* *cur-cellsize* *grid-width*) *field-x*)
					(+ (* *cur-cellsize* i) *field-y*)
					:color (sdl:color :r 50 :g 50 :b 50))
	)
)

;;rendering
(defun render ()
	(sdl:clear-display (sdl:color))
	(draw_grid)
	(sdl:update-display)
)
;;            ======== ZOOMING ========
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
	)
)
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
	)
)

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
			(:mouse-button-down-event (:button b)
				(case b
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
						;(format t "Mouse moving ~a ~a~%" x-rel y-rel)
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
	(draw)
)

;;maintaining error
(sb-int:with-float-traps-masked (:invalid :inexact :overflow)
	(main (rest *posix-argv*))
)
