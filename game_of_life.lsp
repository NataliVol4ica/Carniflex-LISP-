 (let* ((*standard-output* (make-broadcast-stream)) (*error-output* *standard-output*))
	(ql:quickload "lispbuilder-sdl")
)

(defvar *window-w* 1024)
(defvar *window-h* 768)

(defvar *cur-cellsize* 20)
(defvar *grid-width*)
(defvar *grid-height*)
(defvar *field-x* 0) ;move the upper left corner of grid
(defvar *field-y* 0)

(defun draw_grid ()
	(loop for i from 0 to *grid-width* by 1 do
		(sdl:draw-line-* (* *cur-cellsize* i)
					0
					(* *cur-cellsize* i)
					(* *cur-cellsize* *grid-height*)
					:color (sdl:color :r 200 :g 200 :b 200))
	)
	(loop for i from 0 to *grid-height* by 1 do
		(sdl:draw-line-* 0
					(* *cur-cellsize* i)
					(* *cur-cellsize* *grid-width*)
					(* *cur-cellsize* i)
					:color (sdl:color :r 200 :g 200 :b 200))
	)
)

;;rendering
(defun render ()
	(sdl:clear-display (sdl:color))
	(draw_grid)
	(sdl:update-display)
)

;;main thread
(defun draw ()
	(sdl:with-init ()
		(sdl:window *window-w* *window-h* :title-caption "Game of Life")
		(sdl:with-events ()
			(:quit-event ()
				(format t "~%Quitting the program.~%")
				(exit)
				T
			)
			(:key-down-event (:key key)
				(case key
					(:sdl-key-q (princ "pressed Q"))
					(:sdl-key-escape (sdl:push-quit-event))
				)
			)
			(:idle ()
				(render)
			)
		)

	)
)

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
