 (let* ((*standard-output* (make-broadcast-stream)) (*error-output* *standard-output*))
	(ql:quickload "lispbuilder-sdl")
)

;;rendering
(defun draw ()
	(sdl:with-init ()
		(sdl:window 1024 768 :title-caption "Game of Life")
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

;;main
(defun main(args)
	(if (null args) (print-usage))
	(princ (first args))
	(if	(or (string= "-h" (first args)) (string= "--help" (first args)))
		(print-usage)
	)
	(draw)
)

;;maintaining error
(sb-int:with-float-traps-masked (:invalid :inexact :overflow)
	(main (rest *posix-argv*))
)
