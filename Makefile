HOME = /Users/nkolosov

all:
	sbcl --noinform --load game_of_life.lsp

lisp:
	sbcl

flag1:
	sbcl --noinform --load game_of_life.lsp -h

flag2:
	sbcl --noinform --load game_of_life.lsp --help

test1:
	sbcl --noinform --load game_of_life.lsp 20 40
