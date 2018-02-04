
all:
	@make test1

lisp:
	sbcl

flag1:
	sbcl --noinform --load game_of_life.lsp -h

flag2:
	sbcl --noinform --load game_of_life.lsp --help

test0:
	sbcl --noinform --load game_of_life.lsp

testinv1:
	sbcl --noinform --load game_of_life.lsp text 40

testinv2:
	sbcl --noinform --load game_of_life.lsp 20 text

test1:
	sbcl --noinform --load game_of_life.lsp 3 3
