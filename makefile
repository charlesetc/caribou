run: build
	dune exec ./src/caribou.exe

build:
	dune build ./src/caribou.exe
