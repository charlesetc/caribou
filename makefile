run: build
	@dune exec --no-print-directory ./bin/caribou_example.exe

build:
	@dune build --no-print-directory ./bin/caribou_example.exe
