
# Executing

simple: build_simple
	@dune exec --no-print-directory ./examples/simple.exe

files: build_files
	@dune exec --no-print-directory ./examples/files.exe



# Building

build_simple:
	@dune build --no-print-directory ./examples/simple.exe

build_files:
	@dune build --no-print-directory ./examples/files.exe
