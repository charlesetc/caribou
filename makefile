# Executing

# comment or uncomment to choose start
first: files
# first: directories


simple: build_simple
	@dune exec --no-print-directory ./examples/simple.exe

files: build_files
	@dune exec --no-print-directory ./examples/files.exe

directories: build_directories
	@dune exec --no-print-directory ./examples/directories.exe



# Building

build_simple:
	@dune build --no-print-directory ./examples/simple.exe

build_files:
	@dune build --no-print-directory ./examples/files.exe

build_directories:
	@dune build --no-print-directory ./examples/directories.exe
