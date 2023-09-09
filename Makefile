llvm:
	llvm-as output.ll -o output.bc
	llc -march=aarch64 -filetype=obj output.bc -o output.o
	clang -v output.o -o final_executable
	