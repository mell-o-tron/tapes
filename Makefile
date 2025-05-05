# .PHONY: compile
# compile:
# 	mkdir -p build
# 	rm -f build/*
# 	(opam exec -- dune test --force) 2> build/out.asm

.PHONY: build
build:
	opam exec dune build

.PHONY: clean
clean:
	rm -rf build
	opam exec dune clean

.PHONY: deps
deps:
	opam install ppx_deriving
	opam install sedlex
	opam install menhir

	
run: build
	opam exec -- dune build --profile=dev

	OCAMLRUNPARAM=b ./_build/default/bin/main.exe ./formula
# 	cat ./tikz_out/prologue ./figure.txt ./tikz_out/epilogue > ./tikz_out/result.tex
# 	rm ./figure.txt
# 	pdflatex ./tikz_out/result.tex
# 	rm ./result.aux
# 	rm ./result.log
	
