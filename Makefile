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

hoare: build
	opam exec -- dune build --profile=dev

	OCAMLRUNPARAM=b ./_build/default/bin/main.exe ./hoare.tapes
	
run: build
	opam exec -- dune build --profile=dev
	OCAMLRUNPARAM=b ./_build/default/bin/main.exe ./formula.tapes

profile: build
	timeout 30s perf record -F 99 -g --call-graph dwarf _build/default/bin/main.exe hoare.tapes
	perf script -F +pid > test.perf
