.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f final.zip
	zip -r final.zip . -x@exclude.lst

clean:
	dune clean
	rm -f final.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

loc:
	dune clean
	cloc --by-file --include-lang=OCaml .
	dune build

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage