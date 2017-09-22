.DEFAULT_GOAL: native

nc: native

links: native

native: links.opam
	jbuilder build -j 4 @install
	cp _build/default/bin/links.exe ./links

tests: links
	@OCAMLRUNPARAM="" ./run-tests

clean: links.opam
	jbuilder clean
	rm -rf ./links

.PHONY: native nc clean
