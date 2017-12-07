.PHONY: nc native clean tests install uninstall clean
.DEFAULT_GOAL: nc

nc:
	jbuilder build @install
	cp _build/default/bin/links.exe links

native: nc

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean
	rm -rf *.install



# nc: native

# links: native

# tests: links
# 	@OCAMLRUNPARAM="" ./run-tests

# clean: jbuild

# links-postgres: links-postgres.opam
# 	jbuilder build -j 4 @install -p links-postgres

# links-lib: links-lib.opam
# 	jbuilder build -j 4 @install -p links-lib

# native: links.opam
# 	jbuilder build -j 4 @install -p links-lib,links
# 	cp _build/default/bin/links.exe ./links

# tests: links
# 	@OCAMLRUNPARAM="" ./run-tests

# clean: links.opam
# 	jbuilder clean
# 	rm -rf ./links

# .PHONY: native nc clean
