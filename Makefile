.PHONY: nc native clean tests install uninstall clean
.DEFAULT_GOAL: nc

nc:
	jbuilder build @install
	@echo "#!/bin/sh" > links
	@echo "LINKS_LIB=\"_build/*/bin\" LINKS_LD_LIBRARY_PATH=\"_build/*/pg-driver\" jbuilder exec linx -- \$$@" >> links
	@chmod +x links
	ln -f -s links linx

native: nc

install:
	jbuilder install

uninstall:
	jbuilder uninstall

tests: links
	@OCAMLRUNPARAM="" ./run-tests

clean:
	jbuilder clean
	rm -rf *.install
	rm -rf links linx
