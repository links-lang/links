.PHONY: nc native clean tests install uninstall clean
.DEFAULT_GOAL: nc

nc:
	jbuilder build -j 4 @install
	@echo "#!/bin/sh" > links
	@echo "LINKS_LIB=\"$(shell pwd)/_build/default/lib\" LINKS_LD_LIBRARY_PATH=\"$(shell pwd)/_build/default/pg-driver\" $(shell pwd)/_build/default/bin/links.exe \"\$$@\"" >> links
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
