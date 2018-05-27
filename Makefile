.PHONY: nc native clean tests install uninstall clean
.DEFAULT_GOAL: nc

nc:
	jbuilder build -p links -j 4 @install
	@echo "#!/bin/sh" > links
	@echo "LINKS_LIB=\"$(shell pwd)/_build/default/lib\" $(shell pwd)/_build/default/bin/links.exe \"\$$@\"" >> links
	@chmod +x links
	ln -f -s links linx

native: nc

postgresql:
        #Re-state links here because otherwise the version of links installed in
        #opam may be used, leading to a linking error when loading the driver into the
        #version of links built in the current folder
	jbuilder build -p links,links-postgresql -j 4 @install

all: nc postgresql

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

REPO=../opam-repository
PACKAGES=$(REPO)/packages

pkg-%:
	topkg opam pkg -n $*
	mkdir -p $(PACKAGES)/$*
	cp -r _build/$*.* $(PACKAGES)/$*/
	rm -f $(PACKAGES)/$*/$*.opam
	cd $(PACKAGES) && git add $*

PKGS=$(basename $(wildcard *.opam))
opam-pkg:
	$(MAKE) $(PKGS:%=pkg-%)
