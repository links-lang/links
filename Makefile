# Project root and build directory
ROOT:=$(shell pwd)
BUILD_DIR:=$(ROOT)/_build
# The build command and some standard build system flags
BUILD=dune build
FLAGS=--build-dir=$(BUILD_DIR) --profile=development

.PHONY: build-dev-nodb build-dev nc native clean tests install uninstall clean
.DEFAULT_GOAL: nc

nc: build-dev-all create-startup-script
nc-release: build-release-all create-startup-script

native: nc
all: nc

no-db:	build-dev-nodb create-startup-script

create-startup-script:
	@echo "#!/bin/sh" > links
	@echo "LINKS_LIB=\"$(BUILD_DIR)/default/lib\" $(BUILD_DIR)/default/bin/links.exe \"\$$@\"" >> links
	@chmod +x links
	ln -fs links linx

build-dev-all: dune dune-project
	$(BUILD) --only-packages links,links-postgresql $(FLAGS) @install

build-dev-nodb: dune dune-project
	$(BUILD) --only-packages links $(FLAGS) @install

build-release-all:
	$(BUILD) -p links,links-postgresql @install

install:
	dune install

uninstall:
	dune uninstall

tests: links
	@OCAMLRUNPARAM="" ./run-tests

clean:
	dune clean
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
