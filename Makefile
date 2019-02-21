# Project root and build directory
ROOT:=$(shell pwd)
BUILD_DIR:=$(ROOT)/_build
# The build command and some standard build system flags
BUILD=dune build
COMMON_FLAGS=--build-dir=$(BUILD_DIR)
DEV_FLAGS=$(COMMON_FLAGS) --profile=development
REL_FLAGS=$(COMMON_FLAGS) --profile=release

.PHONY: build-dev-nodb build-dev clean install nc native rule-check tests uninstall
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
	$(BUILD) --only-packages links,links-postgresql $(DEV_FLAGS) @install

build-dev-nodb: dune dune-project
	$(BUILD) --only-packages links $(DEV_FLAGS) @install

build-release-all:
	$(BUILD) --only-packages links,links-postgresql $(REL_FLAGS) @install

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

rule-check: tools/rule-check
	@echo "Applying rule check"
	@tools/rule-check

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
