#
# Makefile for the Links Programming Language.
#

# This file contains instructions for building a bleeding-edge version
# of Links in release or development mode with and without database
# support.

# LATEST STABLE RELEASE
# =====================
# To install the latest stable release of Links please see the INSTALL
# file.

# To build the bleeding-edge version of Links please refer to the
# instructions below.

# DEVELOPMENT
# ===========
# By default the build rule `all' attempts to build the latest
# development version of Links with database support, thus the
# following command suffices to build everything
# $ make all
# For legacy reasons the rule `nc' (native compile) is an alias of
# `all'.
# NOTE: The rule `all' may skip the building the database drivers if
# the necessary system prerequisites are absent.

# The following command builds Links in development mode without
# database support
# $ make no-db

# RELEASE
# =======
# To build Links in release mode use the following command
# $ make all-release
# This rule builds every artefact which is part of a release. As such,
# it will include all the database drivers. Currently, there is no
# support for building Links without database support in release mode.


# Project root and build directory
ROOT:=$(shell dirname $(firstword $(MAKEFILE_LIST)))
BUILD_DIR:=$(ROOT)/_build

# The build command and some standard build system flags
BUILD=opam exec dune -- build
MAIN=links
SOURCES=$(MAIN)
DB_STABLE=links-postgresql,links-sqlite3,links-mysql8
DB_SOURCES=$(DB_STABLE)
# Note: this relies on lazy expansion of `SOURCES'.
COMMON_FLAGS=--only-packages $(SOURCES) --build-dir=$(BUILD_DIR)
DEV_FLAGS=$(COMMON_FLAGS) --profile=dev
REL_FLAGS=$(COMMON_FLAGS) --profile=release
CI_FLAGS=$(COMMON_FLAGS) --profile=ci
# List of packages that we currently release
RELEASE_PKGS:=$(MAIN),$(DB_STABLE)

# Build rules.

# The default is to build everything in development mode.
.DEFAULT_GOAL:= all
.PHONY: all
all: build-dev-all create-startup-script

# Builds everything in release mode.
.PHONY: all-release
all-release: build-release-all create-startup-script

# Builds everything in continuous integration mode.
.PHONY: all-ci
all-ci: build-ci-all create-startup-script

# Legacy rule to remain backwards compatible with the OCamlMakefile
# interface.
.PHONY: nc
nc: all

# Builds core Links (without database support) in development mode.
.PHONY: no-db
no-db:	build-dev-nodb create-startup-script

# Creates a thin shell script, which executes the built Links
# executable in a pre-configured environment.
ABS_BUILD_DIR=$(BUILD_DIR)
.PHONY: create-startup-script
create-startup-script:
	@echo "#!/usr/bin/env sh" > links
	@# This path resolution scheme may depend on BASH semantics.
	$(eval ABS_BUILD_DIR=$(shell cd $(BUILD_DIR) ; pwd -P))
	@# This is an attempt to put in a safe guard
	@# If the above resolution fails to yield an existing path then fall back to the relative path in BUILD_DIR.
	$(eval ABS_BUILD_DIR:=$(shell test -d $(ABS_BUILD_DIR) && echo "$(ABS_BUILD_DIR)" || echo "$(BUILD_DIR)"))
	@echo "$(ABS_BUILD_DIR)/default/bin/links.exe \"\$$@\"" >> links
	@chmod +x links
	ln -fs links linx

# Invokes `dune' to build everything in continuous integration mode.
.PHONY: build-ci-all
build-ci-all: dune dune-project include-db-sources
	$(BUILD) $(CI_FLAGS) @install

# Invokes `dune' to build everything in development mode.
.PHONY: build-dev-all
build-dev-all: dune dune-project include-db-sources
	$(BUILD) $(DEV_FLAGS) @install

# Invokes `dune' to build only core Links in development mode.
.PHONY: build-dev-nodb
build-dev-nodb: dune dune-project
	$(BUILD) $(DEV_FLAGS) @install

# Invokes `dune' to build everything in release mode.
.PHONY: build-release-all
build-release-all: dune dune-project include-db-sources
	$(BUILD) $(REL_FLAGS) @install


# This rule updates the contents of the `SOURCES' variable to include
# the database sources.
.PHONY: include-db-sources
include-db-sources:
	$(eval SOURCES:=$(SOURCES),$(DB_SOURCES))

# Auxiliary rule used by `opam install` to build Links
.PHONY: opam-build-links.opam
opam-build-links.opam: links.opam
	$(shell LINKS_BUILT_BY_OPAM=1 opam exec dune -- build -p links)

# Runs the test suite. We reset the variables CAMLRUNPARAM and
# OCAMLRUNPARAM, because some of the tests are sensitive to the
# printing of stack traces.
.PHONY: tests
tests: links
	$(eval OCAMLRUNPARAM="")
	$(eval CAMLRUNPARAM="")
	./run-tests

# Runs the database test suite. This calls run-database-tests
# with the --local flag ensuring that a local config file is used
# if available.
.PHONY: tests
database-tests: links
	./run-tests database
	./run-tests shredding
	./run-tests relational-lenses

# Cleans the project directory.
.PHONY: clean
clean:
	opam exec dune -- clean
	rm -rf *.install
	rm -rf links linx
	rm -rf doc/_build

.PHONY: clean-doc
clean-doc:
	rm -rf doc/_build

# Applies some ad-hoc checks on the source code.
.PHONY: rule-check
rule-check: tools/rule-check
	@echo "Applying rule check"
	@tools/rule-check

doc/_build/html:
	make -C doc html

.PHONY: doc
doc : doc/_build/html

.PHONY: open-doc
open-doc: doc/_build/html
	xdg-open doc/_build/html/index.html

# Release Links
opam-release:
	opam exec dune-release -- tag
	opam exec dune-release -- distrib -p $(RELEASE_PKGS)
	opam exec dune-release -- publish -p $(RELEASE_PKGS) distrib
	opam exec dune-release -- opam -p $(RELEASE_PKGS) pkg
	opam exec dune-release -- opam -p $(RELEASE_PKGS) submit
