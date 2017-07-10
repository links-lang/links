-include ./Makefile.config

LINKSBASEMAKEFILE = ./Makefile.shared
include $(LINKSBASEMAKEFILE)

SOURCES :=	$(SOURCES)		\
		   	links.ml		\

RESULT  = links

include $(OCAMLMAKEFILE)

.PHONY: tests
tests: $(RESULT)
	@OCAMLRUNPARAM="" ./run-tests

fixmes:
	@grep FIXME *.ml *.mli *.mly *.mll

.PHONY: docs docs-clean clean

docs:
	cd doc && make

quick-help:
	cd doc && make quick-help.html

docs-clean:
	cd doc && make clean

unit-tests-clean:
	$(MAKE) -f Makefile.unittests clean

prelude.links.cache: prelude.links links
	@echo "Pre-compiling prelude..."
	@./links -e 'print("Prelude compiled OK.")'

cache-clean:
	-rm -f prelude.links.cache

byte-code: cache-clean

native-code: cache-clean

clean :: docs-clean cache-clean unit-tests-clean

.PHONY: install
install: nc
	@echo "Installing Links to prefix $(LINKS_PREFIX)"
	@echo "Installing Links to $(LINKS_BIN)"
	install -d $(LINKS_BIN)
	install links $(LINKS_BIN)/links
	@echo "Installing prelude.links to $(LINKS_LIB)"
	install -d $(LINKS_LIB)
	install prelude.links $(LINKS_LIB)/prelude.links
	@echo "Add environment variable binding LINKS_LIB=$(LINKS_LIB) to"
	@echo "your environment to enable calling Links from any directory."

.PHONY: uninstall
uninstall:
	@echo "Uninstalling Links from $(LINKS_PREFIX)"
	@echo "Removing $(LINKS_BIN)/links"
	rm -f $(LINKS_BIN)/links
	@echo "Removing $(LINKS_LIB)/prelude.links"
	rm -f $(LINKS_LIB)/prelude.links

.PHONY: unit-tests
unit-tests:
	-$(MAKE) -f Makefile.unittests

