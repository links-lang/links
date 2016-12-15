-include ./Makefile.config

OCAMLMAKEFILE = ./OCamlMakefile

PACKS=str deriving.syntax deriving.syntax.classes deriving.runtime lwt lwt.syntax lwt.unix
export OCAMLFLAGS=-syntax camlp4o

PATH := $(PATH):deriving

POSTGRESQL_LIBDIR=$(shell ocamlfind query postgresql)
SQLITE3_LIBDIR=$(shell ocamlfind query sqlite3)
MYSQL_LIBDIR=$(shell ocamlfind query mysql)

ifneq ($(SQLITE3_LIBDIR),)
   DB_CODE    += lite3_database.ml
   DB_AUXLIBS += $(SQLITE3_LIBDIR)
   DB_CLIBS   += sqlite3
   DB_LIBS    += sqlite3
endif

ifneq ($(MYSQL_LIBDIR),)
   DB_CODE    += mysql_database.ml
   DB_AUXLIBS += $(MYSQL_LIBDIR)
   DB_LIBS    += mysql
endif

ifneq ($(POSTGRESQL_LIBDIR),)
   DB_CODE    += pg_database.ml
   DB_AUXLIBS += $(POSTGRESQL_LIBDIR)
   DB_LIBS    += postgresql
   THREADS = yes
endif

AUXLIB_DIRS = $(DB_AUXLIBS)

ifdef PROF
OCAMLOPT := ocamlopt -p -inline 0
endif

#OCAMLYACC := menhir --infer --comment --explain --dump --log-grammar 1 --log-code 1 --log-automaton 2 --graph
OCAMLYACC := ocamlyacc -v

OCAMLFLAGS=-dtypes -w Ae-44-45 -g -cclib -lunix
#OCAMLDOCFLAGS=-pp deriving

# additional files to clean
TRASH=*.tmp *.output *.cache

# Other people's code.
OPC = cgi.ml netencoding.ml netencoding.mli unionfind.ml unionfind.mli \
      getopt.ml getopt.mli PP.ml unix.cma

SOURCES = $(OPC)                                \
          notfound.ml                           \
          utility.ml                            \
          env.mli env.ml                        \
          settings.mli settings.ml              \
          basicsettings.ml                      \
          debug.mli debug.ml                    \
          performance.mli performance.ml        \
          graph.ml                              \
          types.mli types.ml                    \
          constant.ml                           \
          sourceCode.ml                         \
          regex.ml                              \
          sugartypes.ml                         \
          parser.mly                            \
          lexer.mli lexer.mll                   \
          typeUtils.mli typeUtils.ml            \
          errors.mli errors.ml                  \
          instantiate.mli instantiate.ml        \
          generalise.mli generalise.ml          \
          typevarcheck.mli typevarcheck.ml      \
          unify.mli unify.ml                    \
          var.ml                                \
          ir.mli ir.ml                          \
          tables.ml                             \
          closures.ml                           \
          parse.mli parse.ml                    \
          sugarTraversals.mli  sugarTraversals.ml       \
					moduleUtils.mli moduleUtils.ml \
					chaser.mli chaser.ml \
					desugarModules.mli desugarModules.ml \
          desugarDatatypes.mli desugarDatatypes.ml      \
          defaultAliases.ml                     \
          value.mli value.ml                    \
          eventHandlers.mli eventHandlers.ml    \
          xmlParser.mly xmlLexer.mll            \
          parseXml.mli parseXml.ml              \
          resolvePositions.mli resolvePositions.ml       \
          refineBindings.mli refineBindings.ml           \
          desugarLAttributes.mli desugarLAttributes.ml   \
          transformSugar.mli transformSugar.ml           \
          fixTypeAbstractions.mli fixTypeAbstractions.ml \
          desugarPages.mli desugarPages.ml               \
          desugarFormlets.mli desugarFormlets.ml         \
          desugarRegexes.mli desugarRegexes.ml           \
          desugarFors.mli desugarFors.ml                 \
          desugarDbs.mli desugarDbs.ml                   \
          desugarFuns.mli desugarFuns.ml                 \
          desugarProcesses.mli desugarProcesses.ml       \
          desugarInners.mli desugarInners.ml             \
	  desugarCP.mli desugarCP.ml                     \
          typeSugar.mli typeSugar.ml                     \
          checkXmlQuasiquotes.ml                \
          frontend.ml                           \
          dumpTypes.ml                          \
          compilePatterns.ml                    \
          proc.mli proc.ml                      \
          jsonparse.mly                         \
          jsonlex.mll                           \
          js.ml                                 \
          json.ml                               \
          database.mli database.ml              \
          linksregex.ml                         \
          lib.mli lib.ml                        \
          sugartoir.mli sugartoir.ml            \
          loader.mli loader.ml                  \
          $(DB_CODE)                            \
          irtojs.mli irtojs.ml                  \
          query.mli query.ml                              \
          queryshredding.ml                     \
          evalir.ml                             \
          buildTables.ml                        \
          webif.mli webif.ml                    \
          links.ml                              \

# TODO: get these working again
#
#          test.ml                               \
#          tests.ml                              \


LIBS    = $(DB_LIBS)

RESULT  = links
CLIBS 	= $(DB_CLIBS)

INCDIRS = $(AUXLIB_DIRS) $(EXTRA_INCDIRS)
LIBDIRS = $(AUXLIB_DIRS) $(EXTRA_LIBDIRS)

include $(OCAMLMAKEFILE)

.PHONY: tests
tests: $(RESULT)
	@./run-tests

fixmes:
	@grep FIXME *.ml *.mli *.mly *.mll

.PHONY: docs docs-clean clean

docs:
	cd doc && make

quick-help:
	cd doc && make quick-help.html

docs-clean:
	cd doc && make clean

prelude.links.cache: prelude.links links
	@echo "Pre-compiling prelude..."
	@./links -e 'print("Prelude compiled OK.")'

cache-clean:
	-rm -f prelude.links.cache

byte-code: cache-clean

native-code: cache-clean

clean :: docs-clean cache-clean

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
