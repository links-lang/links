-include ./Makefile.config

OCAMLMAKEFILE = ./OCamlMakefile

COMPILER_LIBS=compiler-libs.bytecomp compiler-libs.optcomp

PACKS=bigarray num str deriving.syntax deriving.syntax.classes deriving.runtime lwt lwt.syntax lwt.unix oUnit quickcheck $(COMPILER_LIBS)
export OCAMLFLAGS=-syntax camlp4o

#POSTGRESQL_LIBDIR=$(HOME)/.opam/4.02.3/lib/postgresql

PATH := $(PATH):deriving

ifdef SQLITE_LIBDIR
   DB_CODE    += lite_database.ml
   DB_AUXLIBS += $(SQLITE_LIBDIR)
   DB_CLIBS   += sqlite_stubs sqlite
   DB_LIBS    += sqlite
endif

ifdef SQLITE3_LIBDIR
   DB_CODE    += lite3_database.ml
   DB_AUXLIBS += $(SQLITE3_LIBDIR)
   DB_CLIBS   += sqlite3
   DB_LIBS    += sqlite3
endif

ifdef MYSQL_LIBDIR
   DB_CODE    += mysql_database.ml
   DB_AUXLIBS += $(MYSQL_LIBDIR)
   DB_LIBS    += mysql
endif

ifdef POSTGRESQL_LIBDIR
   DB_CODE    += pg_database.ml
   DB_AUXLIBS += $(POSTGRESQL_LIBDIR)
   DB_LIBS    += postgresql
   THREADS = yes
endif

ifdef MONETDB5_LIBDIR
	DB_CODE    += m5_database.ml
	DB_AUXLIBS += $(MONETDB5_LIBDIR)
	DB_LIBS    += mapi
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


BACKEND=comp/gather.ml comp/irtolambda.ml comp/compileir.ml

SOURCE_FILES = $(OPC)                                \
          notfound.ml                           \
          utility.ml                            \
          env_links.mli env_links.ml                        \
          settings.mli settings.ml              \
          basicsettings.ml                      \
          debug.mli debug.ml                    \
          performance.mli performance.ml        \
          graph.ml                              \
          types_links.mli types_links.ml                    \
          constant.ml                           \
          sourceCode.ml                         \
          regex.ml                              \
          sugartypes.ml                         \
          parser_links.mly                            \
          lexer_links.mli lexer_links.mll                   \
          typeUtils.mli typeUtils.ml            \
          errors.mli errors.ml                  \
          instantiate.mli instantiate.ml        \
          generalise.mli generalise.ml          \
          typevarcheck.mli typevarcheck.ml      \
          unify.mli unify.ml                    \
          var.ml                                \
          ir.mli ir.ml                          \
	  handlerUtils.ml                       \
          tables.ml                             \
          closures_links.ml                           \
          parse_links.mli parse_links.ml                    \
          sugarTraversals.mli  sugarTraversals.ml       \
          desugarDatatypes.mli desugarDatatypes.ml      \
          defaultAliases.ml                     \
          value.mli value.ml                    \
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
	  desugarHandlers.mli desugarHandlers.ml         \
          typeSugar.mli typeSugar.ml                     \
          checkXmlQuasiquotes.ml                \
          frontend.ml                           \
          dumpTypes.ml                          \
          compilePatterns.ml                    \
          jsonparse.mly                         \
          jsonlex.mll                           \
          js.ml                                 \
          json.ml                               \
          database.mli database.ml              \
          linksregex.ml                         \
          proc_links.mli proc_links.ml                      \
          lib.mli lib.ml                        \
          sugartoir.mli sugartoir.ml            \
          loader.mli loader.ml                  \
          $(DB_CODE)                            \
          irtojs.mli irtojs.ml                  \
          query.ml                              \
          evalir.ml                             \
          buildTables.ml                        \
          webif.mli webif.ml                    \
          $(BACKEND)                    

# TODO: get these working again
#
#          test.ml                               \
#          tests.ml                              \


# Set-up the testsuite as a subproject s.t.
# $ make testsuite
# generates a binary "testsuite" which runs the tests.
define PROJ_testsuite
  SOURCES=$(SOURCE_FILES) \
          testsuite.ml
  RESULT="testsuite"
endef
export PROJ_testsuite

ifndef SUBPROJS
  export SUBPROJS = testsuite
endif

# Replicate previous makefile
SOURCES=$(SOURCE_FILES) links.ml
RESULT="links"

LIBS    = $(DB_LIBS)

CLIBS 	= $(DB_CLIBS)

INCDIRS = $(AUXLIB_DIRS) $(EXTRA_INCDIRS)
LIBDIRS = $(AUXLIB_DIRS) $(EXTRA_LIBDIRS)

include $(OCAMLMAKEFILE)

testsuite:
	@make -f $(OCAMLMAKEFILE) subprojs

test-raw:
	for i in tests/*.tests; do echo $$i 1>&2; ./test-harness $$i; done

tests:
test: $(RESULT)
	@./run-tests
	@perl -MTest::Harness -e 'Test::Harness::runtests("tests/web-tests.pl")'

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

testsuite-clean:
	rm -f testsuite

byte-code: cache-clean

native-code: cache-clean

clean :: docs-clean cache-clean testsuite-clean

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
