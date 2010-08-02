-include ./Makefile.config

OCAMLMAKEFILE = ./OCamlMakefile

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

DERIVING_DIR=deriving-310

AUXLIB_DIRS = $(DB_AUXLIBS) $(DERIVING_DIR)/lib

ifdef PROF
OCAMLOPT := ocamlopt.opt -p -inline 0
else
OCAMLOPT := ocamlopt.opt
endif
OCAMLC := ocamlc.opt

# use ocamldep.opt if it exists
# (it doesn't exist for all OCaml installations)
OCAMLDEP := $(shell if ocamldep.opt > /dev/null 2>&1; then echo 'ocamldep.opt'; else echo 'ocamldep'; fi)

PATH := $(PATH):$(DERIVING_DIR)/syntax

#OCAMLYACC := menhir --infer --comment --explain --dump --log-grammar 1 --log-code 1 --log-automaton 2 --graph
OCAMLYACC := ocamlyacc -v

OCAMLFLAGS=-dtypes -w Ae
OCAMLDOCFLAGS=-pp deriving

# additional files to clean
TRASH=*.tmp *.output *.cache

# Other people's code.
OPC = cgi.ml netencoding.ml netencoding.mli unionfind.ml unionfind.mli \
      getopt.ml getopt.mli PP.ml

SOURCES = $(OPC)                		\
          notfound.ml				\
          utility.ml            		\
          env.mli env.ml                        \
          settings.mli settings.ml 		\
          basicsettings.ml                      \
          debug.mli debug.ml    		\
          performance.mli performance.ml	\
          graph.ml                              \
          types.mli types.ml 	                \
          constant.ml                           \
          sourceCode.ml                         \
          regex.ml                              \
          sugartypes.ml                         \
          parser.mly            		\
          lexer.mli lexer.mll         		\
	  typeUtils.mli typeUtils.ml            \
          errors.mli errors.ml                  \
          instantiate.mli instantiate.ml        \
          generalise.mli generalise.ml          \
          typevarcheck.mli typevarcheck.ml      \
          unify.mli unify.ml                    \
          var.ml                                \
          ir.mli ir.ml                          \
          parse.mli parse.ml    		\
          sugarTraversals.mli  sugarTraversals.ml	\
          desugarDatatypes.mli desugarDatatypes.ml      \
          defaultAliases.ml                     \
          value.mli value.ml                    \
          xmlParser.mly xmlLexer.mll            \
          parseXml.mli parseXml.ml              \
	  resolvePositions.mli resolvePositions.ml	\
          refineBindings.mli refineBindings.ml		\
          desugarLAttributes.mli desugarLAttributes.ml	\
          transformSugar.mli transformSugar.ml          \
          desugarPages.mli desugarPages.ml		\
          desugarFormlets.mli desugarFormlets.ml        \
          desugarRegexes.mli desugarRegexes.ml		\
          desugarFors.mli desugarFors.ml                \
          desugarDbs.mli desugarDbs.ml                  \
          desugarFuns.mli desugarFuns.ml                \
          desugarProcesses.mli desugarProcesses.ml      \
          desugarInners.mli desugarInners.ml            \
          typeSugar.mli typeSugar.ml			\
          frontend.ml                           \
	  dumpTypes.ml                          \
          compilePatterns.ml                    \
          jsonparse.mly                         \
          jsonlex.mll           		\
          js.ml                                 \
          json.ml                               \
          database.mli database.ml 		\
          linksregex.ml                         \
          proc.mli proc.ml                      \
          lib.mli lib.ml                        \
          sugartoir.mli sugartoir.ml            \
          loader.mli loader.ml                  \
          $(DB_CODE)            		\
          irtojs.mli irtojs.ml                  \
          query.ml                              \
          evalir.ml                             \
          webif.mli webif.ml                    \
          links.ml              		\

# TODO: get these working again
#
#          test.ml                               \
#          tests.ml                              \


LIBS    = nums str $(DB_LIBS) deriving

ifndef THREADS
LIBS += unix
endif

RESULT  = links
CLIBS 	= $(DB_CLIBS)

INCDIRS = $(AUXLIB_DIRS) $(EXTRA_INCDIRS)
LIBDIRS = $(AUXLIB_DIRS) $(EXTRA_LIBDIRS)

PRE_TARGETS = $(DERIVING_DIR)/built

include $(OCAMLMAKEFILE)

.PHONY : $(DERIVING_DIR)/built
$(DERIVING_DIR)/built:
	cd $(DERIVING_DIR) && make

deriving-clean:
	cd $(DERIVING_DIR) && make clean

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

byte-code: cache-clean

native-code: cache-clean

clean :: deriving-clean docs-clean cache-clean


