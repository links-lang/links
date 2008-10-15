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
endif

DERIVING_DIR=deriving

AUXLIB_DIRS = $(DB_AUXLIBS) $(DERIVING_DIR)/lib

OCAMLOPT := ocamlopt.opt
OCAMLC := ocamlc.opt

# use ocamldep.opt if it exists
# (it doesn't exist for all OCaml installations)
OCAMLDEP := $(shell if ocamldep.opt > /dev/null 2>&1; then echo 'ocamldep.opt'; else echo 'ocamldep'; fi)

PATH := $(PATH):deriving/syntax

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
          rewrite.ml            		\
          performance.mli performance.ml	\
          graph.ml                              \
          types.mli types.ml 	                \
          constant.ml                           \
          syntaxutils.ml                        \
          sqlQuery.ml               		\
          sourceCode.ml                         \
          syntax.mli syntax.ml        		\
          sqlcompile.ml                         \
          regex.ml                              \
          result.mli result.ml         		\
          sugartypes.ml                         \
          parser.mly            		\
          lexer.mli lexer.mll         		\
          errors.mli errors.ml                  \
          parse.mli parse.ml    		\
          instantiate.mli instantiate.ml        \
	  typeUtils.mli typeUtils.ml            \
          generalise.mli generalise.ml          \
          typevarcheck.mli typevarcheck.ml      \
          unify.mli unify.ml                    \
          callgraph.ml					\
          sugarTraversals.mli  sugarTraversals.ml	\
          desugarDatatypes.mli desugarDatatypes.ml      \
	  resolvePositions.mli resolvePositions.ml	\
          refineBindings.mli refineBindings.ml		\
          desugarLAttributes.mli desugarLAttributes.ml	\
          transformSugar.mli transformSugar.ml          \
          desugarPages.mli desugarPages.ml		\
          desugarFormlets.mli desugarFormlets.ml        \
          desugarRegexes.mli desugarRegexes.ml		\
          typeSugar.mli typeSugar.ml			\
          sugar.mli sugar.ml    		\
          frontend.ml                           \
	  dumpTypes.ml                          \
          $(DB_CODE)            		\
          database.mli database.ml 		\
          inference.mli inference.ml 		\
          jsonparse.mly         		\
          jsonlex.mll           		\
          json.ml               		\
          linksregex.ml                         \
          library.mli library.ml 		\
          optimiser.mli optimiser.ml    	\
          var.ml                                \
          ir.mli ir.ml                          \
          compilePatterns.ml                    \
          value.mli value.ml                    \
          compileir.ml                          \
          loader.mli loader.ml                  \
          irtojs.mli irtojs.ml                  \
          interpreter.mli interpreter.ml 	\
          evalir.ml                             \
          webif.mli webif.ml           	        \
          test.ml                               \
          tests.ml                              \
          sqlcompileTest.ml                     \
          links.ml              		\

LIBS    = unix nums str $(DB_LIBS) deriving
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


