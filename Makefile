-include ./Makefile.config

OCAMLMAKEFILE = ./OCamlMakefile

ifdef SQLITE_LIBDIR
   DB_CODE    += lite_database.ml
   DB_AUXLIBS += $(SQLITE_LIBDIR)
   DB_CLIBS   += sqlite_stubs sqlite
   DB_LIBS    += sqlite
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

#OCAMLYACC := menhir --infer --comment --explain --dump --log-grammar 1 --log-code 1 --log-automaton 2
OCAMLYACC := ocamlyacc -v

OCAMLFLAGS=-w Aef
OCAMLDOCFLAGS=-pp deriving

# additional files to clean
TRASH=*.tmp *.output

# Other people's code.
OPC = cgi.ml netencoding.ml netencoding.mli unionfind.ml getopt.ml getopt.mli

SOURCES = $(OPC)                		\
          utility.ml            		\
          settings.mli settings.ml 		\
          debug.mli debug.ml    		\
          rewrite.ml            		\
          performance.ml        		\
          graph.ml              		\
          inferencetypes.mli inferencetypes.ml 	\
          query.mli query.ml          		\
          sql.mli sql.ml               		\
          syntax.mli syntax.ml        		\
          regex.ml                              \
          sugartypes.mli                        \
          sugar.mli sugar.ml    		\
          result.mli result.ml         		\
          errors.mli errors.ml                  \
          sql_transform.mli sql_transform.ml	\
          parser.mly            		\
          $(DB_CODE)            		\
          jsonparse.mly         		\
          json.ml               		\
          forms.mli forms.ml    		\
          database.mli database.ml 		\
          lexer.mll             		\
          parse.mli parse.ml    		\
          callgraph.ml                          \
          inference.mli inference.ml 		\
          linksregex.ml                         \
          library.mli library.ml 		\
          jsonlex.mll           		\
          interpreter.mli interpreter.ml 	\
          optimiser.mli optimiser.ml    	\
          js.mli js.ml          		\
          loader.ml                             \
          webif.mli webif.ml           		\
          test.ml                               \
          links.ml              		\

LIBS    = unix nums str $(DB_LIBS) deriving
RESULT  = links
CLIBS 	= $(DB_CLIBS)

INCDIRS = $(AUXLIB_DIRS)
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

docs-clean:
	cd doc && make clean

clean :: deriving-clean docs-clean

