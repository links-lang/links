-include ./Makefile.config

OCAMLMAKEFILE = ./OCamlMakefile

ifdef SQLLITE_LIBDIR
   DB_CODE    += lite_database.ml
   DB_AUXLIBS += $(SQLLITE_LIBDIR)
   DB_CLIBS   += cclib lsqlite_stubs
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

PP=deriving/syntax/deriving

OCAMLOPT := ocamlopt.opt -pp $(PP)
OCAMLC := ocamlc.opt -pp $(PP)
OCAMLDEP := ocamldep -pp $(PP)

VPATH=regex

#OCAMLYACC := menhir --infer --comment --explain --dump --log-grammar 1 --log-code 1 --log-automaton 2
OCAMLYACC := ocamlyacc -v

OCAMLFLAGS=-w Aef

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
          type_basis.mli type_basis.ml 		\
          types.mli types.ml      		\
          query.ml              		\
          sql.ml                		\
          syntax.ml             		\
          regex.ml                              \
          inferencetypes.mli inferencetypes.ml 	\
          sugartypes.mli                        \
          sugar.ml sugar.mli    		\
          result.ml result.mli         		\
          errors.mli errors.ml                  \
          sql_transform.ml      		\
          parser.mly            		\
          $(DB_CODE)            		\
          jsonparse.mly         		\
          json.ml               		\
          forms.mli forms.ml    		\
          database.mli database.ml 		\
          lexer.mll             		\
          parse.mli parse.ml    		\
          inference.mli inference.ml 		\
          linksregex.ml                         \
          library.mli library.ml 		\
          jsonlex.mll           		\
          interpreter.mli interpreter.ml 	\
          optimiser.ml          		\
          js.mli js.ml          		\
          webif.ml              		\
          links.ml              		\

LIBS    = unix nums str $(DB_LIBS) deriving
RESULT  = links
CLIBS 	= $(DB_CLIBS)

#PROFILING = 1

INCDIRS = $(AUXLIB_DIRS)
LIBDIRS = $(AUXLIB_DIRS)

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

.PHONY: docs

docs: quick-help.html

quick-help.html: quick-help.pod
	pod2html quick-help.pod > quick-help.html
	@rm pod2htm*.tmp

clean : deriving-clean
