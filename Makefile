-include ./Makefile.config

OCAMLMAKEFILE = ./OCamlMakefile

ifdef BUILD_SQLITE
   DB_CODE    += lite_database.ml
   DB_AUXLIBS += $(SQLLITE_LIBDIR)
   DB_CLIBS   += cclib lsqlite_stubs
   DB_LIBS    += sqlite
endif

ifdef BUILD_MYSQL
   DB_CODE    += mysql_database.ml
   DB_AUXLIBS += $(MYSQL_LIBDIR)
   DB_LIBS    += mysql
endif

ifdef BUILD_POSTGRESQL
   DB_CODE    += pg_database.ml
   DB_AUXLIBS += $(POSTGRESQL_LIBDIR)
   DB_LIBS    += postgresql
endif

AUXLIB_DIRS = $(DB_AUXLIBS)

DERIVING_DIR=deriving
CLASSES=deriving.cmo show_class.cmo enum_class.cmo bounded_class.cmo pickle_class.cmo
PP='camlp4o -I $(DERIVING_DIR)/syntax $(CLASSES)'

OCAMLOPT := ocamlopt.opt -pp $(PP)
OCAMLC := ocamlc.opt -pp $(PP)
OCAMLDEP := ocamldep -pp $(PP)

#OCAMLYACC := menhir --infer --comment --explain --dump --log-grammar 1 --log-code 1 --log-automaton 2
OCAMLYACC := ocamlyacc -v

OCAMLFLAGS=-w Aef

# Other people's code.
OPC = cgi.ml netencoding.ml netencoding.mli unionfind.ml getopt.ml getopt.mli

SOURCES = $(OPC)                		\
          utility.ml            		\
          settings.mli settings.ml 		\
          debug.mli debug.ml    		\
          rewrite.ml            		\
          pickle.mli pickle.ml  		\
          performance.ml        		\
          graph.ml              		\
          type_basis.mli type_basis.ml 		\
          types.mli types.ml      		\
          query.ml              		\
          inferencetypes.mli inferencetypes.ml 	\
          sql.ml                		\
          syntax.ml             		\
          sugar.ml              		\
          result.ml             		\
          sql_transform.ml      		\
          parser.mly            		\
          $(DB_CODE)            		\
          jsonparse.mly         		\
          json.ml               		\
          forms.mli forms.ml    		\
          errors.mli errors.ml  		\
          database.mli database.ml 		\
          lexer.mll             		\
          parse.mli parse.ml    		\
          inference.mli inference.ml 		\
          library.mli library.ml 		\
          jsonlex.mll           		\
          interpreter.mli interpreter.ml 	\
          optimiser.ml          		\
          js.mli js.ml          		\
          webif.ml              		\
          links.ml              		\

LIBS    = unix nums str $(DB_LIBS) 
RESULT  = links
CLIBS 	= $(DB_CLIBS)

#PROFILING = 1

INCDIRS = $(AUXLIB_DIRS)
LIBDIRS = $(AUXLIB_DIRS)

PRE_TARGETS = $(DERIVING_DIR)/built

clean : deriving-clean

include $(OCAMLMAKEFILE)

.PHONY : deriving
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
	@grep FIXME *.ml*

.PHONY: docs

docs: quick-help.html

quick-help.html: quick-help.pod
	pod2html quick-help.pod > quick-help.html
