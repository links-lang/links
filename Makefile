OCAMLMAKEFILE = ./OCamlMakefile

ifndef OCAML_LIBDIR
   OCAML_LIBDIR = $(OCAMLLIB)/site-lib
endif

ifdef BUILD_SQLITE
   DB_CODE    += lite_database.ml
   DB_AUXLIBS += $(OCAML_LIBDIR)/sqlite
   DB_CLIBS   += cclib lsqlite_stubs
   DB_LIBS    += sqlite
endif

ifdef BUILD_MYSQL
   DB_CODE    += mysql_database.ml
   DB_AUXLIBS += $(OCAML_LIBDIR)/mysql
   DB_LIBS    += mysql
endif

ifdef OCAMLNET_FINDLIB
#-- ocamlnet without findlib
   OCAMLNET_DIRS = $(OCAML_LIBDIR)/ocamlnet
else
#-- ocamlnet with findlib
   OCAMLNET_DIRS = $(OCAML_LIBDIR)/netstring $(OCAML_LIBDIR)/cgi 
endif

AUXLIB_DIRS = $(OCAMLNET_DIRS) \
	$(OCAML_LIBDIR)/pcre $(OCAML_LIBDIR)/getopt \
	$(OCAML_LIBDIR)/postgresql $(DB_AUXLIBS)

OCAMLOPT := ocamlopt.opt
OCAMLC := ocamlc.opt

#OCAMLYACC := menhir --infer --comment --explain --dump --log-grammar 1 --log-code 1 --log-automaton 2
OCAMLYACC := ocamlyacc

OCAMLFLAGS=-w Ae

SOURCES = cgi.ml                \
          unionfind.ml          \
          utility.ml            \
          rewrite.ml            \
          pickle.mli pickle.ml  \
          performance.ml        \
          graph.ml              \
          type_basis.ml type_basis.mli \
          kind.mli kind.ml      \
          query.ml              \
          inferencetypes.mli inferencetypes.ml \
          sql.ml                \
          syntax.ml             \
          sugar.ml              \
          result.ml             \
          sql_transform.ml      \
          pg_database.ml        \
          parser.mly            \
          $(DB_CODE)            \
          jsonparse.mly         \
          json.ml               \
          forms.mli forms.ml    \
          errors.mli errors.ml  \
          database.mli database.ml \
          library.ml            \
          lexer.mll             \
          jsonlex.mll           \
          inference.mli inference.ml \
          parse.mli parse.ml    \
          interpreter.mli interpreter.ml \
          optimiser.ml          \
          js.mli js.ml          \
          webif.ml              \
          links.ml              \

LIBS    = unix postgresql nums str pcre netstring cgi getopt $(DB_LIBS) 
RESULT  = links
CLIBS 	= cclib lssl cclib lcrypto $(DB_CLIBS)

#PROFILING = 1

INCDIRS = $(AUXLIB_DIRS)
LIBDIRS = $(AUXLIB_DIRS)

include $(OCAMLMAKEFILE)

test-raw:
	for i in tests/*.tests; do echo $$i 1>&2; ./test-harness $$i; done

tests:
test: $(RESULT)
	@./run-tests
	@perl -MTest::Harness -e 'Test::Harness::runtests("tests/web-tests.pl")'

fixmes:
	@grep FIXME *.ml*

docs:
	pod2html quick-help.pod > quick-help.html
