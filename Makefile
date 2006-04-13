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

AUXLIB_DIRS = $(OCAMLNET_DIRS) $(OCAML_LIBDIR)/curl \
	$(OCAML_LIBDIR)/pcre $(OCAML_LIBDIR)/getopt \
	$(OCAML_LIBDIR)/postgresql $(DB_AUXLIBS)

OCAMLOPT := ocamlopt.opt
OCAMLC := ocamlc.opt

#OCAMLYACC := menhir --infer --comment --explain --dump --log-grammar 1 --log-code 1 --log-automaton 2
OCAMLYACC := ocamlyacc

OCAMLFLAGS=-w Ae

SOURCES = cgi.ml                \
          utility.ml            \
          unionfind.ml          \
          graph.ml              \
          performance.ml        \
          rewrite.ml            \
          pickle.ml             \
          kind.ml               \
          query.ml              \
          sql.ml                \
          syntax.ml             \
          result.ml             \
          jsonparse.ml          \
          jsonlex.ml            \
          json.ml               \
          sugar.ml              \
          forms.ml              \
          inferencetypes.ml     \
          errors.ml             \
          inference.ml          \
          database.ml           \
          pg_database.ml        \
          $(DB_CODE)            \
          parser.ml             \
          lexer.mll             \
          parse.ml              \
          library.ml            \
          interpreter.ml        \
          sql_transform.ml      \
          optimiser.ml          \
          import.ml             \
          js.ml                 \
          webif.ml              \
          links.ml              \

LIBS    = unix postgresql nums str curl pcre netstring cgi getopt $(DB_LIBS) 
RESULT  = links
CLIBS 	= cclib lssl cclib lcrypto $(DB_CLIBS)

#PROFILING = 1

INCDIRS = $(AUXLIB_DIRS)
LIBDIRS = $(AUXLIB_DIRS)

include $(OCAMLMAKEFILE)

tests:
test: $(RESULT)
	@for i in tests/*.tests; do echo $$i 1>&2; ./test-harness $$i; done

fixmes:
	@grep FIXME *.ml*

docs:
	pod2html quick-help.pod > quick-help.html
