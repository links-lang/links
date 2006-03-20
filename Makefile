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
          sl_utility.ml         \
          unionfind.ml          \
          graph.ml              \
          performance.ml        \
          rewrite.ml            \
          sl_pickle.ml          \
          sl_kind.ml            \
          query.ml              \
          sl_sql.ml             \
          sl_syntax.ml          \
          sl_inspect.ml         \
          sl_result.ml          \
          jsonparse.ml          \
          jsonlex.ml            \
          json.ml               \
          sl_sugar.ml	        \
          sl_forms.ml           \
          inferencetypes.ml     \
          sl_errors.ml          \
          sl_inference.ml	\
          sl_database.ml        \
          pg_database.ml        \
          $(DB_CODE)            \
          sl_parser.ml          \
          sl_lexer.mll          \
          parse.ml              \
          sl_library.ml         \
          sl_interpreter.ml     \
          sl_sql_transform.ml	\
          sl_optimiser.ml       \
          sl_import.ml          \
          sl_js.ml              \
          webif.ml              \
          links.ml              \

LIBS    = unix postgresql nums str curl pcre netstring cgi getopt $(DB_LIBS) 
RESULT  = links
CLIBS 	= cclib lssl cclib lcrypto $(DB_CLIBS)

#PROFILING = 1

INCDIRS = $(AUXLIB_DIRS)
LIBDIRS = $(AUXLIB_DIRS)

include $(OCAMLMAKEFILE)

test: $(RESULT)
	@for i in tests/*.tests; do echo $$i 1>&2; ./test-harness $$i; done

fixmes:
	@grep FIXME *.ml*

docs:
	pod2html quick-help.pod > quick-help.html
