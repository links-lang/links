OCAMLMAKEFILE = ../OCamlMakefile

AUXLIB_DIRS  = ../lib/
OCAMLOPT     = ocamlopt.opt
OCAMLC       = ocamlc.opt
OCAMLDEP     = ocamldep
PATH        := $(PATH):../syntax
OCAMLFLAGS   = -w Aef

SOURCES = defs.ml               \
          bimap.ml bimap.mli    \
          sigs.ml               \
          pickle_tests.ml       \
	  typeable_tests.ml     \
	  bounded_tests.ml      \
	  eq_tests.ml           \
	  dump_tests.ml         \
	  enum_tests.ml         \
	  functor_tests.ml      \
	  show_tests.ml         \
          exp.ml                \
          inline.ml             \

LIBS    = nums deriving
RESULT  = tests
CLIBS 	= 

INCDIRS = $(AUXLIB_DIRS)
LIBDIRS = $(AUXLIB_DIRS)

include $(OCAMLMAKEFILE)
