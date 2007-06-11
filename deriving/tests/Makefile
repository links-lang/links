OCAMLMAKEFILE = ../../OCamlMakefile

AUXLIB_DIRS  = ../lib/
OCAMLOPT     = ocamlopt.opt
OCAMLC       = ocamlc.opt
OCAMLDEP     = ocamldep
PATH        := $(PATH):../syntax
OCAMLFLAGS   = -w Aef

SOURCES = tests.ml tests.mli shelvetest.ml typeable_tests.ml exp.ml

LIBS    = nums deriving
RESULT  = tests
CLIBS 	= 

INCDIRS = $(AUXLIB_DIRS)
LIBDIRS = $(AUXLIB_DIRS)

include $(OCAMLMAKEFILE)
