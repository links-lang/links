OCAMLMAKEFILE = ../../OCamlMakefile

AUXLIB_DIRS  = ../lib/
OCAMLOPT     = ocamlopt.opt
OCAMLC       = ocamlc.opt
OCAMLDEP     = ocamldep
PATH        := $(PATH):../syntax
OCAMLFLAGS   = -w Ae

SOURCES = tests.ml tests.mli shelvetest.ml

LIBS    = nums deriving
RESULT  = test
CLIBS 	= 

INCDIRS = $(AUXLIB_DIRS)
LIBDIRS = $(AUXLIB_DIRS)

include $(OCAMLMAKEFILE)
