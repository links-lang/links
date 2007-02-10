OCAMLMAKEFILE = ../../OCamlMakefile

AUXLIB_DIRS = ../lib/
OCAMLOPT := ocamlopt.opt
OCAMLC := ocamlc.opt
OCAMLDEP := ocamldep

PATH := $(PATH):../syntax
OCAMLFLAGS=-w Ae

SOURCES = eqtest.ml

LIBS    = nums deriving
RESULT  = test
CLIBS 	= 

INCDIRS = $(AUXLIB_DIRS)
LIBDIRS = $(AUXLIB_DIRS)

DOC_FILES=

include $(OCAMLMAKEFILE)
