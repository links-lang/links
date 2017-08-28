
PATH := $(PATH):/home/harvey/links/deriving

SQLITE3_LIBDIR=$(shell ocamlfind query sqlite3)

ifneq ($(SQLITE3_LIBDIR),)
   DB_AUXLIBS += $(SQLITE3_LIBDIR)
   DB_CLIBS   += sqlite3
   DB_LIBS    += sqlite3
endif

OCAMLLDFLAGS = -shared

LIBS    = $(DB_LIBS) 
INCDIRS = $(DB_AUXLIBS) $(EXTRA_INCDIRS) /home/harvey/dynlinx/links
LIBDIRS = $(DB_AUXLIBS) $(EXTRA_LIBDIRS) /home/harvey/dynlinx/links

RESULT = lite3_database.cmxs
SOURCES = lite3_database.ml

all: nc

OCAMLMAKEFILE = ./OCamlMakefile
include $(OCAMLMAKEFILE)