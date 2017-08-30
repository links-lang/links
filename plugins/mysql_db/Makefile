
PATH := $(PATH):/home/harvey/links/deriving

MYSQL_LIBDIR=$(shell ocamlfind query mysql)

ifneq ($(MYSQL_LIBDIR),)
   DB_AUXLIBS += $(MYSQL_LIBDIR)
   DB_LIBS    += mysql
endif

OCAMLLDFLAGS = -shared

LIBS    = $(DB_LIBS) 
INCDIRS = $(DB_AUXLIBS) $(EXTRA_INCDIRS) /home/harvey/dynlinx/links
LIBDIRS = $(DB_AUXLIBS) $(EXTRA_LIBDIRS) /home/harvey/dynlinx/links

RESULT = mysql_database.cmxs
SOURCES = mysql_database.ml

all: nc

OCAMLMAKEFILE = ./OCamlMakefile
include $(OCAMLMAKEFILE)