
PATH := $(PATH):/home/harvey/links/deriving

POSTGRESQL_LIBDIR=$(shell ocamlfind query postgresql)

ifneq ($(POSTGRESQL_LIBDIR),)
   DB_AUXLIBS += $(POSTGRESQL_LIBDIR)
   DB_LIBS    += postgresql
   THREADS = yes
endif

OCAMLLDFLAGS = -shared

LIBS    = $(DB_LIBS) 
INCDIRS = $(DB_AUXLIBS) $(EXTRA_INCDIRS) /home/harvey/dynlinx/links
LIBDIRS = $(DB_AUXLIBS) $(EXTRA_LIBDIRS) /home/harvey/dynlinx/links
THREADS = yes

RESULT = pg_database.cmxs
SOURCES = pg_database.ml

all: nc

OCAMLMAKEFILE = ./OCamlMakefile
include $(OCAMLMAKEFILE)