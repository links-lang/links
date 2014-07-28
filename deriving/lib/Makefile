OCAMLMAKEFILE = ../OCamlMakefile

LIBS=

PATH       := $(PATH):../syntax

OCAMLFLAGS =-w ae -g

SOURCES = show.ml           show.mli		\
          interned.mli      interned.ml		\
          eq.ml             eq.mli		\
          hash.ml           hash.mli		\
          bounded.ml        bounded.mli		\
          enum.ml           enum.mli		\
          dump.ml           dump.mli		\
          typeable.ml       typeable.mli	\
          dynmap.ml         dynmap.mli		\
          pickle.ml	    pickle.mli    	\
                                                 \
          monad.ml          monad.mli


RESULT  = deriving

include $(OCAMLMAKEFILE)
