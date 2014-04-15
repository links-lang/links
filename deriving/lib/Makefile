OCAMLMAKEFILE = ../OCamlMakefile

PATH       := $(PATH):../syntax

OCAMLOPT   = ocamlopt.opt
OCAMLC     = ocamlc.opt
OCAMLDEP   = ocamldep.opt
OCAMLFLAGS =-w ae -g
LIBS       = nums str unix

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
