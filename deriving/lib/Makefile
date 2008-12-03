OCAMLMAKEFILE = ../OCamlMakefile

PATH       := $(PATH):../syntax

OCAMLOPT   = ocamlopt.opt
OCAMLC     = ocamlc.opt
OCAMLDEP   = ocamldep.opt
OCAMLFLAGS =-w ae
LIBS       = nums str unix

SOURCES = show.ml           show.mli		\
          interned.mli      interned.ml		\
          eq.ml             eq.mli		\
          bounded.ml        bounded.mli		\
          enum.ml           enum.mli		\
          monad.ml          monad.mli		\
          dump.ml           dump.mli		\
          typeable.ml       typeable.mli	\
          dynmap.ml         dynmap.mli		\
          pickle.ml	    pickle.mli    	\
          functor.ml        functor.mli		\


RESULT  = deriving

include $(OCAMLMAKEFILE)
