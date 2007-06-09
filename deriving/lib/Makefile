OCAMLMAKEFILE = ../../OCamlMakefile

PATH       := $(PATH):../syntax

OCAMLOPT   = ocamlopt.opt
OCAMLC     = ocamlc.opt
OCAMLDEP   = ocamldep.opt
OCAMLFLAGS =-w Ae
LIBS       = nums

SOURCES = util.ml           util.mli		\
          interned.mli      interned.ml		\
          show.ml           show.mli		\
          eq.ml             eq.mli		\
          bounded.ml        bounded.mli		\
          enum.ml           enum.mli		\
          monad.ml          monad.mli		\
          pickle.ml         pickle.mli		\
          typeable.ml       typeable.mli	\
          dynmap.ml         dynmap.mli		\
          shelvehelper.ml   shelvehelper.mli	\
          shelve.ml	    shelve.mli    	\
          functor.ml        functor.mli		\
          rewriter.ml       rewriter.mli	\
          quickcheck.ml                         \

RESULT  = deriving

include $(OCAMLMAKEFILE)
