OCAMLMAKEFILE = ../../OCamlMakefile

AUXLIB_DIRS = 
OCAMLOPT := ocamlopt.opt
OCAMLC := ocamlc.opt
OCAMLDEP := ocamldep

OCAMLFLAGS=-w Ae

SOURCES = util.ml util.mli              \
          show.ml show.mli              \
          bounded.ml bounded.mli        \
          enum.ml enum.mli              \
          monad.ml monad.mli            \
          pickle.ml pickle.mli          \
          typeable.ml typeable.mli      \
          primitives.ml primitives.mli  \
          functor.ml functor.mli        \
          rewriter.ml rewriter.mli      \
          quickcheck.ml                 \

LIBS    = nums
RESULT  = deriving
CLIBS 	= 

INCDIRS = $(AUXLIB_DIRS)
LIBDIRS = $(AUXLIB_DIRS)

DOC_FILES=

include $(OCAMLMAKEFILE)
