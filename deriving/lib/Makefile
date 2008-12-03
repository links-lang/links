OCAMLMAKEFILE = ../../OCamlMakefile

AUXLIB_DIRS = 
OCAMLOPT := ocamlopt.opt
OCAMLC := ocamlc.opt
OCAMLDEP := ocamldep

PATH := $(PATH):../syntax
OCAMLFLAGS=-w Ae

SOURCES = util.ml util.mli              	\
          show.ml show.mli              	\
          eq.ml eq.mli                  	\
          bounded.ml bounded.mli        	\
          enum.ml enum.mli              	\
          monad.ml monad.mli            	\
          pickle.ml pickle.mli          	\
          typeable.ml typeable.mli      	\
          dynmap.ml dynmap.mli                  \
          primitives.ml primitives.mli  	\
          shelvehelper.ml shelvehelper.mli      \
          shelve.ml                            	\
          functor.ml functor.mli        	\
          rewriter.ml rewriter.mli      	\
          quickcheck.ml                 	\

LIBS    = nums
RESULT  = deriving
CLIBS 	= 

INCDIRS = $(AUXLIB_DIRS)
LIBDIRS = $(AUXLIB_DIRS)

DOC_FILES=

include $(OCAMLMAKEFILE)
