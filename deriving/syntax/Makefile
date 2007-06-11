OCAMLMAKEFILE = ../../OCamlMakefile

OCAMLC     = ocamlc.opt
OCAMLOPT   = ocamlopt.opt
OCAMLDEP   = ocamldep.opt
ANNOTATE   = yes
PPFLAGS    = -loc loc
USE_CAMLP4 = yes
LIBS       = camlp4lib unix

SOURCES = utils.ml              \
          type.ml               \
          base.ml               \
          extend.ml             \
          show_class.ml         \
          pickle_class.ml       \
          enum_class.ml         \
          bounded_class.ml      \
          eq_class.ml           \
          typeable_class.ml     \
          functor_class.ml      \
          shelve_class.ml       \

#shelve rewriter fold

RESULT = deriving

include $(OCAMLMAKEFILE)





#ocamlc -g -thread -I -custom -o macroprint unix.cma threads.cma camlp4/camlp4lib.cma camlp4/Camlp4Printers/Camlp4OCamlPrinter.cmo camlp4/Camlp4Parsers/Camlp4OCamlParser.cmo Camlp4MacroParser.cmo camlp4/Camlp4Filters/Camlp4TrashRemover.cmo camlp4/Camlp4Bin.cmo
