OCAMLMAKEFILE = ../OCamlMakefile

OCAMLC     = ocamlc.opt
OCAMLOPT   = ocamlopt.opt
OCAMLDEP   = ocamldep.opt
ANNOTATE   = yes
PPFLAGS    = -loc loc
USE_CAMLP4 = yes
LIBS       = camlp4lib unix

CAMLP4_PRE_NOPRINTER = 

CAMLP4_PREFILES = Camlp4Parsers/Camlp4OCamlRevisedParser.cmo \
                  Camlp4Parsers/Camlp4OCamlParser.cmo        \
                  Camlp4Printers/Camlp4AutoPrinter.cmo
CAMLP4_POSTFILES = Camlp4Bin.cmo
CAMLP4_NATIVE_PREFILES=$(CAMLP4_PREFILES:.cmo=.cmx)
CAMLP4_NATIVE_POSTFILES=$(CAMLP4_POSTFILES:.cmo=.cmx)

SOURCES = id.ml                 \
          utils.ml              \
          type.ml               \
          base.ml               \
          extend.ml             \
          show_class.ml         \
          dump_class.ml         \
          enum_class.ml         \
          bounded_class.ml      \
          eq_class.ml           \
          typeable_class.ml     \
          functor_class.ml      \
          pickle_class.ml       \

RESULT = deriving

all: exe

include $(OCAMLMAKEFILE)

exe: $(IMPL_CMO)
	$(OCAMLC) -linkall $(ALL_LDFLAGS) $(CAMLP4_PREFILES) $(IMPL_CMO) -o deriving $(CAMLP4_POSTFILES)
