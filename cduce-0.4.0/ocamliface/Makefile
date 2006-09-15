# This Makefile generates caml_cduce.cmo/.cmx
# It must be called with an OCAML_SRC argument pointing to the root
# of an OCaml source tree.
include ../Makefile.conf

all: caml_cduce.cmo caml_cduce.cmx

STDLIB=$(shell ocamlc -where)
CMI_MAGIC=$(shell grep cmi_magic ocaml-3.08.4/utils/config.mlp)


ifeq ($(FORPACK),true)
  FORPACKOPT1=-for-pack Cduce_lib.Caml_cduce
  FORPACKOPT2=-for-pack Cduce_lib
else
  FORPACKOPT1=
  FORPACKOPT2=
endif

ocaml_files:
	mkdir ocaml_files
	$(HIDE)cp $(patsubst %,$(OCAML_SRC)/%, $(COPY_FILES)) ocaml_files/
	cp location.ml ocaml_files
	mv ocaml_files/asttypes.mli ocaml_files/asttypes.ml
	sed s=STDLIB=$(STDLIB)= config.ml > ocaml_files/config.ml
	grep cmi_magic $(OCAML_SRC)/utils/config.mlp >> ocaml_files/config.ml

caml_cduce.cmo: ocaml_files
	@echo "Build $@"
	(cd ocaml_files; ocamlc $(FORPACKOPT1) $@ -c $(COMPILE_FILES);\
	ocamlc $(FORPACKOPT2) -pack -o $@ $(OBJECTS); \
	cp caml_cduce.cmo caml_cduce.cmi ..)

caml_cduce.cmx: ocaml_files
	@echo "Build $@"
	(cd ocaml_files; ocamlopt $(FORPACKOPT1) $@ -c $(COMPILE_FILES);\
	ocamlopt $(FORPACKOPT2) -pack -o $@ $(XOBJECTS); \
	cp caml_cduce.cmx caml_cduce.o caml_cduce.cmi ..)

clean:
	rm -Rf ocaml_files *~ *.cm*

COPY_FILES=\
  utils/misc.ml utils/tbl.ml \
  utils/consistbl.ml utils/warnings.ml utils/terminfo.ml utils/clflags.ml \
  parsing/asttypes.mli parsing/longident.ml \
  typing/ident.ml typing/path.ml \
  typing/primitive.ml typing/types.ml \
  typing/btype.ml typing/oprint.ml \
  typing/subst.ml typing/predef.ml \
  typing/datarepr.ml typing/env.ml \
  typing/ctype.ml typing/ctype.mli typing/printtyp.ml

COMPILE_FILES=\
  config.ml misc.ml tbl.ml \
  clflags.ml consistbl.ml warnings.ml terminfo.ml \
  location.ml asttypes.ml longident.ml \
  ident.ml path.ml \
  primitive.ml types.ml \
  btype.ml oprint.ml \
  subst.ml predef.ml \
  datarepr.ml env.ml ctype.mli ctype.ml printtyp.ml

COMPILE_FILES_ML=$(filter %.ml,$(COMPILE_FILES))
OBJECTS=$(COMPILE_FILES_ML:.ml=.cmo)
XOBJECTS=$(COMPILE_FILES_ML:.ml=.cmx)