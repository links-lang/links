include Makefile.conf
include VERSION


ALL_TARGET=cduce cduce_lib.cma
INSTALL_BINARIES=cduce$(EXE)

ifneq ($(ML_INTERFACE), false)
  INSTALL_BINARIES += cduce_mktop
endif


ifeq ($(NATIVE),true)
all: cduce_lib.cmxa
endif

PACKAGES = ulex camlp4 pcre num netstring cgi

# Call make with VERBOSE=true to get a trace of commands

VERBOSE=false

ifneq ($(VERBOSE), true)
   HIDE=@
else
   HIDE=
endif


SYNTAX = -I misc/ q_symbol.cmo \
  -symbol cduce_version=\"$(VERSION)\" \
  -symbol build_date=\"$(shell date +%Y-%m-%d)\" \
  -symbol session_dir=\"$(SESSION_DIR)\" \
  -loc "_loc"

SYNTAX_PARSER = -syntax camlp4o $(SYNTAX:%=-ppopt %)

CAMLC_P = ocamlc -g
DEPEND_OCAMLDEP = misc/q_symbol.cmo
ifeq ($(PROFILE), true)
  CAMLOPT_P = ocamlopt -p -inline 10000
  ifeq ($(NATIVE), false) 
    CAMLC_P = ocamlcp -p a
    SYNTAX_PARSER = 
    DEPEND_OCAMLDEP =
  endif
else
  CAMLOPT_P = ocamlopt -inline 10000
endif

OPT = -warn-error FPSXY
ANNOT =
INTERFACE =
ifeq ($(ANNOT), true)
	OPT += -passopt -dtypes
endif
ifeq ($(INTERFACE), true)
	OPT += -passopt -i
endif
OCAMLFIND = ocamlfind
CAMLC = $(OCAMLFIND) $(CAMLC_P) $(OPT) -package "$(PACKAGES)"
CAMLOPT = $(OCAMLFIND) $(CAMLOPT_P) $(OPT) -package "$(PACKAGES)"

ifeq ($(NATIVE), true)
  EXTENSION = cmx
  EXTENSION_LIB = cmxa
  CAML=ocamlopt
  COMPILE = $(CAMLOPT)
  LINK = $(CAMLOPT) -linkpkg gramlib.cmxa camlp4.cmxa pr_o.cmx
  SYNTAX += -symbol ocaml_compiler=\"native\"
else
  EXTENSION = cmo
  EXTENSION_LIB = cma
  COMPILE = $(CAMLC)
  CAML=ocamlc
  LINK = $(CAMLC) -custom -linkpkg gramlib.cma camlp4.cma pr_o.cmo
  SYNTAX += -symbol ocaml_compiler=\"bytecode\"
endif

ifeq ($(FORPACK),true)
  FORPACKOPT=-passopt -for-pack -passopt Cduce_lib
else
  FORPACKOPT=
endif

INSTALL := install -c

ifeq ($(NATIVE),true)
.PHONY: cduce_lib.cma
cduce_lib.cma:
	$(HIDE)$(MAKE) NATIVE=false $@
endif

install: all install_bin install_lib install_doc

install_bin:
	@echo "Install binaries"
	$(HIDE)mkdir -p $(BINDIR)
	$(HIDE)$(INSTALL) -m755 $(INSTALL_BINARIES) $(BINDIR)/
	

install_lib:
	@echo "Build META"
	$(HIDE)(sed "s/%REQ%/$(PACKAGES)/" < META.in | sed "s/%VER%/$(VERSION)/" > META)
	$(HIDE)-$(OCAMLFIND) remove cduce
	$(HIDE)-$(OCAMLFIND) install cduce META \
         cduce_lib.cmi $(wildcard *.cma) $(wildcard *.cmxa) $(wildcard *.a) \


uninstall:
	rm -f $(BINDIR)/cduce$(EXE) $(BINDIR)/cduce_mktop
	rm -f $(MANDIR)/man1/cduce.1
	rm -Rf $(DOCDIR)
	ocamlfind remove cduce

help:
	@echo "GOALS"
	@echo "  cduce : compiles the CDuce command line interpreter"
	@echo "  dtd2cduce: compiles the dtd2cduce tools"
	@echo "  doc : build the documentation"
	@echo "  all : build binaries and libraries"
	@echo "  install : install binaries, man pages, documentation"
	@echo "  clean :  back to the starting point"
	@echo "  uninstall : remove installed files"

# Source directories 

DIRS = misc parser schema typing types compile runtime driver query ocamliface
CLEAN_DIRS = $(DIRS) tools tests

# Objects to build

OBJECTS = \
 driver/config.cmo misc/stats.cmo misc/custom.cmo misc/encodings.cmo \
 misc/upool.cmo misc/pretty.cmo misc/ns.cmo misc/imap.cmo misc/html.cmo \
 \
 types/compunit.cmo types/sortedList.cmo misc/bool.cmo types/ident.cmo \
 types/intervals.cmo types/chars.cmo types/atoms.cmo types/normal.cmo \
 types/types.cmo compile/auto_pat.cmo \
 types/sequence.cmo types/builtin_defs.cmo \
 \
 runtime/value.cmo \
 \
 schema/schema_pcre.cmo schema/schema_types.cmo \
 schema/schema_xml.cmo schema/schema_common.cmo \
 schema/schema_builtin.cmo schema/schema_validator.cmo \
 \
 types/patterns.cmo \
 \
 compile/lambda.cmo \
 runtime/run_dispatch.cmo runtime/explain.cmo runtime/eval.cmo \
 \
 parser/location.cmo  parser/url.cmo \
 parser/ulexer.cmo parser/ast.cmo parser/parser.cmo \
 \
 typing/typed.cmo typing/typepat.cmo types/externals.cmo typing/typer.cmo \
 compile/compile.cmo \
 \
 schema/schema_parser.cmo schema/schema_converter.cmo \
 runtime/load_xml.cmo runtime/print_xml.cmo compile/operators.cmo types/builtin.cmo \
 driver/librarian.cmo types/sample.cmo \
 compile/print_auto.cmo \
 driver/cduce.cmo \
 \
 runtime/system.cmo query/query_aggregates.cmo

schema/schema_types.ml: schema/schema_types.mli
	cp $^ $@
compile/auto_pat.ml: compile/auto_pat.mli
	cp $^ $@
compile/lambda.ml: compile/lambda.mli
	cp $^ $@

ML_INTERFACE_OBJS = \
  ocamliface/caml_cduce.cmo \
  ocamliface/mltypes.cmo ocamliface/mlstub.cmo

ifneq ($(ML_INTERFACE), false)
  OBJECTS += $(ML_INTERFACE_OBJS)
endif

ifneq ($(CURL), false)
  OBJECTS += parser/cduce_curl.cmo
  PACKAGES += curl
endif
ifneq ($(NETCLIENT), false)
  OBJECTS += parser/cduce_netclient.cmo
  PACKAGES += netclient
endif

PXP_PACK=pxp-engine pxp-lex-iso88591
ifeq ($(PXP_WLEX), true)
  PXP_PACK += pxp-wlex-utf8
else
  PXP_PACK += pxp-lex-utf8
endif

ifeq ($(PXP),true)
  OBJECTS += runtime/cduce_pxp.cmo
  PACKAGES += $(PXP_PACK)
  ALL_TARGET += dtd2cduce
  INSTALL_BINARIES += dtd2cduce
endif
ifeq ($(EXPAT), true)
  OBJECTS += runtime/cduce_expat.cmo
  PACKAGES += expat
endif


all: $(ALL_TARGET)

OBJECTS += driver/run.cmo
CDUCE = $(OBJECTS) driver/start.cmo 

ALL_OBJECTS = $(OBJECTS) \
 driver/start.cmo driver/examples.cmo \
 driver/webiface.cmo driver/evaluator.cmo \
 tools/validate.cmo \
 $(ML_INTERFACE_OBJS) parser/cduce_curl.cmo \
 parser/cduce_netclient.cmo \
 runtime/cduce_expat.cmo runtime/cduce_pxp.cmo

ALL_INTERFACES = schema/schema_types.mli 

DEPEND = $(ALL_OBJECTS:.cmo=.ml) $(ALL_OBJECTS:.cmo=.mli) $(ALL_INTERFACES)

INCLUDES = $(DIRS:%=-I %) -I +camlp4

cduce: $(CDUCE:.cmo=.$(EXTENSION))
	@echo "Build $@"
	$(HIDE)$(LINK) $(INCLUDES) -o $@ $^ $(EXTRA_LINK_OPTS)

cduce_packed.ml: $(CDUCE:.cmo=.ml)
	rm -f cduce_packed.ml
	ocaml tools/pack.ml $^ > cduce_packed.ml

cduce_packed: cduce_packed.$(EXTENSION)
	$(LINK) $(INCLUDES) -o $@ $^ $(EXTRA_LINK_OPTS)


cduce_lib.$(EXTENSION_LIB): $(OBJECTS:.cmo=.$(EXTENSION))
	@echo "Pack cduce_lib.$(EXTENSION)"
	$(HIDE)$(COMPILE) -o cduce_lib.$(EXTENSION) -pack $^
	@echo "Build $@"
	$(HIDE)$(COMPILE) -a -o $@ cduce_lib.$(EXTENSION)

dtd2cduce: tools/dtd2cduce.ml
	@echo "Build $@"
	$(HIDE)$(OCAMLFIND) $(CAML) -o $@ -package "$(PXP_PACK) cgi" -linkpkg $^

.PHONY: compute_depend
compute_depend: $(DEPEND_OCAMLDEP)
	@echo "Computing dependencies ..."
	ocamlfind ocamldep -package "$(PACKAGES)" \
		$(INCLUDES) $(SYNTAX_PARSER) $(DEPEND) > depend
clean:
	for i in $(CLEAN_DIRS); do \
	(cd $$i; rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa *~); \
	done
	(cd ocamliface; make clean)
	rm -f `find . -name "*~"`
	rm -f *.cmi *.cmo *.cma *.cmx *.a *.cmxa *.o *~ META
	rm -f cduce$(EXE) ocamlprof.dump
	rm -f dtd2cduce$(EXE) webiface$(EXE) evaluator$(EXE)
	rm -Rf prepro package
	rm -f web/www/*.html web/*~
	rm -f web/*.cdo
	rm -f configure.log
	rm -rf web/doc
	rm -f META

distclean: clean
	rm -f Makefile.conf

ocamliface/mltypes.$(EXTENSION): ocamliface/caml_cduce.$(EXTENSION)

$(ALL_OBJECTS:.cmo=.$(EXTENSION)): misc/q_symbol.cmo
$(ALL_INTERFACES): misc/q_symbol.cmo

.SUFFIXES: .ml .mli .cmo .cmi .cmx

misc/q_symbol.cmo: misc/q_symbol.ml
	@echo "Build $@"		  
	$(HIDE)$(CAMLC) -c -pp 'camlp4o pa_extend.cmo q_MLast.cmo' $<

.ml.cmo:
	@echo "Build $@"
	$(HIDE)$(CAMLC) -c $(INCLUDES) $(SYNTAX_PARSER) $<

.ml.cmx:
	@echo "Build $@"
	$(HIDE)$(CAMLOPT) $(FORPACKOPT) -c $(SYNTAX_PARSER) $(INCLUDES) $<

.mli.cmi:
	@echo "Build $@"
	$(HIDE)$(CAMLC) -c $(SYNTAX_PARSER) $(INCLUDES) $<

include depend

# Documentation

doc: cduce web/site.cdo
	mkdir -p web/doc
	(cd web; ../cduce --run site.cdo --arg doc.xml -o doc)

web/site.cdo: cduce web/xhtml.cdo web/site.cd
	./cduce -I web/ --compile web/site.cd

web/xhtml.cdo: cduce web/xhtml.cd
	./cduce -I web/ --compile web/xhtml.cd

install_doc: doc
	$(HIDE)mkdir -p $(MANDIR)/man1
	$(HIDE)(cd doc; $(MAKE) install INSTALL="$(INSTALL)" MANDIR=$(MANDIR) HIDE=$(HIDE))
	mkdir -p $(DOCDIR)/img
	cp web/doc/*.html $(DOCDIR)/
	cp CHANGES INSTALL INSTALL.WIN32 README AUTHORS LICENSE $(DOCDIR)/
	cp web/img/left.gif web/img/right.gif $(DOCDIR)/img


### Pack OCaml modules for the CDuce/OCaml interface


ocamliface/caml_cduce.cmo:
	@cd ocamliface; \
	$(MAKE) caml_cduce.cmo

ocamliface/caml_cduce.cmx:
	@cd ocamliface; \
	$(MAKE) caml_cduce.cmx

