# $Id: Makefile,v 1.12 2002/02/04 10:21:44 ddr Exp $

OCAMLC   = ocamlc.opt
CAMLP4   = "camlp4r -loc loc "
OCAMLOPT = ocamlopt.opt
OCAMLDEP = ocamldep
LIBDIR = `ocamlc -where`

DERIVERS=pickle_class show_class enum_class bounded_class
DERIVER_SRCS=$(addsuffix .ml,$(DERIVERS))
DERIVER_OBJS=$(addsuffix .cmo,$(DERIVERS))
OBJS=deriving.cmo $(DERIVER_OBJS)
NOBJS=deriving.cmx $(addsuffix .cmx,$(DERIVERS))

byte: $(OBJS)
opt: $(OBJS)
all: byte opt deriving

# Create a custom native camlp4 that incorporates the deriving
# extension.
#
# This is less flexible than loading the bytecode files into the
# standard camlp4, but (consequently) runs much faster.
deriving:
	$(OCAMLOPT) -linkall -o $@ -I `camlp4 -where` odyl.cmxa camlp4.cmxa pa_o.cmx pa_op.cmx pr_dump.cmx $(NOBJS) camlp4/odyl.cmx 

clean:
	rm -f *.cm[oix] *.o

depend:
	$(OCAMLDEP) -pp $(CAMLP4) *.ml* > .depend

$(OBJS): %.cmo: %.ml
	$(OCAMLC) -pp $(CAMLP4) -I +camlp4 -c $<
	$(OCAMLOPT) -pp $(CAMLP4) -I +camlp4 -c $<

.SUFFIXES: .mli .ml .cmi .cmo .cmx

-include .depend
