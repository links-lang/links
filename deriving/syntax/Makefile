# $Id: Makefile,v 1.12 2002/02/04 10:21:44 ddr Exp $

OCAMLC   = ocamlc.opt
CAMLP4   = "camlp4o -loc loc "
OCAMLOPT = ocamlopt.opt
OCAMLDEP = ocamldep
LIBDIR = $(shell ocamlc -where)
CAMLP4DIR = $(shell camlp4 -where)

OCAMLFLAGS=-w Aef

DERIVERS=eq_class typeable_class pickle_class show_class enum_class bounded_class functor_class rewriter_class
DERIVER_SRCS=$(addsuffix .ml,$(DERIVERS))
DERIVER_OBJS=$(addsuffix .cmo,$(DERIVERS))
OBJS=deriving.cmo $(DERIVER_OBJS)
NOBJS=deriving.cmx $(addsuffix .cmx,$(DERIVERS))

all: byte opt deriving
byte: $(OBJS)
opt: $(OBJS)

# Create a custom native camlp4 that incorporates the deriving
# extension.
#
# This is less flexible than loading the bytecode files into the
# standard camlp4, but (consequently) runs much faster.
deriving: $(OBJS)
	$(OCAMLOPT) -linkall -o $@ -I $(CAMLP4DIR) odyl.cmxa camlp4.cmxa pa_o.cmx pa_op.cmx pr_dump.cmx $(NOBJS) camlp4/odyl.cmx 

clean:
	rm -f *.cm[oix] *.o deriving

depend:
	$(OCAMLDEP) -pp $(CAMLP4) *.ml* > .depend

$(OBJS): %.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -pp $(CAMLP4) -I +camlp4 -c $<
	$(OCAMLOPT) $(OCAMLFLAGS) -pp $(CAMLP4) -I +camlp4 -c $<

.SUFFIXES: .mli .ml .cmi .cmo .cmx

-include .depend
