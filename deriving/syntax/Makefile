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

byte: $(OBJS)
opt: $(OBJS)
all: byte opt

clean:
	rm -f *.cm[oix] *.o

depend:
	$(OCAMLDEP) -pp $(CAMLP4) *.ml* > .depend

$(OBJS): %.cmo: %.ml
	$(OCAMLC) -pp $(CAMLP4) -I +camlp4 -c $<

.SUFFIXES: .mli .ml .cmi .cmo .cmx

-include .depend
