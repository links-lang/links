#!/bin/sh

./links --debug -c $1 > /tmp/tmp.ml
ocamlfind ocamlopt \
	-I lib/compiler \
	-thread bigarray.cmxa unix.cmxa  \
	threads.cmxa nums.cmxa str.cmxa \
	deriving-310/lib/deriving.cmxa \
	-package postgresql postgresql.cmxa links.cmxa \
	/tmp/tmp.ml \
	-o $2
