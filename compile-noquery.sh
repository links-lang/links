#!/bin/sh

./links --debug --noquery -c $1 > /tmp/tmp.ml
ocamlfind ocamlopt \
	-I lib/compiler \
	-thread bigarray.cmxa unix.cmxa  \
	threads.cmxa nums.cmxa str.cmxa \
	deriving-310/lib/deriving.cmxa \
	-package postgresql postgresql.cmxa links.cmxa \
	-w -20 -w -11 -w -26 \
        /tmp/tmp.ml \
	-o $2
