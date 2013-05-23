#!/bin/sh

./links --debug -c $1 > /tmp/tmp.ml
ocamlopt -annot -I /opt/local/lib/ocaml/site-lib/postgresql/ -I lib/compiler -ccopt -L/opt/local/lib/ocaml/site-lib/postgresql/ -thread bigarray.cmxa unix.cmxa threads.cmxa nums.cmxa str.cmxa deriving-310/lib/deriving.cmxa postgresql.cmxa links.cmxa /tmp/tmp.ml -o $2

