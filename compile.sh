#!/bin/sh

./links --debug -c --nocps $1 > /tmp/tmp.ml
ocamlc -annot -I lib/compiler unix.cma nums.cma str.cma deriving-310/lib/deriving.cma links.cma /tmp/tmp.ml -o $2

