#!/bin/sh

./links --debug -c $1 > /tmp/tmp.ml
ocamlc -annot -I lib/compiler -thread unix.cma threads.cma nums.cma str.cma deriving-310/lib/deriving.cma links.cma /tmp/tmp.ml -o $2

