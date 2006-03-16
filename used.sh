#!/bin/bash

# Answer the question:
#
#    which files use which external names from other files?
#
# Doesn't make any attempt to understand syntax, etc.

# Ask `make' for the list of library paths
ocamlc_options=$(make -p -n  | grep '^AUXLIB_DIRS' | sed "s@\$(OCAML_LIBDIR)@-I $OCAML_LIBDIR@g;s@AUXLIB_DIRS = @@" | sed 's/$(DB_AUXLIBS)//')
for file in *.ml; do
    echo >&2 Processing $file
    for name in $(ocamlc  -i $ocamlc_options  $file 2>/dev/null | sed -n '/^val/ { s/^val \([^ ]*\).*/   \1/ p }'); do
      uses=$(egrep -c -w -- $name *.ml | egrep -v  ":0\$|$file" )
      if [ -z "$uses" ]; then
	  echo "Unused outside: $file:$name"
      else
	  echo -e "$file:$name\t$(echo $uses)"
      fi
    done
done