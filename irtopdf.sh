#!/bin/sh

LINKS=~/dev/git-links/links
LINKSARGS="-d --config=debug.config"

$LINKS $LINKSARGS $@
COMPLETE=`basename -s .links $@`_complete.pdf
QUERY=`basename -s .links $@`_query.pdf
dot -Tpdf -o $COMPLETE ir_complete.dot
dot -Tpdf -o $QUERY ir_query.dot