#!/bin/sh

LINKS=~/dev/git-links/links
LINKSARGS="-d --config=algebra.config"

$LINKS $LINKSARGS $@
BASE=`basename -s .links $@`
echo ">>> pfdot"
pfdot plan.xml | dot -Tpdf -o ${BASE}_plan.pdf
echo ">>> pfdot optimized"
pfdot plan_opt.xml | dot -Tpdf -o ${BASE}_plan_opt.pdf
echo ">>> IR complete"
dot -Tpdf -o ${BASE}_ir_complete.pdf ir_complete.dot
echo ">>> IR query"
dot -Tpdf -o ${BASE}_ir_query.pdf ir_query.dot
