#!/bin/sh

LINKS=~/dev/git-links/links
LINKSARGS="-d --config=algebra.config"

$LINKS $LINKSARGS $@
BASE=`basename -s .links $@`
echo ">>> pfdot"
#pfdot plan.xml | dot -Tpdf -o ${BASE}_plan.pdf
echo ">>> pfopt"
#pfopt plan.xml | pfdot | dot -Tpdf -o ${BASE}_plan_opt.pdf
echo ">>> pfsql"
#pfopt plan.xml | pfsql > ${BASE}_plan.sql
