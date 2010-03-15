#!/bin/sh

LINKS=~/dev/git-links/links
LINKSARGS="-d --config=algebra.config"

$LINKS $LINKSARGS $@
BASE=`basename -s .links $@`
pfdot plan.xml | dot -Tpdf -o ${BASE}_plan.pdf
pfopt plan.xml | pfdot | dot -Tpdf -o ${BASE}_plan_opt.pdf
pfopt plan.xml | pfsql > ${BASE}_plan.sql
cat ${BASE}_plan.sql
