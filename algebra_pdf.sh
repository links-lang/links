#!/bin/sh

LINKS=~/dev/git-links/links
LINKSARGS="-d --config=algebra.config"

$LINKS $LINKSARGS $@
BASE=`basename -s .links $@`
for file in `ls plan_unopt_*.xml`;
do
    echo $file
    NUM=`echo $file | sed -e 's/plan_unopt_\(.*\).xml/\1/'`
    pfdot $file | dot -Tpdf -o ${BASE}_plan_unopt_${NUM}.pdf
done;

for file in `ls plan_opt_*.xml`;
do
    echo $file
    NUM=`echo $file | sed -e 's/plan_opt_\(.*\).xml/\1/'`
    pfdot $file | dot -Tpdf -o ${BASE}_plan_opt_${NUM}.pdf
done;
echo ">>> IR complete"
dot -Tpdf -o ${BASE}_ir_complete.pdf ir_complete.dot
echo ">>> IR query"
dot -Tpdf -o ${BASE}_ir_query.pdf ir_query.dot
