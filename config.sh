#!/bin/sh

path=$PWD/`dirname $0`/links
echo -e Where are your web files kept? 
read webdir
cd examples/
for i in *.links; do
  dest=$webdir/$i;
  dest=${dest/.links/.cgi};
  echo $dest;
  echo \#\!$path | cat - $i > $dest;
  chmod +x $dest
done

cd ..
mkdir $webdir/lib
mkdir $webdir/lib/yahoo
cp lib/js/yahoo/YAHOO.js $webdir/lib/yahoo
cp lib/js/yahoo/event.js $webdir/lib/yahoo
cp lib/js/jslib.js $webdir/lib
cp lib/js/json.js $webdir/lib
cp lib/js/regex.js $webdir/lib
