#!/bin/bash

set -e

# download, unpack
#wget http://msowww.anu.edu.au/%7Eralph/OPTED/v003.zip
wget http://groups.inf.ed.ac.uk/links/dict/v003.zip
unzip v003.zip
cd v003

# recode (MAC -> Unix)
perl -pi -e 's/\r/\n/g' *.html

# transform
sed -ne "/<P>/ {s!<P><B>\(.*\)</B> (<I>\(.*\)</I>) \(.*\)</P>!\1	\2	\3!;s!'!''!g;s!\(.*\)	\(.*\)	\(.*\)!insert into wordlist (word, type, meaning) values ('\1', '\2', '\3');!;p}" *.html > data.sql

# patch
patch -R data.sql <<EOF
134007,134009c134007
< insert into wordlist (word, type, meaning) values ('Reenforce', 'v.', 'That part of a cannon near the breech which is thicker than the rest of the piece, so as better to resist the force of the exploding powder. See Illust. of Cannon.');
< insert into wordlist (word, type, meaning) values ('Reenforce', 'v.', '(b)');
< insert into wordlist (word, type, meaning) values ('Reenforce', 'v.', 'An additional thickness of canvas, cloth, or the like, around an eyelet, buttonhole, etc.');
---
> insert into wordlist (word, type, meaning) values ('Reenforce</B> (<I>v.</I>) That part of a cannon near the breech which is thicker than the rest of the piece, so as better to resist the force of the exploding powder. See Illust. of Cannon.<P><B>Reenforce</B> (<I>v.</I>)  (b) <P><B>Reenforce', 'v.', 'An additional thickness of canvas, cloth, or the like, around an eyelet, buttonhole, etc.');
153185,153186c153183
< insert into wordlist (word, type, meaning) values ('Spilikin', 'n.', 'One of a number of small pieces or pegs of wood, ivory, bone, or other material, for playing a game, or for counting the score in a game, as in cribbage. In the plural (spilikins');
< insert into wordlist (word, type, meaning) values ('Spilikins', 'pl.', 'of Spilikin');
---
> insert into wordlist (word, type, meaning) values ('Spilikin</B> (<I>n.</I>) One of a number of small pieces or pegs of wood, ivory, bone, or other material, for playing a game, or for counting the score in a game, as in cribbage. In the plural (spilikins<P><B>spilikins', 'pl. ', 'of Spilikin');
EOF

# clean up
mv data.sql ..
cd ..
rm -r v003
rm v003.zip
