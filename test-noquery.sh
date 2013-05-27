#!/bin/sh

/usr/bin/time sh compile-noquery.sh tests/compiler/$1.links /tmp/$1
/usr/bin/time /tmp/$1
