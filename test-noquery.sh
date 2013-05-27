#!/bin/sh

sh compile-noquery.sh tests/compiler/$1.links /tmp/$1
/tmp/$1
