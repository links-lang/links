#!/bin/sh

sh compile.sh tests/compiler/$1.links /tmp/$1
/tmp/$1
