#!/usr/bin/env bash

OPAM2_URL="https://github.com/ocaml/opam/releases/download/2.0.1/opam-2.0.1-x86_64-linux"
ROOT_DIR=~/.opam #must not quote for tilde expansion
EXEC_NAME=opam2_local


if [ -e $ROOT_DIR ]; then
    echo "opam root exists, aborting"
    exit 1
fi

wget "$OPAM2_URL" -O "$EXEC_NAME"
chmod u+x "$EXEC_NAME"
./${EXEC_NAME} init --disable-sandboxing -n