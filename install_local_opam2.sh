#!/usr/bin/env bash

# OPAM version to download.
OPAM_VERSION="2.0.4"
OPAM_URL="https://github.com/ocaml/opam/releases/download/${OPAM_VERSION}/opam-${OPAM_VERSION}-x86_64-linux"
# Designated home of opam.
OPAM_HOME_DIR="$HOME/.opam"
# Name of the local OPAM executable.
EXEC_NAME="opam${OPAM_VERSION}_local"

# Abort if the OPAM home directory already exists -- which could
# indicate that the current installation image isn't clean.
if [ -e $OPAM_HOME_DIR ]; then
    echo "opam root exists, aborting" >> /dev/stderr
    exit 1
fi

# Attempt to download the desired OPAM version.
wget "$OPAM_URL" -O "$EXEC_NAME"
if [[ $? -ne 0 || ! -f "./$EXEC_NAME" ]]; then
    echo "error: download failed $OPAM_URL" >> /dev/stderr
    exit 1
fi

# Make the script executable for the owner.
chmod u+x "$EXEC_NAME"

# Initialise OPAM.
eval "$EXEC_NAME init --disable-sandboxing -n"
