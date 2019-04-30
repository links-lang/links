#!/usr/bin/env bash

# OPAM version to download.
OPAM_VERSION="2.0.4"
OPAM_URL="https://github.com/ocaml/opam/releases/download/${OPAM_VERSION}/opam-${OPAM_VERSION}-x86_64-linux"
# Designated home of opam.
OPAM_HOME_DIR="$HOME/.opam"
# Name of the local OPAM executable.
OPAM_EXEC="opam2_local"

# Abort if the OPAM home directory already exists -- which could
# indicate that the current installation image isn't clean.
if [ -e $OPAM_HOME_DIR ]; then
    echo "opam root exists, aborting" >> /dev/stderr
    exit 1
fi

# Attempt to download the desired OPAM version.
wget "$OPAM_URL" -O "$OPAM_EXEC"
if [[ $? -ne 0 || ! -f "./$OPAM_EXEC" ]]; then
    echo "error: download failed $OPAM_URL" >> /dev/stderr
    exit 1
fi

# Make the script executable for the owner.
chmod u+x "$OPAM_EXEC"

# Initialise OPAM.
eval "./$OPAM_EXEC init --disable-sandboxing -n"
