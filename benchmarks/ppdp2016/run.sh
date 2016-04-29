#!/bin/bash

set -e
set -o xtrace

LINKSROOT="../../"

function drop_tables {
    psql -v ON_ERROR_STOP=1 -q -U postgres -d links -f "droptables.sql"
}

function recreate_tables {
    psql -v ON_ERROR_STOP=1 -q -U postgres -d links -f "organisation.sql"
}

function populate {
    "$LINKSROOT/links" --config=nodebug.config setup.links
}

function noprov {
    "$LINKSROOT/links" --config=db.config noprov.links
}

# Dump
# pg_dump links -t departments -t employees -t tasks -t contacts > filename.sql

# Restore from dump
# psql -U postgres links < filename.sql

drop_tables
recreate_tables
time populate
#time noprov
