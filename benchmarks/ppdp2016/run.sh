#!/bin/bash

set -e
set -o xtrace

LINKSROOT="../../"

function recreate_tables {
    psql -v ON_ERROR_STOP=1 -q -U postgres -d links -f "organisation.sql"
}

function populate {
    "$LINKSROOT/links" --config=nodebug.config setup.links
}

function noprov {
    "$LINKSROOT/links" --config=db.config noprov.links
}

recreate_tables
populate
time noprov
