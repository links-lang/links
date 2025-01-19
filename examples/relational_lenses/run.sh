#!/bin/bash


# Check if database configuration exists. If not the creating from
# a default config.sample file
if [[ ! -e config ]]; then
  echo -en "$STARTCOLOR"
  echo -e  "Creating default database test configuration $1/config"
  echo -e  "from $1/config.sample"
  echo -e  "Please customize to match your local database setup."
  echo -en "$ENDCOLOR"
  cp config.sample config
fi

PSQL="psql links -U links -h localhost"
$PSQL < music_example.sql
../../linx cds.links --config=config
echo "SELECT * FROM albums" | $PSQL
echo "SELECT * FROM tracks" | $PSQL
