#!/bin/bash

PSQL="psql links -U links -h localhost"
$PSQL < music_example.sql
../../linx cds.links --config=links.config
echo "SELECT * FROM albums" | $PSQL
echo "SELECT * FROM tracks" | $PSQL 
