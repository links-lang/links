#!/usr/bin/env bash

a = grep -Ril "Type." ./core
b = grep -Ril "open Types" ./core

comm -13 <(a) <(b)

