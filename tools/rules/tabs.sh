#!/usr/bin/env bash
#
# Checks whether any textual files contain tab characters

source tools/rule_helpers


# Counter to keep track of number of files with tabs
ERRORS=0


if has_gnu_find ; then
  for dir in ${DIRS[@]}; do
      # trailing $ in string literal activates c-style
      # escape sequences
      TAB_REGEX=$'\t'
      EVAL_CMD="find \""$dir"\" \
        -type f \( ${FIND_STR} \) \
        -exec grep -E -l \"$TAB_REGEX\" {} \; "
      for file in $(eval "$EVAL_CMD"); do
          echo "Tab character found in $file, alerting authorities."
          exit 1
      done
  done
else
  echo "Warning: GNU find not found, skipping test"
fi
