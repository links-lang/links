#!/usr/bin/env bash
#
# Checks whether any textual files contain trailing whitespace.
#
# By passing the argument "strip" the script attempts to automatically
# strip trailing white space.

source tools/rule_helpers

# Whether to automatically strip trailing white space.
STRIP=0
if [[ $1 == "strip" ]]; then
    STRIP=1
fi

# Counter to keep track of number of files with trailing white space.
ERRORS=0


for dir in ${DIRS[@]}; do
    for file in $(eval "find \""$dir"\" -type f \( ${FIND_STR} \) \
      -exec grep -E -l \" +$\" {} \;"); do
          echo -n "$file"
          if [[ $STRIP -eq 1 ]]; then
              echo -n "..."
              sed -i 's/[ \t]*$//' $file
              if [[ $? -eq 0 ]]; then
                  echo " CLEANED."
              else
                  echo " FAILED."
                  ERRORS=$((ERRORS+1))
              fi
          else
              echo ""
              ERRORS=$((ERRORS+1))
          fi
    done
done

if [[ $ERRORS -ne 0 ]]; then
    exit 1
else
    exit 0
fi
