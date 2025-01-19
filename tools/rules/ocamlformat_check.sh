#!/usr/bin/env bash
#
# Checks whether any textual files correspond to the output of ocamlformat.
#
# By passing the argument "fix" the script attempts to automatically
# fix code formatting.

# Whether to fix the formatting in all code files.
FIX=0
if [[ $1 == "fix" ]]; then
    FIX=1
fi

# Directories under scrutiny
DIRS=("bin" "core" "lens" "tests/unit")

# Counter to keep track of number of files with trailing white space.
ERRORS=0

PATTERNS=("*.ml" "*.mli")

# https://stackoverflow.com/questions/1527049/how-can-i-join-elements-of-an-array-in-bash
SEP=" -o -name "
FIND_STR="$( printf "${SEP}\"%s\"" "${PATTERNS[@]}" )"
FIND_STR="${FIND_STR:${#SEP}}"
FIND_STR="-name $FIND_STR"
#echo ${FIND_STR}

for dir in ${DIRS[@]}; do
    for file in $(eval "find \""$dir"\" -type f \( ${FIND_STR} \)"); do
        CAT=`cat $file`
        FMT=`ocamlformat $file`
        if [ "$FMT" != "$CAT" ]; then
            echo -n "$file"

            if [ $FIX -eq 1 ]; then
                echo -n "..."
                ocamlformat -i $file

                if [ $? -eq 0 ]; then
                    echo -n " FIXED."
                else
                    echo -n " FAILED."
                fi
            else
                ERRORS=$((ERRORS+1))
            fi

            echo ""
        fi

    done
done

if [[ $ERRORS -ne 0 ]]; then
    exit 1
else
    exit 0
fi
