#!/usr/bin/env bash
# shellcheck disable=

declare -A libreoffice_type_map
libreoffice_type_map=(
        [detect]='1'
        [text]='2'
        [date]='5'    ## We insist on ISO dates.  Choose a better future!
        [skip]='9'
        [ignore]='9'
        [number]='10'
)

libreoffice_import_format_string() {
        local fieldno=1
        for ty in "$@"
        do tyspec=${libreoffice_type_map[$ty]}
           if test -z "${tyspec}"
           then echo "ERROR:  unknown type spec: '$ty'; known: ${!libreoffice_type_map[@]}">&2; exit 1; fi
           echo -n '/'$fieldno'/'"$tyspec"
           fieldno=$((fieldno+1))
        done | cut -c2-
}

libreoffice_csv_import_schema() {
        ## A specification for the CSV import filter of 'libreoffice --convert-to',
        ## that provides it with field interpretation types.
        ##
        ## For a detailed explanation of this b****t, please see:
        ## https://wiki.openoffice.org/wiki/Documentation/DevGuide/Spreadsheets/Filter_Options
        echo ':44,34,0,1,'"$(libreoffice_import_format_string "$@")"
}
