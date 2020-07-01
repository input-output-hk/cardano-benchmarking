#!/usr/bin/env bash

OUTDIR=$1

set -e

pushd ${OUTDIR}

## Described in:
## https://wiki.openoffice.org/wiki/Documentation/DevGuide/Spreadsheets/Filter_Options
declare -A conversion_options
conversion_options=(
        ['addtochain']=':44,34,0,1,1/1/2/1/3/5/4/2'
        ['adopted']=':44,34,0,1,1/1/2/1/3/5/4/2/5/1/6/1'
        ['cpu']=':44,34,0,1,1/5/2/2/3/1'
        ['genesis_parameters']=':44,34,0,1,1/1/2/1/3/1/4/1/5/1/6/1/7/1/8/1/9/1/10/1'
        ['leader']=':44,34,0,1,1/1/2/1/3/5/4/2'
        ['mem']=':44,34,0,1,1/5/2/2/3/1'
        ['outcome']=':44,34,0,1,1/1/2/1/3/5/4/2/5/1/6/1/7/1'
        ['switchedtoafork']=':44,34,0,1,1//2//3/'
        ['timeline']=':44,34,0,1,1/1/2/1/3/1/4/2/5/1/6/1/7/1'
        ['tryswitchtoafork']=':44,34,0,1,1//2//3/'
        ['txadopted']=':44,34,0,1,1/1/2/1/3/5/4/2'
        ['txmempool']=':44,34,0,1,1/1/2/2/3/5/4/2'
        ['txrejected']=':44,34,0,1,1/1/2/2/3/5/4/2'
        ['utxosize']=':44,34,0,1,1/1/2/1/3/5/4/1/5/2'
)

rm -f -- *.ods
for csv in *.csv
do key=$(echo $csv | sed 's_[\.-].*$__g')
   file_options=${conversion_options[${key}]}
   if test -z "$file_options"
   then echo "ERROR:  no converssion_options entry for CSV file $csv (key $key)" >&2
        exit 1
   else echo "key $key options $file_options file $csv"
   fi
   libreoffice --infilter="csv${file_options}" \
       --convert-to ods \
       --outdir . \
       "$csv" 2>&1 |
   grep -v 'Theme parsing error: gtk.css\|^$'
done

report_name=report #$(basename "$OUTDIR")
rm -f                ${report_name}.ods
set -x
ssconvert --merge-to ${report_name}.ods *.ods

