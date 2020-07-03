#!/bin/sh

jq '[ .timestamp, .kind, .slot | tostring ]
   | @csv
   ' --raw-output "$@" |
sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/; s_"__g'
