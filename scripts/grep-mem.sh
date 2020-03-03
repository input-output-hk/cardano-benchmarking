#!/bin/sh

jq -r 'select(.data.kind=="LogValue" and .data.name=="Mem.resident") | [ .at, .data.name, .data.value.contents*4096 ] | @csv' $* | sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/' 

