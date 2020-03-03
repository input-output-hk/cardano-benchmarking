#!/bin/sh

jq -r 'select(.data.kind=="LogValue" and .data.name=="Stat.utime") | [ .at, .data.name, .data.value.contents ] | @csv' $* | sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/' 

