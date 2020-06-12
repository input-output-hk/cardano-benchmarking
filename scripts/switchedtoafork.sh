#!/bin/sh

grep -h '"TraceAddBlockEvent.SwitchedToAFork"' $* |
jq -r 'select(.data.kind=="TraceAddBlockEvent.SwitchedToAFork") | [ ([.data.headers[].slotNo] | join("/")), .at, .data.kind ] | @csv' |
sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/'
