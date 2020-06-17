#!/bin/sh

# arguments: node# [logs/node[0-9]-*.json]

NODENUM=$1
shift

grep -h '"TraceAddBlockEvent.SwitchedToAFork"' $* |
jq -r 'select(.data.kind=="TraceAddBlockEvent.SwitchedToAFork") | [ ([.data.headers[].slotNo] | join("/")), "'${NODENUM}'", .at, .data.kind ] | @csv' |
sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/'
