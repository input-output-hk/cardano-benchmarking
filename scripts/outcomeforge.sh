#!/bin/sh

# arguments: node# [logs/node[0-9]-*.json]

NODENUM=$1
shift

grep -h '"OutcomeTraceForgeEvent"' $* |
  jq -r 'select(.data.kind=="OutcomeTraceForgeEvent") | [ .data.slot, "'${NODENUM}'", .at, .data.kind, .data.difftime, .data.mempoolbytes, .data.mempoolnumtx ] | @csv' |
  # reformat timestamp
  sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/' 

