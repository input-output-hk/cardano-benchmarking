#!/bin/sh

# arguments: node# [logs/node[0-9]-*.json]

NODENUM=$1
shift

grep -h '"TraceStartLeadershipCheck".*utxoSize' $* |
  jq -r 'select(.data.kind=="TraceStartLeadershipCheck") | [ .data.slot, "'${NODENUM}'", .at, .data.utxoSize, .data.kind ] | @csv' |
  sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/'
