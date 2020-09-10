#!/bin/sh

grep -h '"TraceAdoptedBlock"' $* |
  jq -r 'select(.data.kind=="TraceAdoptedBlock") | [ .data.slot, .at, .data.kind, .data.blockHash, (.data.txIds | length), .data.blockSize ] | @csv' |
  # reformat timestamp
  sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/; s/"""/"/g' 

