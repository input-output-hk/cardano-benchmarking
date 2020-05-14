#!/bin/sh

grep -h '"TraceAddBlockEvent.AddedToCurrentChain"' $* |
jq -r 'select(.data.kind=="TraceAddBlockEvent.AddedToCurrentChain") | [ .data.headers[0].slotNo, .at, .data.kind ] | @csv' |
sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/'
