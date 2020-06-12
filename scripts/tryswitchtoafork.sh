#!/bin/sh

grep -h '"TraceAddBlockEvent.TrySwitchToAFork"' $* |
jq -r 'select(.data.kind=="TraceAddBlockEvent.TrySwitchToAFork") | [ .data.block.slot, .at, .data.kind ] | @csv' |
sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/'
