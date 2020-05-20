#!/bin/sh

jq -r 'select(.data.kind=="TraceAddBlockEvent.AddedBlockToQueue") | [ .data.block.slot, .at, .data.kind ] | @csv' $* | sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/'
