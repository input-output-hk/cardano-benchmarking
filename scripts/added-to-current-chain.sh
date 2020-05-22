#!/usr/bin/env bash

jq ' .
   | select(.data.kind=="TraceAddBlockEvent.AddedToCurrentChain")
   | [ .at
     , .data.kind
     , try (.data.headers[0].slotNo | fromjson)
       catch -1
     ]
   | @csv' "$@" --raw-output |
        sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/'
