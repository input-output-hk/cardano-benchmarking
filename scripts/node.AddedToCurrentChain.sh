#!/bin/sh

grep -Fh '"TraceAddBlockEvent.AddedToCurrentChain"' "$@" |
jq '.
   | { timestamp: .at
     , host:      .host
     , slot:      .data.headers[0].slotNo
     , kind:      .data.kind
     }
   ' --compact-output
