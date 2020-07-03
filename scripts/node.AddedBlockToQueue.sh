#!/bin/sh

grep -Fh '"TraceAddBlockEvent.AddedBlockToQueue"' "$@" |
jq '.
   | { timestamp: .at
     , host:      .host
     , slot:      .data.block.slot
     , kind:      .data.kind
     }
   ' --compact-output
