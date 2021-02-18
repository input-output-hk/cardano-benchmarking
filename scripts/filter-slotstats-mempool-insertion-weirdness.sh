#!/usr/bin/env bash

args=(
        --argjson cutoff 4
     )

jq ' select(.slTxsMemSpan > $cutoff)
   | { at:    .slStart
     , epoch: .slEpoch
     , slot:  .slEpochSlot
     , dt:    .slTxsMemSpan
     }
   '  --compact-output ${args[*]} "$@"
