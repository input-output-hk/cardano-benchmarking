#!/usr/bin/env bash

grep --no-filename -F 'TraceAdoptedBlock' "$@" |
jq ' map ( { at:    .at
           , blkid: .data.blockHash
           , blksz: .data.blockSize
           , txs:  (.data.txIds | length)
           }
         )
   | sort_by (.at)
   | .[]
   ' --slurp --compact-output
