#!/usr/bin/env bash

grep --no-filename -F 'MsgBlock' "$@" |
jq ' map ( { at:    .at
           , blkid: .data.msg.blkid
           , txs:  (.data.msg.txids | length)
           }
         )
   | sort_by (.at)
   | .[]
   ' --slurp --compact-output
