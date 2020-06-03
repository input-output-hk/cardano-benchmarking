#!/usr/bin/env bash

grep --no-filename -F 'MsgBlock' "$@" |
jq ' map ( { at:    .at
           , blkid: .data.msg."block hash"
           , blksz: .data.msg."block size"
           , txs:  (.data.msg."tx ids" | length)
           }
         )
   | sort_by (.at)
   | .[]
   ' --slurp --compact-output
