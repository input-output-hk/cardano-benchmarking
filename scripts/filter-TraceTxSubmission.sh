#!/usr/bin/env bash

grep -F '"TraceTxSubmission' "$@" |
jq '{ at:   .at
    , kind: .data.kind
    , tid:  .thread
    }
   ' --compact
