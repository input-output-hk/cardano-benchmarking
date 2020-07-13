#!/bin/sh

grep -Fh '"TraceMempoolRejectedTx"' "$@" |
jq 'def katip_timestamp_to_iso8601:
      .[:-4] + "Z";
    .
    | map
      ( (.at | katip_timestamp_to_iso8601)
        as $date_iso
      | { date_iso:    $date_iso
        , timestamp:   ($date_iso | fromdateiso8601)
        , host:        .host
        , txid:        .data.tx.txid[5:]
        , err:         .data.err
        })
    | sort_by (.timestamp)
    | .[]
    ' --slurp --compact-output
