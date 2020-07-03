#!/usr/bin/env bash

grep --no-filename -F 'MsgBlock' "$@" |
jq 'def katip_timestamp_to_iso8601:
      .[:-4] + "Z";
    .
    | map
      ( (.at | katip_timestamp_to_iso8601)
        as $date_iso
      | { date_iso:    $date_iso
        , timestamp:   $date_iso | fromdateiso8601
        , blkid:       .data.msg."block hash"
        , txs:         (.data.msg."tx ids" | length)
        }
      )
    | sort_by (.timestamp)
    | .[]
    ' --slurp --compact-output
