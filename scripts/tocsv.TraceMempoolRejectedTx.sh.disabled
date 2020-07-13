#!/bin/sh

jq 'def nodename_to_index:
      .[5:];

    [ .date_iso
    , (.host | nodename_to_index)
    , .txid
    , .err
    ]
   | join(",")
   ' --raw-output "$@" |
sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/; s_"__g'
