#!/usr/bin/env bash

jq -c '
  map ( (.data.kind // "no .data.kind")
        as $kind1
      | (if (try (.data.msg.kind // "") catch "") != ""
            then "." + .data.msg.kind else "" end)
        as $kind2
      | { kind: ($kind1 + $kind2), namespace: .ns[0] })
  | unique
  | .[]' --slurp --raw-output "$@"
