#!/bin/sh
# {"at":"2020-05-15T19:40:40.08Z","env":"1.11.0:66f0e","ns":["cardano.node.Forge"],"data":{"kind":"TraceAdoptedBlock","slot":348,"tx ids":"[txid:95d10027,txid:7eccc89d,txid:6

# jq -r 'select(.data.kind=="TraceAdoptedBlock") | [ .data.slot, .at, .data.kind, .data."block hash" ] | @csv' |

grep -h '"TraceAdoptedBlock"' $* |
  jq -r 'select(.data.kind=="TraceAdoptedBlock") | [ .data.slot, .at, .data.kind, .data."block hash", (.data."tx ids" | length), .data."block size" ] | @csv' |
  # reformat 'HashHeader'
  sed -e 's/HashHeader {unHashHeader = \([a-z0-9]\+\)}/\1/g' |
  # reformat timestamp
  sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/; s/"""/"/g' 

