#!/bin/sh

# arguments: node# [logs/node[0-9]-*.json]
nodeid=$1
shift

grep -h '"TraceMempoolRejectedTx"' $* |
jq -r 'select(.data.kind=="TraceMempoolRejectedTx") | [ "'$nodeid'", .data.tx.txid, .at, .data.kind ] | @csv' |
sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/' |
sed -e 's/txid: TxId {_TxId = \([0-9a-f]\+\)}/\1/'
