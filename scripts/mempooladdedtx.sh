#!/bin/sh

# arguments: node# [logs/node[0-9]-*.json]
nodeid=$1
shift

grep -h '"TraceMempoolAddedTx"' $* |
jq -r 'select(.data.kind=="TraceMempoolAddedTx") | [ .data.tx.txid, .at, '$nodeid', .data.kind ] | @csv' |
sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/; s/""/"/g' |
sed -e 's/txid: TxId {_unTxId = "\([0-9a-f]\+\)"}/\1/'
