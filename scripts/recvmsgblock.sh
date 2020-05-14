#!/bin/sh

grep -h '"MsgBlock"' $* |
jq -r 'select(.data.kind=="Recv" and .data.msg.kind=="MsgBlock") | [ .at, .data.kind, .data.msg.blkid, (.data.msg.txids | length), .data.peer ] | @csv' |
sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/' 

