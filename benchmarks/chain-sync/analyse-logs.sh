#!/bin/sh

NETWORK=${1:-mainnet}

BASEPATH=$(realpath $(dirname $0))

# get first block copy time
LOGFILE_JSON=`ls -1 state-node-${NETWORK}/node-0-*.json | head -2`
FIRSTSLOT=`jq -r 'select(.data.kind=="TraceAddBlockEvent.AddedToCurrentChain") | [ .at, .data.kind, .data.newtip ] | @csv' $LOGFILE_JSON | sed -e 's/"[A-Za-f0-9]\+@\([0-9]\+\)"/\1/;' | sort -k 1.2,1.23 | head -n 1 | sed -e 's/^\("[0-9]\+-[0-9]\+-[0-9]\+\)T\([0-9]\+:[0-9]\+:[0-9]\+[.][0-9]\+\)Z\(.*\)$/\1 \2\3/'`
FIRSTNETIN=`jq -r 'select(.data.kind=="LogValue" and .data.name=="Net.IpExt:InOctets") | [ .at, .data.name, .data.value.contents ] | @csv' $LOGFILE_JSON | sort -k 1.2,1.23 | head -n 1 | sed -e 's/^"\(.*\)"$/\1/;s/\\"/"/g;s/^\("[0-9]\+-[0-9]\+-[0-9]\+\)T\([0-9]\+:[0-9]\+:[0-9]\+[.][0-9]\+\)Z\(.*\)$/\1 \2\3/'`
FIRSTNETOUT=`jq -r 'select(.data.kind=="LogValue" and .data.name=="Net.IpExt:OutOctets") | [ .at, .data.name, .data.value.contents ] | @csv' $LOGFILE_JSON | sort -k 1.2,1.23 | head -n 1 | sed -e 's/^"\(.*\)"$/\1/;s/\\"/"/g;s/^\("[0-9]\+-[0-9]\+-[0-9]\+\)T\([0-9]\+:[0-9]\+:[0-9]\+[.][0-9]\+\)Z\(.*\)$/\1 \2\3/'`

LOGFILE_JSON=`ls -1r state-node-${NETWORK}/node-0-*.json | head -2`
echo $LOGFILE_JSON
LASTSLOT=`jq -r 'select(.data.kind=="TraceAddBlockEvent.AddedToCurrentChain") | [ .at, .data.kind, .data.newtip ] | @csv' $LOGFILE_JSON | sort -k 1.2,1.23 | tail -1 | sed -e 's/"[A-Za-f0-9]\+@\([0-9]\+\)"/\1/;s/^\("[0-9]\+-[0-9]\+-[0-9]\+\)T\([0-9]\+:[0-9]\+:[0-9]\+[.][0-9]\+\)Z\(.*\)$/\1 \2\3/'`
LASTRSS=`jq -r 'select(.data.kind=="LogValue" and .data.name=="Mem.resident") | [ .at, .data.name, .data.value.contents * 4096 ] | @csv' $LOGFILE_JSON | sort -k 1.2,1.23 | tail -1 | sed -e 's/^"\(.*\)"$/\1/;s/\\"/"/g;s/^\("[0-9]\+-[0-9]\+-[0-9]\+\)T\([0-9]\+:[0-9]\+:[0-9]\+[.][0-9]\+\)Z\(.*\)$/\1 \2\3/'`
LASTNETIN=`jq -r 'select(.data.kind=="LogValue" and .data.name=="Net.IpExt:InOctets") | [ .at, .data.name, .data.value.contents ] | @csv' $LOGFILE_JSON | sort -k 1.2,1.23 | tail -1 | sed -e 's/^"\(.*\)"$/\1/;s/\\"/"/g;s/^\("[0-9]\+-[0-9]\+-[0-9]\+\)T\([0-9]\+:[0-9]\+:[0-9]\+[.][0-9]\+\)Z\(.*\)$/\1 \2\3/'`
LASTNETOUT=`jq -r 'select(.data.kind=="LogValue" and .data.name=="Net.IpExt:OutOctets") | [ .at, .data.name, .data.value.contents ] | @csv' $LOGFILE_JSON | sort -k 1.2,1.23 | tail -1 | sed -e 's/^"\(.*\)"$/\1/;s/\\"/"/g;s/^\("[0-9]\+-[0-9]\+-[0-9]\+\)T\([0-9]\+:[0-9]\+:[0-9]\+[.][0-9]\+\)Z\(.*\)$/\1 \2\3/'`
LASTDISKIN=`jq -r 'select(.data.kind=="LogValue" and .data.name=="IO.rbytes") | [ .at, .data.name, .data.value.contents ] | @csv' $LOGFILE_JSON | sort -k 1.2,1.23 | tail -1 | sed -e 's/^"\(.*\)"$/\1/;s/\\"/"/g;s/^\("[0-9]\+-[0-9]\+-[0-9]\+\)T\([0-9]\+:[0-9]\+:[0-9]\+[.][0-9]\+\)Z\(.*\)$/\1 \2\3/'`
LASTDISKOUT=`jq -r 'select(.data.kind=="LogValue" and .data.name=="IO.wbytes") | [ .at, .data.name, .data.value.contents ] | @csv' $LOGFILE_JSON | sort -k 1.2,1.23 | tail -1 | sed -e 's/^"\(.*\)"$/\1/;s/\\"/"/g;s/^\("[0-9]\+-[0-9]\+-[0-9]\+\)T\([0-9]\+:[0-9]\+:[0-9]\+[.][0-9]\+\)Z\(.*\)$/\1 \2\3/'`

# output git revision
echo -n "cardano-benchmarking commit;"
git log | head -1 | cut -d ' ' -f 2
echo -n "slotfirst,"; echo $FIRSTSLOT
echo -n "slotlast,"; echo $LASTSLOT
echo -n "memorylast,"; echo $LASTRSS
echo -n "netinfirst,"; echo $FIRSTNETIN
echo -n "netinlast,"; echo $LASTNETIN
echo -n "netoutfirst,"; echo $FIRSTNETOUT
echo -n "netoutlast,"; echo $LASTNETOUT
echo -n "diskinlast,"; echo $LASTDISKIN
echo -n "diskoutlast,"; echo $LASTDISKOUT

