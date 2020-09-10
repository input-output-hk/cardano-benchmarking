#!/bin/sh

# arguments: node# [logs/node[0-9]-*.json]

NODENUM=$1
shift

TEMPFILE=$(mktemp)

  # grep from all log files passed in as arguments
grep -h '"TraceAdoptedBlock"' $* |
  # reformat string of tx ids as list
  sed -e 's/\\"//g;s/"txid: TxId {_unTxId = \([0-9a-f]\+\)}"\(,\{0,1\}\)/"\1"\2/g' | 
  # get rid of constructors, extract tx hash
  sed -e 's/"[^"]\+_unTxId = \([a-z0-9]\+\)[^"]\+"/"\1"/g' |
  # extract specific data
  jq -cr 'select(.data.kind=="TraceAdoptedBlock") | [ .data.slot, .at, .data.kind, .data.blockHash, .data.blockSize, (.data.txIds | length), .data.txIds ]' |
  # reformat timestamp
  sed -e 's/\([0-9-]\+\)T\([0-9:.]\+\)Z/\1 \2/;s/\\"//g' > ${TEMPFILE}

OLDIFS=$IFS
cat "${TEMPFILE}" | {
  read ln
  while [ -n "${ln}" ]; do

    SLOT=$(echo $ln | sed -ne 's/^\[\([0-9]\+\),"\([^"]\+\)","[^"]*","[^"]*",[0-9]\+,[0-9]\+,\[\(.*\)\]\]$/\1/p')
    TSTAMP=$(echo $ln | sed -ne 's/^\[\([0-9]\+\),"\([^"]\+\)","[^"]*","[^"]*",[0-9]\+,[0-9]\+,\[\(.*\)\]\]$/\2/p')
    TXIDS=$(echo $ln | sed -ne 's/^\[\([0-9]\+\),"\([^"]\+\)","[^"]*","[^"]*",[0-9]\+,[0-9]\+,\[\(.*\)\]\]$/\3/p;' |
                       sed -e 's/"[^"]*_unTxId = \([a-f0-9]\+\)[^"]*"/\1/g')
    if [ -n "$TXIDS" ]; then
      IFS=","
      for txid in ${TXIDS}; do
        echo "${txid},\"${TSTAMP}\",${NODENUM},${SLOT}"
      done
    fi

    IFS=$OLDIFS
    read ln
  done
}

exit 0
