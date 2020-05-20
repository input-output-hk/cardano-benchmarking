#!/usr/bin/env bash
# shellcheck disable=SC2016,SC2207,SC2002

set -e

# generator_log_prefix="${1:-generator/tx-gen}"
# explorer_log_prefix="${2:-node-on-explorer/node}"
generator_log_prefix="${1:-logs/generator.}"
explorer_log_prefix="${2:-logs/node}"
extra_prefix="${3}"

rm -rf ./'analysis'
mkdir -p 'analysis'
cd 'analysis'
BP=../$(dirname "$0")

## NOTE:  Keep the pattern as-is:
##        it allows matching 'node.json' with 'node' passed as prefix.
sender_logs=($(ls ../"${generator_log_prefix}"*json))
receiver_logs=($(ls ../"${explorer_log_prefix}"*json))

# Given a JSON log from a Tx generator with configuration, where:
#
#   minSeverity: Debug
#   TracingVerbosity: MaximalVerbosity
#
# ..extract the list of submitted transactions,
# as soon as we see them being requested by the remote peer:
#
#   | TraceBenchTxSubServReq [txid]
#   -- ^ Request for @tx@ recieved from `TxSubmit.TxSubmission` protocol
#   --   peer.
extract_sends() {
        local msgty=$1; shift
        jq '
          select (.data.kind == "'"${msgty}"'")
        | .at as $at       # bind timestamp
        | .data.txIds      # narrow to the txid list
        | map ( .[5:]          # cut the "txid: txid: " prefix
              | "\(.);\($at)") # produce the resulting string
        | .[]              # merge string lists over all messages
        ' "$@" |
        tr -d '"'
}

# Given a JSON log from a node with configuration, where:
#
#   minSeverity: Debug
#   TracingVerbosity: MaximalVerbosity
#   TraceBlockFetchClient: True
#
# ..extract the list of incoming transactions,
# as soon as we see them coming in a via BlockFetch protocol.
extract_recvs() {
        jq '
          select (.data.kind == "Recv" and .data.msg.kind == "MsgBlock")
        | .at as $at       # bind timestamp
        | .data.msg.txids  # narrow to the txid list
        | map ( .[5:]          # cut the "txid: txid: " prefix
              | "\(.);\($at)") # produce the resulting string
        | .[]              # merge string lists over all messages
        ' $1 |
        tr -d '"'
}

###
### Since we're using intermediate files with intricate semantics of sorting,
### we better use more descriptive file names, lest we risk going awry..
###
### Hence the scheme is:  FIELD[_FIELD..][-SUFFIX],SORT-FIELD-NR
### ..where FIELD are the semantic names of ';'-separated fields,
### and SORT-FIELD-NR denotes what field this file is sorted on.
### Optional SUFFIX adds further semantics.

## Field semantics
##  stx, rtix - TxId on the sender/receiver side
##  stime, rtime - send/receipt time stamp
##  blk - block number

# extract tx;timestamp pairs from generator, sort by TxId
extract_sends 'TraceBenchTxSubServAnn' "${sender_logs[@]}" |
        sort -k 1 -t ';'                   > atx_atime.1
        sort -k 2 -t ';'                   > atx_atime.2     < atx_atime.1
count_annced="$(cat atx_atime.1 | wc -l)"

extract_sends 'TraceBenchTxSubServAck' "${sender_logs[@]}" |
        sort -k 1 -t ';'                   > ktx_ktime.1
        sort -k 2 -t ';'                   > ktx_ktime.2     < ktx_ktime.1
count_acked="$(cat ktx_ktime.1 | wc -l)"

extract_sends 'TraceBenchTxSubServDrop' "${sender_logs[@]}" |
        sort -k 1 -t ';'                   > dtx_dtime.1
        sort -k 2 -t ';'                   > dtx_dtime.2     < dtx_dtime.1
count_dropped="$(cat dtx_dtime.1 | wc -l)"

extract_sends 'TraceBenchTxSubServReq' "${sender_logs[@]}" |
        sort -k 1 -t ';'                   > stx_stime.1
        sort -k 2 -t ';'                   > stx_stime.2     < stx_stime.1
count_sent="$(cat stx_stime.1 | wc -l)"

# extract tx;timestamp pairs from explorer node, sort & unique by TxId
for L in "${receiver_logs[@]}"
do extract_recvs "$L"
done |  sort -k 2 -t ';'                   > rtx_rtime-with-dups.1
        sed 's_^\([^;]*\);\(.*\)$_\2;\1_'  < rtx_rtime-with-dups.1 |
        sort -k 2 -t ';' -u                |  ## W/around for apparent bug in sort.
        sed 's_^\([^;]*\);\(.*\)$_\2;\1_'  > rtx_rtime.1
        sort -k 2 -t ';'                   > rtx_rtime.2     < rtx_rtime.1
count_recvd="$(cat rtx_rtime.1 | wc -l)"
#count_recvd_dups="$(($(cat rtx_rtime-with-dups,1 | wc -l) - count_recvd))"

cat <<EOF
-- Txs announced:        ${count_annced}
-- Txs sent:             ${count_sent}
-- Txs acked:            ${count_acked}
-- Txs dropped:          ${count_dropped}
-- Unique Txs received:  ${count_recvd}
-- Announces:            analysis/atx_atime.1
-- Sends:                analysis/stx_stime.1
-- Acks:                 analysis/ktx_ktime.1
-- Drops:                analysis/dtx_dtime.1
-- Receipts:             analysis/rtx_rtime.1
EOF

count_missing=0 count_martian=0
if test "${count_sent}" != "${count_recvd}"
then join -1 1       -2 1 -v 1 -t ";" \
          stx_stime.1 rtx_rtime.1          > rtx_stime-missing.1
     sort -k 2 -t ';'                      > rtx_stime-missing.2 < rtx_stime-missing.1
     count_missing=$(cat rtx_stime-missing.1 | wc -l)
     if test "${count_missing}" -eq 0
     then rm -f rtx_*-missing.*
     else cat <<EOF
-- Lost Txs:             ${count_missing}: or $(((count_missing * 100 / count_sent)))% of sent
-- Missing sends:        ${extra_prefix}analysis/rtx_stime-missing.1 ${extra_prefix}analysis/rtx_stime-missing.2
EOF
     fi

     join -1 1       -2 1 -v 2 -t ";" \
          stx_stime.1 rtx_rtime.1          > rtx_rtime-martian.1
     count_martian=$(cat rtx_rtime-martian.1 | wc -l)
     if test "${count_martian}" -eq 0
     then rm -f rtx_*-martian.*
     else cat <<EOF
-- Martian Txs:          ${count_martian}: or $(((count_martian * 100 / count_recvd)))% of received
-- Martian receipts:     ${extra_prefix}analysis/rtx_rtime-martian.1
EOF
     fi
fi

jq > tx-stats.json --null-input '
{ "tx_generated":      '${count_sent}'
, "tx_seen_in_blocks": '${count_recvd}'
, "tx_missing":        '${count_missing}'
, "tx_martian":        '${count_martian}'
}'

# count distinct tx receipt stamps -- which essentially translates to blocks nos
block_stamps=($(cut -d ';' -f 2 < rtx_rtime.1 | sort -u))
blockno=1
{ for timestamp in "${block_stamps[@]}"
  do echo "${timestamp};${blockno}"
     blockno=$((blockno+1))
  done
} > rtime_blk.1.2

# join on timestamp; adds block number
join     -1 2    -2 1 -t ";" \
     rtx_rtime.2 rtime_blk.1.2             |
        sort -k 2 -t ';'                   > rtime_rtx_blk.2

# join on txid; adds receipt time
join  -1 1          -2 2 -t ";" \
  stx_stime.1 rtime_rtx_blk.2              |
        sort -k 2 -t ';'                   > stx_stime_rtime_blk.2

ln -s stx_stime_rtime_blk.2 timetoblock.lst

# clean stamps
sed -e 's/\([.0-9]\+\)Z/\1/g;s/\([0-9]\+\)T\([0-9]\+:\)/\1 \2/g;' timetoblock.lst > timetoblock.csv
