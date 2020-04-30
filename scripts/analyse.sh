#!/usr/bin/env bash
# shellcheck disable=SC2016,SC2207,SC2002

set -e

# generator_log_prefix="${1:-generator/tx-gen}"
# explorer_log_prefix="${2:-node-on-explorer/node}"
generator_log_prefix="${1:-logs/generator}"
explorer_log_prefix="${2:-logs/node}"

mkdir -p 'analysis'
cd 'analysis'
rm -f ./*
BP=../$(dirname "$0")

sender_logs=($(ls ../"${generator_log_prefix}"-*.json))
receiver_logs=($(ls ../"${explorer_log_prefix}"-*.json))

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
"${BP}"/xsends.sh "${sender_logs[@]}"      |
        sort -k 1 -t ';'                   > stx_stime.1
        sort -k 2 -t ';'                   > stx_stime.2     < stx_stime.1
count_sent="$(cat stx_stime.1 | wc -l)"

# extract tx;timestamp pairs from explorer node, sort & unique by TxId
for L in "${receiver_logs[@]}"
do "${BP}"/xrecvs.sh "$L"
done |  sort -k 2 -t ';'                   > rtx_rtime-with-dups.1
        sed 's_^\([^;]*\);\(.*\)$_\2;\1_'  < rtx_rtime-with-dups.1 |
        sort -k 2 -t ';' -u                |  ## W/around for apparent bug in sort.
        sed 's_^\([^;]*\);\(.*\)$_\2;\1_'  > rtx_rtime.1
        sort -k 2 -t ';'                   > rtx_rtime.2     < rtx_rtime.1
count_recvd="$(cat rtx_rtime.1 | wc -l)"
#count_recvd_dups="$(($(cat rtx_rtime-with-dups,1 | wc -l) - count_recvd))"

cat <<EOF
-- Txs sent:             ${count_sent}
-- Unique Txs received:  ${count_recvd}
-- Sends:                analysis/stx_stime.1
-- Receipts:             analysis/rtx_rtime.1
EOF
if test "${count_sent}" != "${count_recvd}"
then join -1 1       -2 1 -v 1 -t ";" \
          stx_stime.1 rtx_rtime.1          > rtx_stime_rtime-missing.1
     join -1 1       -2 1 -v 2 -t ";" \
          stx_stime.1 rtx_rtime.1          > rtx_stime_rtime-martian.1
     missing=$(cat rtx_stime_rtime-missing.1 | wc -l)
     martian=$(cat rtx_stime_rtime-martian.1 | wc -l)
     cat <<EOF
-- Lost Txs:             ${missing}: or $(((missing * 100 / count_sent)))% of sent
-- Martian Txs:          ${martian}: or $(((martian * 100 / count_recvd)))% of received
-- Missing sends:        analysis/rtx_stime_rtime-missing.1
-- Martian receipts:     analysis/rtx_stime_rtime-martian.1
EOF
fi

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
