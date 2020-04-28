#!/usr/bin/env bash

set -e

# generator_log_prefix="${1:-generator/tx-gen}"
# explorer_log_prefix="${2:-node-on-explorer/node}"
generator_log_prefix="${1:-logs/generator}"
explorer_log_prefix="${2:-logs/node}"

mkdir -p 'analysis'
cd 'analysis'
rm -f ./*
BP=../$(dirname "$0")

sender_logs=($(ls ../${generator_log_prefix}-*.json))
receiver_logs=($(ls ../${explorer_log_prefix}-*.json))

###
### Since we're using intermediate files with intricate semantics of sorting,
### we better use more descriptive file names, let we go awry..
###
### Hence the scheme is:  FIELD[:FIELD..][-SUFFIX],SORT-FIELD-NR
### ..where FIELD are the semantic names of ';'-separated fields,
### and SORT-FIELD-NR denotes what field this file is sorted on.
### Optional SUFFIX adds further semantics.

## Field semantics
##  stx, rtix - TxId on the sender/receiver side
##  stime, rtime - send/receipt time stamp
##  blk - block number

# extract tx;timestamp pairs from generator, sort by TxId
${BP}/xsends.sh "${sender_logs[@]}"        |
        sort -k 1 -t ';'                   > stx:stime,1
count_sent="$(cat stx:stime,1 | wc -l)"

# extract tx;timestamp pairs from explorer node, sort & unique by TxId
for L in "${receiver_logs[@]}"
do ${BP}/xrecvs.sh $L
done |  sort -k 1 -t ';'                   > rtx:rtime-all,1
        sort -k 1 -t ';' -u                > rtx:rtime,1     < rtx:rtime-all,1
        sort -k 2 -t ';'                   > rtx:rtime,2     < rtx:rtime,1
count_recvd="$(cat rtx:rtime,1 | wc -l)"
count_recvd_dups="$(($(cat rtx:rtime-all,1 | wc -l) - count_recvd))"

delta_sent_recvd=$((count_sent - count_recvd))
cat <<EOF
-- Txs sent:             ${count_sent}
-- unique Txs received:  ${count_recvd}
-- Tx dups received:     ${count_recvd_dups}
-- Txs lost:             ${delta_sent_recvd}, or $((100 - (delta_sent_recvd * 100 / count_sent)))% of sent
EOF
if test "${count_sent}" != "${count_recvd}"
then join -1 1       -2 1 -v 2 -t ";" \
          stx:stime,1 rtx:rtime,1          > rtx:stime:rtime-missing,1
fi

# count distinct tx receipt stamps -- which essentially translates to blocks nos
block_stamps=($(cut -d ';' -f 2 < rtx:rtime,1 | sort -u))
blockno=1
{ for timestamp in "${block_stamps[@]}"
  do echo "${timestamp};${blockno}"
     blockno=$((blockno+1))
  done
} > rtime:blk,1,2

# join on timestamp; adds block number
join     -1 2    -2 1 -t ";" \
     rtx:rtime,2 rtime:blk,1,2             |
        sort -k 2 -t ';'                   > rtime:rtx:blk,2

# join on txid; adds receipt time
join  -1 1          -2 2 -t ";" \
  stx:stime,1 rtime:rtx:blk,2              |
        sort -k 2 -t ';'                   > stx:stime:rtime:blk,2

ln -s stx:stime:rtime:blk,2 timetoblock.lst

# clean stamps
sed -e 's/\([.0-9]\+\)Z/\1/g;s/\([0-9]\+\)T\([0-9]\+:\)/\1 \2/g;' timetoblock.lst > timetoblock.csv
