#!/bin/sh

set -ex

BP=$(dirname $0)

# extract txs from generator
${BP}/xtx.sh generator/tx-gen-*.json > txs
sort -k 1 -t ';' txs > txs1

# extract txs in blocks received
rm -f bks
for L in node-on-explorer/node-*.json; do
  ${BP}/xbk.sh $L >> bks
done

sort -k 2 -t ';' bks > bks1

# number blocks
BC=1
{ for B in `cat bks | cut -d ';' -f 2 | sort | uniq`; do
    echo "$B;$BC"
    BC=$((BC+1))
  done
} > bknum

# join on txid; adds block number
join -1 2 -2 1 -t ";" bks1 bknum > bks2
sort -k 2 -t ';' bks2 > bks3

# join on txid; adds block time
join -1 1 -2 2 -t ";" txs1 bks3 > timetoblock1.lst
sort -k 2 -t ';' timetoblock1.lst > timetoblock.lst

# clean timestamps
sed -e 's/\([.0-9]\+\)Z/\1/g;s/\([0-9]\+\)T\([0-9]\+:\)/\1 \2/g;' timetoblock.lst > timetoblock.csv

