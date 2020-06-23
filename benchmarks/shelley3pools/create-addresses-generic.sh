#!/usr/bin/env bash

. configuration/parameters

ID=$1

NUM_OF_ADDRESSES=${NUM_OF_ADDRESSES:-3}
CLICMD="../../bin/cardano-cli"

WORKDIR=./tmp

for i in $(seq 1 $NUM_OF_ADDRESSES)
do
    ${CLICMD} shelley address key-gen \
              --verification-key-file ${WORKDIR}/Test_${ID}/addresses/payment_$i.vkey \
              --signing-key-file $WORKDIR/Test_${ID}/addresses/payment_$i.skey

    ${CLICMD} shelley address build \
              --verification-key-file $WORKDIR/Test_${ID}/addresses/payment_$i.vkey \
              --testnet-magic ${MAGIC} > $WORKDIR/Test_${ID}/addresses/address_$i
done
