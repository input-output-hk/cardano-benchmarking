#!/usr/bin/env bash

. configuration/parameters

NUM_OF_ADDRESSES=${NUM_OF_ADDRESSES:-3}
CLICMD="../../bin/cardano-cli"

WORKDIR=./tmp

for i in $(seq 1 $NUM_OF_ADDRESSES)
do
    ${CLICMD} shelley address key-gen \
              --verification-key-file ${WORKDIR}/addresses/payment_$i.vkey \
              --signing-key-file $WORKDIR/addresses/payment_$i.skey

    ${CLICMD} shelley address build \
              --verification-key-file $WORKDIR/addresses/payment_$i.vkey \
              --testnet-magic ${MAGIC} > $WORKDIR/addresses/address_$i
done
