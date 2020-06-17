#!/usr/bin/env bash

BASEDIR=$(realpath $(dirname "$0"))
. ${BASEDIR}/../../scripts/common.sh
. ${BASEDIR}/configuration/parameters

set -e

CLICMD=${CLICMD:-"run cardano-cli"}

export CARDANO_NODE_SOCKET_PATH=${CARDANO_NODE_SOCKET_PATH:-"./logs/sockets/1"}
WORKDIR=./tmp-exgenesis

# Initial set-up
mkdir -p ${WORKDIR}/txs

## accessing genesis
GENESIS_UTXO_IDX=3
VKEY=configuration/genesis/utxo-keys/utxo${GENESIS_UTXO_IDX}.vkey
SKEY=configuration/genesis/utxo-keys/utxo${GENESIS_UTXO_IDX}.skey


# Fund address from Genesis
if [ ! -e ${WORKDIR}/genesis_utxo ]; then
  echo "Accessing genesis TxIn"
  ## Get the initial UTxO TxIn
  ${CLICMD} shelley genesis initial-txin \
            --verification-key-file ${VKEY} \
            --testnet-magic ${MAGIC} > ${WORKDIR}/genesis_utxo
else 
  echo "already have ${WORKDIR}/genesis_utxo"
fi

## Set-up the Payer
if [ ! -e ${WORKDIR}/payer.addr ]; then
  echo "Creating the payer address"
  ### Create the keys and addresses for Payer
  ${CLICMD} shelley address key-gen \
      --verification-key-file ${WORKDIR}/payer.vkey \
      --signing-key-file ${WORKDIR}/payer.skey

  ${CLICMD} shelley address build \
            --testnet-magic ${MAGIC} \
            --payment-verification-key-file ${WORKDIR}/payer.vkey > ${WORKDIR}/payer.addr
else 
  echo "already have ${WORKDIR}/payer.addr"
fi

if [ ! -e ${WORKDIR}/txs/genesis_to_funding.tx ]; then
  echo "Create funding transaction"
  ### Build, Sign, Submit a Genesis UTxO to the Payer
  ${CLICMD} shelley transaction build-raw \
      --tx-in  `cat ${WORKDIR}/genesis_utxo`#0 \
      --tx-out `cat ${WORKDIR}/payer.addr`+$(( ${SUPPLY} / ${NNODES} )) \
      --ttl 10000 \
      --fee 0 \
      --tx-body-file ${WORKDIR}/txs/genesis_to_funding.txbody

  echo "Sign funding transaction"
  ${CLICMD} shelley transaction sign \
      --tx-body-file ${WORKDIR}/txs/genesis_to_funding.txbody \
      --signing-key-file ${SKEY} \
      --testnet-magic ${MAGIC} \
      --tx-file ${WORKDIR}/txs/genesis_to_funding.tx
else 
  echo "already have ${WORKDIR}/txs/genesis_to_funding.tx"
fi

if [ ! -e ${WORKDIR}/txs/genesis_to_funding.tx-submitted ]; then
  echo "Submitting funding transaction"
  ${CLICMD} shelley transaction submit \
      --tx-file ${WORKDIR}/txs/genesis_to_funding.tx \
      --testnet-magic ${MAGIC}
  touch ${WORKDIR}/txs/genesis_to_funding.tx-submitted
else 
  echo "already have ${WORKDIR}/txs/genesis_to_funding.tx-submitted"
fi


if [ ! -e ${WORKDIR}/payer_utxo_0 ]; then
  echo "Waiting for the UTxO to appear on-chain... (this will take ~15 seconds)"
  COUNT=0
  while [ $COUNT -lt 5 ]; do
    sleep 2

    # Get the initial UTxO
    PAYER_UTXO=$(${CLICMD} shelley query utxo \
        --address `cat ${WORKDIR}/payer.addr` \
        --testnet-magic ${MAGIC} | grep 0 | cut -f1 -d ' ' | sed 's/$/#0/g')
    if [ -n "${PAYER_UTXO}" ]; then
      echo "${PAYER_UTXO}" > ${WORKDIR}/payer_utxo_0
      COUNT=99
    fi

    COUNT=$((COUNT + 1))
  done
fi

if [ -e ${WORKDIR}/payer_utxo_0 ]; then
  echo "payer UTxO:"
  cat ${WORKDIR}/payer_utxo_0
fi

