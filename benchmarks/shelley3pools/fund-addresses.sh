#!/usr/bin/env bash
# set -x

echo "Starting..."
echo "Importing and initialising variables..."

BASEDIR=$(realpath $(dirname "$0"))
. ${BASEDIR}/../../scripts/common.sh
. ${BASEDIR}/configuration/parameters

CLICMD=${CLICMD:-"run cardano-cli"}

export CARDANO_NODE_SOCKET_PATH=${CARDANO_NODE_SOCKET_PATH:-"./logs/sockets/1"}
NUM_OF_ADDRESSES=${NUM_OF_ADDRESSES:-3}
WORKDIR=./tmp

# Initial set-up
if [ -d ${WORKDIR} ]; then rm -rf ./${WORKDIR}; fi
mkdir -p ./${WORKDIR}/txs
mkdir -p ./${WORKDIR}/addresses

echo "Creating the payer account..."

# Fund address from Genesis
## Get the initial UTxO TxIn
${CLICMD} shelley genesis initial-txin \
          --verification-key-file configuration/genesis/utxo-keys/utxo3.vkey \
          --testnet-magic ${MAGIC} > ${WORKDIR}/genesis_utxo

## Set-up the Payer
### Create the keys and addresses for Payer
${CLICMD} shelley address key-gen \
    --verification-key-file ${WORKDIR}/payer.vkey \
    --signing-key-file ${WORKDIR}/payer.skey

${CLICMD} shelley address build \
          --testnet-magic ${MAGIC} \
          --payment-verification-key-file ${WORKDIR}/payer.vkey > ${WORKDIR}/payer.addr

echo "Funding the payer account..."

### Build, Sign, Submit a Genesis UTxO to the Payer
${CLICMD} shelley transaction build-raw \
    --tx-in  `cat ${WORKDIR}/genesis_utxo`#0 \
    --tx-out `cat ${WORKDIR}/payer.addr`+$(( ${SUPPLY} / ${NNODES} )) \
    --ttl 10000 \
    --fee 0 \
    --tx-body-file ${WORKDIR}/txs/genesis_to_funding.txbody

${CLICMD} shelley transaction sign \
    --tx-body-file ${WORKDIR}/txs/genesis_to_funding.txbody \
    --signing-key-file configuration/genesis/utxo-keys/utxo3.skey \
    --testnet-magic ${MAGIC} \
    --tx-file ${WORKDIR}/txs/genesis_to_funding.tx

${CLICMD} shelley transaction submit \
    --tx-file ${WORKDIR}/txs/genesis_to_funding.tx \
    --testnet-magic ${MAGIC}

echo "Waiting for the UTxO to appear on-chain... (this will take ~15 seconds)"

sleep 15

# Get the initial UTxO
${CLICMD} shelley query utxo \
    --address `cat ${WORKDIR}/payer.addr` \
    --testnet-magic ${MAGIC} | grep 0 | cut -f1 -d ' ' | sed 's/$/#0/g' > ${WORKDIR}/payer_utxo_0

echo "Creating ${NUM_OF_ADDRESSES} addresses..."

# Create n target addresses
time ./create-addresses.sh
echo ""

# Set-up variables for calculating change
let "payer_ada = ${SUPPLY} / ${utxo_keys}"
STD_TX=${txvalue}
STD_FEE=${txfee}

echo "Generating ${NUM_OF_ADDRESSES} transactions..."

time for i in $(seq 1 ${NUM_OF_ADDRESSES})
do
    # Calculate change
    let "payer_ada-=${STD_TX}"
    let "payer_ada-=${STD_FEE}"

    # Build n transactions
    ${CLICMD} shelley transaction build-raw \
        --tx-in `cat ${WORKDIR}/payer_utxo_$((${i} - 1))` \
        --tx-out `cat ${WORKDIR}/addresses/address_${i}`+${STD_TX} \
        --tx-out `cat ${WORKDIR}/payer.addr`+${payer_ada} \
        --ttl 100000 \
        --fee ${STD_FEE} \
        --out-file ${WORKDIR}/txs/tx_${i}.raw

    # Sign n transactions
    ${CLICMD} shelley transaction sign \
        --tx-body-file ${WORKDIR}/txs/tx_${i}.raw \
        --signing-key-file ${WORKDIR}/payer.skey \
        --testnet-magic ${MAGIC} \
        --out-file ${WORKDIR}/txs/tx_${i}.signed

    # Get the UTxO of the transaction for the input of the subsequent transaction
    ${CLICMD} shelley transaction txid --tx-body-file ${WORKDIR}/txs/tx_${i}.raw | sed 's/$/#1/g'> ${WORKDIR}/payer_utxo_${i}
done

echo""
echo "Submitting ${NUM_OF_ADDRESSES} transactions..."

time for i in $(seq 1 ${NUM_OF_ADDRESSES})
do
    # Submit n transactions
    ${CLICMD} shelley transaction submit \
          --tx-file ${WORKDIR}/txs/tx_${i}.signed \
          --testnet-magic ${MAGIC}
done

echo""
echo "Finished!"
