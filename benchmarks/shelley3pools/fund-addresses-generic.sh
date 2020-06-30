#!/usr/bin/env bash
# set -x

echo "Importing and initialising variables..."

BASEDIR=$(realpath $(dirname "$0"))
. ${BASEDIR}/../../scripts/common.sh
. ${BASEDIR}/configuration/parameters

ID=$1
UTXO_ID=$2
SOCKET_ID=$3

export CARDANO_NODE_SOCKET_PATH="./logs/sockets/${SOCKET_ID}"
NUM_OF_ADDRESSES=${NUM_OF_ADDRESSES:-3}
WORKDIR=./tmp

# Initial set-up
if [ -d ${WORKDIR} ]; then rm -rf ./${WORKDIR}; fi
mkdir -p ./${WORKDIR}/Test_${ID}/txs
mkdir -p ./${WORKDIR}/Test_${ID}/addresses

echo "Creating Payer ${ID}s account..."

# Fund address from Genesis
## Get the initial UTxO TxIn
${CLICMD} shelley genesis initial-txin \
          --verification-key-file configuration/genesis/utxo-keys/utxo${UTXO_ID}.vkey \
          --testnet-magic ${MAGIC} > ${WORKDIR}/Test_${ID}/genesis_utxo${ID}

## Set-up the Payer
### Create the keys and addresses for Payer
${CLICMD} shelley address key-gen \
    --verification-key-file ${WORKDIR}/Test_${ID}/payer${ID}.vkey \
    --signing-key-file ${WORKDIR}/Test_${ID}/payer${ID}.skey

${CLICMD} shelley address build \
          --testnet-magic ${MAGIC} \
          --payment-verification-key-file ${WORKDIR}/Test_${ID}/payer${ID}.vkey > ${WORKDIR}/Test_${ID}/payer${ID}.addr

echo "Funding Payer ${ID}s account..."

### Build, Sign, Submit a Genesis UTxO to the Payer
${CLICMD} shelley transaction build-raw \
    --tx-in  `cat ${WORKDIR}/Test_${ID}/genesis_utxo${ID}`#0 \
    --tx-out `cat ${WORKDIR}/Test_${ID}/payer${ID}.addr`+$(( ${SUPPLY} / ${NNODES} )) \
    --ttl 10000 \
    --fee 0 \
    --tx-body-file ${WORKDIR}/Test_${ID}/txs/genesis_to_funding${ID}.txbody

${CLICMD} shelley transaction sign \
    --tx-body-file ${WORKDIR}/Test_${ID}/txs/genesis_to_funding${ID}.txbody \
    --signing-key-file configuration/genesis/utxo-keys/utxo${UTXO_ID}.skey \
    --testnet-magic ${MAGIC} \
    --tx-file ${WORKDIR}/Test_${ID}/txs/genesis_to_funding${ID}.tx

${CLICMD} shelley transaction submit \
    --tx-file ${WORKDIR}/Test_${ID}/txs/genesis_to_funding${ID}.tx \
    --testnet-magic ${MAGIC}

echo "Waiting for the UTxO to appear on-chain... (this will take ~15 seconds)"

sleep 15

# Get the initial UTxO
${CLICMD} shelley query utxo \
    --address `cat ${WORKDIR}/Test_${ID}/payer${ID}.addr` \
    --testnet-magic ${MAGIC} | grep 0 | cut -f1 -d ' ' | sed 's/$/#0/g' > ${WORKDIR}/Test_${ID}/payer_utxo_0

echo "Creating ${NUM_OF_ADDRESSES} addresses..."

# Create n target addresses
time ./create-addresses-generic.sh ${ID}
echo ""

# Set-up variables for calculating change
let "payer_ada = ${SUPPLY} / ${utxo_keys}"
STD_TX=${txvalue}
STD_FEE=${txfee}

echo "Submitting ${NUM_OF_ADDRESSES} transactions..."

time for i in $(seq 1 ${NUM_OF_ADDRESSES})
do
    # Calculate change
    let "payer_ada-=${STD_TX}"
    let "payer_ada-=${STD_FEE}"

    # Build n transactions
    ${CLICMD} shelley transaction build-raw \
        --tx-in `cat ${WORKDIR}/Test_${ID}/payer_utxo_$((${i} - 1))` \
        --tx-out `cat ${WORKDIR}/Test_${ID}/addresses/address_${i}`+${STD_TX} \
        --tx-out `cat ${WORKDIR}/Test_${ID}/payer${ID}.addr`+${payer_ada} \
        --ttl 10000000 \
        --fee ${STD_FEE} \
        --out-file ${WORKDIR}/Test_${ID}/txs/tx_${i}.raw

    # Sign n transactions
    ${CLICMD} shelley transaction sign \
        --tx-body-file ${WORKDIR}/Test_${ID}/txs/tx_${i}.raw \
        --signing-key-file ${WORKDIR}/Test_${ID}/payer${ID}.skey \
        --testnet-magic ${MAGIC} \
        --out-file ${WORKDIR}/Test_${ID}/txs/tx_${i}.signed

    # Get the UTxO of the transaction for the input of the subsequent transaction
    ${CLICMD} shelley transaction txid --tx-body-file ${WORKDIR}/Test_${ID}/txs/tx_${i}.raw | sed 's/$/#1/g'> ${WORKDIR}/Test_${ID}/payer_utxo_${i}
    # Submit n transactions
    ${CLICMD} shelley transaction submit \
              --tx-file ${WORKDIR}/Test_${ID}/txs/tx_${i}.signed \
              --testnet-magic ${MAGIC}

    if ! ((${i} % 100)); then
        echo "Generated ${i} transactions."
    fi
done

echo ""
echo "Finished!"
