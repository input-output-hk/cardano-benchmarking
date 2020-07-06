#!/usr/bin/env bash

# for debugging
# set -x

set -e

BASEDIR=$(realpath $(dirname "$0"))
. ${BASEDIR}/../../scripts/common.sh
. ${BASEDIR}/configuration/parameters

# check parameter availability
if [ -z "${GEN_SLOTLENGTH}" ]; then
  echo "missing \$GEN_SLOTLENGTH"; exit 1
fi
if [ -z "${GEN_ACTIVESLOTSCOEFF}" ]; then
  echo "missing \$GEN_ACTIVESLOTSCOEFF"; exit 1
fi
if [ -z "${GEN_SECURITYPARAM}" ]; then
  echo "missing \$GEN_SECURITYPARAM"; exit 1
fi
if [ -z "${GEN_EPOCHLENGTH}" ]; then
  echo "missing \$GEN_EPOCHLENGTH"; exit 1
fi
if [ -z "${GEN_DECENTRALISATIONPARAM}" ]; then
  echo "missing \$GEN_DECENTRALISATIONPARAM"; exit 1
fi

cd ${BASEDIR}

if [ -n "${GENESISDIR}" -a -d ${GENESISDIR} ]; then
    rm -rf ${GENESISDIR}
fi
mkdir -p ${GENESISDIR}

CLICMD=${CLICMD:-'run cardano-cli'}

# === genesis ===
${CLICMD} shelley genesis create \
     --genesis-dir ${GENESISDIR} \
     --gen-genesis-keys 1 \
     --gen-utxo-keys 3 \
     --testnet-magic ${MAGIC} \
     --supply ${SUPPLY}

## set parameters in template
sed -i ${GENESISDIR}/genesis.spec.json \
    -e 's/"slotLength": .*,/"slotLength": '${GEN_SLOTLENGTH}',/' \
    -e 's/"activeSlotsCoeff": .*,/"activeSlotsCoeff": '${GEN_ACTIVESLOTSCOEFF}',/' \
    -e 's/"securityParam": .*,/"securityParam": '${GEN_SECURITYPARAM}',/' \
    -e 's/"securityParam": [0-9]\+/"securityParam": '${GEN_SECURITYPARAM}'/' \
    -e 's/"epochLength": .*,/"epochLength": '${GEN_EPOCHLENGTH}',/' \
    -e 's/"decentralisationParam": .*,/"decentralisationParam": '${GEN_DECENTRALISATIONPARAM}',/'

## update genesis from template
${CLICMD} shelley genesis create --genesis-dir ${GENESISDIR} --testnet-magic ${MAGIC} --supply ${SUPPLY}

## set variables
for N in $(seq -s ' ' 1 $NNODES); do Pools[$N]=1; done
for N in ${STAKEPOOLS}; do Pools[$N]=2; done

## create KES, VRF, certs per node
for N in $(seq 1 $NNODES); do
    mkdir -p ${GENESISDIR}/node${N}/cold

    ${CLICMD} shelley node key-gen-KES \
      --verification-key-file ${GENESISDIR}/node${N}/kes.vkey \
      --signing-key-file ${GENESISDIR}/node${N}/kes.skey

    #### cold keys (do not copy to production system)
    if [ ${Pools[$N]} -eq 2 ]; then   ## Stakepool node
        ${CLICMD} shelley node key-gen-VRF \
        --verification-key-file ${GENESISDIR}/node${N}/vrf.vkey \
        --signing-key-file ${GENESISDIR}/node${N}/vrf.skey
        ${CLICMD} shelley node key-gen \
        --cold-verification-key-file ${GENESISDIR}/node${N}/cold/operator.vkey \
        --cold-signing-key-file ${GENESISDIR}/node${N}/cold/operator.skey \
        --operational-certificate-issue-counter-file ${GENESISDIR}/node${N}/cold/operator.counter
    else ## BFT node
        SRCN=1
        ln -s ../../delegate-keys/delegate${SRCN}.skey    ${GENESISDIR}/node${N}/cold/operator.skey
        ln -s ../../delegate-keys/delegate${SRCN}.vkey    ${GENESISDIR}/node${N}/cold/operator.vkey
        ln -s ../../delegate-keys/delegate${SRCN}.counter ${GENESISDIR}/node${N}/cold/operator.counter
        ln -s ../delegate-keys/delegate${SRCN}.vrf.skey   ${GENESISDIR}/node${N}/vrf.skey
        ln -s ../delegate-keys/delegate${SRCN}.vrf.vkey   ${GENESISDIR}/node${N}/vrf.vkey
        cat <<EOF                                         >${GENESISDIR}/node${N}/cold/operator.counter
type: Node operational certificate issue counter
title: Next certificate issue number: 0
cbor-hex:
 00
EOF
    fi

    # certificate (adapt kes-period for later certs)
    ${CLICMD} shelley node issue-op-cert \
      --hot-kes-verification-key-file         ${GENESISDIR}/node${N}/kes.vkey \
      --cold-signing-key-file                 ${GENESISDIR}/node${N}/cold/operator.skey \
      --operational-certificate-issue-counter ${GENESISDIR}/node${N}/cold/operator.counter \
      --kes-period 0 \
      --out-file ${GENESISDIR}/node${N}/node.cert
done

# === delegation ===

## prepare addresses
mkdir -p ${GENESISDIR}/addresses

USER_ADDRS=$(for N in $STAKEPOOLS; do echo -n "user${N} "; done)
POOL_ADDRS=$(for N in $STAKEPOOLS; do echo -n "pool-owner${N} "; done)

ADDRS="${USER_ADDRS} ${POOL_ADDRS}"
for ADDR in ${ADDRS}; do

  echo -n "$ADDR "
  ### Payment address keys
  ${CLICMD} shelley address key-gen \
      --verification-key-file ${GENESISDIR}/addresses/${ADDR}.vkey \
      --signing-key-file      ${GENESISDIR}/addresses/${ADDR}.skey

  ### Stake address keys
  ${CLICMD} shelley stake-address key-gen \
      --verification-key-file ${GENESISDIR}/addresses/${ADDR}-stake.vkey \
      --signing-key-file      ${GENESISDIR}/addresses/${ADDR}-stake.skey

  ### Payment addresses
  ${CLICMD} shelley address build \
      --payment-verification-key-file ${GENESISDIR}/addresses/${ADDR}.vkey \
      --stake-verification-key-file   ${GENESISDIR}/addresses/${ADDR}-stake.vkey \
      --testnet-magic ${MAGIC} \
      --out-file ${GENESISDIR}/addresses/${ADDR}.addr

  ### Stake addresses
  ${CLICMD} shelley stake-address build \
      --stake-verification-key-file ${GENESISDIR}/addresses/${ADDR}-stake.vkey \
      --testnet-magic ${MAGIC} \
      --out-file ${GENESISDIR}/addresses/${ADDR}-stake.addr

  ### Stake addresses registration certs
  ${CLICMD} shelley stake-address registration-certificate \
      --stake-verification-key-file ${GENESISDIR}/addresses/${ADDR}-stake.vkey \
      --out-file ${GENESISDIR}/addresses/${ADDR}-stake.reg.cert

done
echo

## create delegation certs

for N in ${STAKEPOOLS}; do
  echo -n "user ${N} -> pool ${N}  "
  ### Stake address delegation certs
  ${CLICMD} shelley stake-address delegation-certificate \
      --stake-verification-key-file ${GENESISDIR}/addresses/user${N}-stake.vkey \
      --cold-verification-key-file  ${GENESISDIR}/node${N}/cold/operator.vkey \
      --out-file ${GENESISDIR}/addresses/user${N}-stake.deleg.cert

  ln -s ../addresses/pool-owner${N}-stake.vkey ${GENESISDIR}/node${N}/owner.vkey
  ln -s ../addresses/pool-owner${N}-stake.skey ${GENESISDIR}/node${N}/owner.skey

done
echo

## make stake pool registration cert

for NODE in ${STAKEPOOLS}; do
  echo -n "pool ${NODE}  "
  ${CLICMD} shelley stake-pool registration-certificate \
    --testnet-magic ${MAGIC} \
    --pool-pledge 0 --pool-cost 0 --pool-margin 0 \
    --cold-verification-key-file             ${GENESISDIR}/node${NODE}/cold/operator.vkey \
    --vrf-verification-key-file              ${GENESISDIR}/node${NODE}/vrf.vkey \
    --reward-account-verification-key-file   ${GENESISDIR}/node${NODE}/owner.vkey \
    --pool-owner-stake-verification-key-file ${GENESISDIR}/node${NODE}/owner.vkey \
    --out-file                               ${GENESISDIR}/node${NODE}/registration.cert
done
echo

## prepare delegation transaction

STAKE=$((SUPPLY / NNODES))
for N in ${STAKEPOOLS}; do

    echo "move funds to user ${N}, delegate to pool ${N}"
    ### build tx
    ${CLICMD} shelley transaction build-raw \
        --ttl 1000 \
        --fee 0 \
        --tx-in $(${CLICMD} shelley genesis initial-txin \
                    --testnet-magic ${MAGIC} \
                    --verification-key-file ${GENESISDIR}/utxo-keys/utxo${N}.vkey) \
        --tx-out $(cat ${GENESISDIR}/addresses/user${N}.addr)+${STAKE} \
        --certificate-file ${GENESISDIR}/addresses/pool-owner${N}-stake.reg.cert \
        --certificate-file ${GENESISDIR}/node${N}/registration.cert \
        --certificate-file ${GENESISDIR}/addresses/user${N}-stake.reg.cert \
        --certificate-file ${GENESISDIR}/addresses/user${N}-stake.deleg.cert \
        --out-file         ${GENESISDIR}/node${N}/tx-delegate${N}.txbody

    ### sign tx
    ${CLICMD} shelley transaction sign \
        --signing-key-file ${GENESISDIR}/utxo-keys/utxo${N}.skey \
        --signing-key-file ${GENESISDIR}/addresses/user${N}-stake.skey \
        --signing-key-file ${GENESISDIR}/node${N}/owner.skey \
        --signing-key-file ${GENESISDIR}/node${N}/cold/operator.skey \
        --testnet-magic    ${MAGIC} \
        --tx-body-file     ${GENESISDIR}/node${N}/tx-delegate${N}.txbody \
        --out-file         ${GENESISDIR}/node${N}/tx-delegate${N}.tx

done
echo
