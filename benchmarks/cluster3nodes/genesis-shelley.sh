#!/usr/bin/env bash
# shellcheck disable=SC1090

set -e

BASEDIR=$(realpath $(dirname "$0"))
. "$(realpath "${BASEDIR}"/../../scripts/common.sh)"

CONFIGDIR="${BASEDIR}/configuration-shelley"

. "${CONFIGDIR}"/parameters

cd ${BASEDIR}

tmpdir="`mktemp`.d"
args=(--genesis-dir     "${tmpdir}"
      --testnet-magic    ${MAGIC}
      --supply           ${SUPPLY}
      --gen-genesis-keys $NNODES
      --gen-utxo-keys    $NNODES
)

run 'cardano-cli' genesis create "${args[@]}"


SED=sed
ARCH=$(uname)
if [ $ARCH == "Darwin" ]; then
  # check for GNU sed
  T=$(which gsed && echo 1 || echo 0)
  if [ "${T}" == "0" ]; then
    echo "On Darwin we need GNU's version of sed"
    echo "can be installed via 'brew install gnu-sed'"
    exit 1
  else
    SED=gsed
  fi
fi

## set parameters in template
$SED -i ${tmpdir}/genesis.spec.json \
    -e 's/"slotLength": .*,/"slotLength": 0.2,/' \
    -e 's/"activeSlotsCoeff": .*,/"activeSlotsCoeff": 0.1,/' \
    -e 's/"securityParam": .*,/"securityParam": 10,/' \
    -e 's/"epochLength": .*,/"epochLength": 1500,/' \
    -e 's/"decentralisationParam": .*,/"decentralisationParam": 0.7,/'

args=(--genesis-dir     "${tmpdir}"
      --testnet-magic    ${MAGIC}
      --supply           ${SUPPLY}
)
## update genesis from template
run 'cardano-cli' genesis create "${args[@]}"

## create KES, VRF, certs per node
for N in $(seq 0 $((NNODES - 1))); do
    mkdir -p ${tmpdir}/node${N}/cold

    run 'cardano-cli' node key-gen-KES \
      --verification-key-file ${tmpdir}/node${N}/kes.vkey \
      --signing-key-file ${tmpdir}/node${N}/kes.skey
    run 'cardano-cli' node key-gen-VRF \
      --verification-key-file ${tmpdir}/node${N}/vrf.vkey \
      --signing-key-file ${tmpdir}/node${N}/vrf.skey

    # cold keys (do not copy to production system)
    # for Release-1.13
    run 'cardano-cli' node key-gen \
     --cold-verification-key-file ${tmpdir}/node${N}/cold/operator.vkey \
     --cold-signing-key-file ${tmpdir}/node${N}/cold/operator.skey \
     --operational-certificate-issue-counter-file ${tmpdir}/node${N}/cold/operator.counter
    # for Release-1.12
    # ln -s ../../delegate-keys/delegate${N}.skey ${tmpdir}/node${N}/cold/operator.skey
    # ln -s ../../delegate-keys/delegate${N}.vkey ${tmpdir}/node${N}/cold/operator.vkey
    # ln -s ../../delegate-keys/delegate-opcert${N}.counter ${tmpdir}/node${N}/cold/operator.counter

    # certificate (adapt kes-period for later certs)
    run 'cardano-cli' node issue-op-cert \
      --hot-kes-verification-key-file         ${tmpdir}/node${N}/kes.vkey \
      --cold-signing-key-file                 ${tmpdir}/node${N}/cold/operator.skey \
      --operational-certificate-issue-counter ${tmpdir}/node${N}/cold/operator.counter \
      --out-file                              ${tmpdir}/node${N}/node.cert \
      --kes-period                            0
done

# === delegation ===

## prepare addresses
mkdir -p ${tmpdir}/addresses

USER_ADDRS=$(for N in $STAKEPOOLS; do echo -n "user${N} "; done)
POOL_ADDRS=$(for N in $STAKEPOOLS; do echo -n "pool-owner${N} "; done)

ADDRS="${USER_ADDRS} ${POOL_ADDRS}"
for ADDR in ${ADDRS}; do

  echo -n "$ADDR "
  ### Payment address keys
  run 'cardano-cli' address key-gen \
      --verification-key-file ${tmpdir}/addresses/${ADDR}.vkey \
      --signing-key-file      ${tmpdir}/addresses/${ADDR}.skey

  ### Stake address keys
  run 'cardano-cli' stake-address key-gen \
      --verification-key-file ${tmpdir}/addresses/${ADDR}-stake.vkey \
      --signing-key-file      ${tmpdir}/addresses/${ADDR}-stake.skey

  ### Payment addresses
  run 'cardano-cli' address build \
      --payment-verification-key-file ${tmpdir}/addresses/${ADDR}.vkey \
      --stake-verification-key-file ${tmpdir}/addresses/${ADDR}-stake.vkey \
      --testnet-magic ${MAGIC} \
      --out-file ${tmpdir}/addresses/${ADDR}.addr

  ### Stake addresses
  run 'cardano-cli' stake-address build \
      --stake-verification-key-file ${tmpdir}/addresses/${ADDR}-stake.vkey \
      --testnet-magic ${MAGIC} \
      --out-file ${tmpdir}/addresses/${ADDR}-stake.addr

  ### Stake addresses registration certs
  run 'cardano-cli' stake-address registration-certificate \
      --stake-verification-key-file ${tmpdir}/addresses/${ADDR}-stake.vkey \
      --out-file ${tmpdir}/addresses/${ADDR}-stake.reg.cert

done
echo

## create delegation certs

for N in ${STAKEPOOLS}; do
  echo -n "user ${N} -> pool ${N}  "
  ### Stake address delegation certs
  run 'cardano-cli' stake-address delegation-certificate \
      --stake-verification-key-file ${tmpdir}/addresses/user${N}-stake.vkey \
      --cold-verification-key-file  ${tmpdir}/node${N}/cold/operator.vkey \
      --out-file ${tmpdir}/addresses/user${N}-stake.deleg.cert

  ln -s ../addresses/pool-owner${N}-stake.vkey ${tmpdir}/node${N}/owner.vkey
  ln -s ../addresses/pool-owner${N}-stake.skey ${tmpdir}/node${N}/owner.skey

done
echo

## make stake pool registration cert

for NODE in ${STAKEPOOLS}; do
  echo -n "pool ${NODE}  "
  run 'cardano-cli' stake-pool registration-certificate \
    --testnet-magic ${MAGIC} \
    --pool-pledge 0 --pool-cost 0 --pool-margin 0 \
    --cold-verification-key-file             ${tmpdir}/node${NODE}/cold/operator.vkey \
    --vrf-verification-key-file              ${tmpdir}/node${NODE}/vrf.vkey \
    --reward-account-verification-key-file   ${tmpdir}/node${NODE}/owner.vkey \
    --pool-owner-stake-verification-key-file ${tmpdir}/node${NODE}/owner.vkey \
    --out-file                               ${tmpdir}/node${NODE}/registration.cert
done
echo

## prepare delegation transaction

STAKE=$((SUPPLY / NNODES))
for N in ${STAKEPOOLS}; do

    echo "move funds to user ${N}, delegate to pool ${N}"
    ### build tx
    run 'cardano-cli' transaction build-raw \
        --ttl 1000 \
        --fee 0 \
        --tx-in $(run 'cardano-cli' genesis initial-txin \
                    --testnet-magic ${MAGIC} \
                    --verification-key-file ${tmpdir}/utxo-keys/utxo${N}.vkey) \
        --tx-out $(cat ${tmpdir}/addresses/user${N}.addr)+${STAKE} \
        --certificate-file ${tmpdir}/addresses/pool-owner${N}-stake.reg.cert \
        --certificate-file ${tmpdir}/node${N}/registration.cert \
        --certificate-file ${tmpdir}/addresses/user${N}-stake.reg.cert \
        --certificate-file ${tmpdir}/addresses/user${N}-stake.deleg.cert \
        --out-file ${tmpdir}/node${N}/tx-delegate${N}.txbody

    ### sign tx
    run 'cardano-cli' transaction sign \
        --signing-key-file ${tmpdir}/utxo-keys/utxo${N}.skey \
        --signing-key-file ${tmpdir}/addresses/user${N}-stake.skey \
        --signing-key-file ${tmpdir}/node${N}/owner.skey \
        --signing-key-file ${tmpdir}/node${N}/cold/operator.skey \
        --testnet-magic ${MAGIC} \
        --tx-body-file  ${tmpdir}/node${N}/tx-delegate${N}.txbody \
        --out-file      ${tmpdir}/node${N}/tx-delegate${N}.tx

done

# GENHASH=$(run 'cardano-cli' \
#               print-genesis-hash \
#               --genesis-json "${tmpdir}/genesis.json" |
#                   tail -1)
TARGETDIR="${CONFIGDIR}/genesis"

rm -rf "${TARGETDIR}"
mv "${tmpdir}" "${TARGETDIR}"

echo
