#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2206,SC2209

set -e

unset profile xc

BASEDIR=$(realpath $(dirname "$0"))
. "$(realpath "${BASEDIR}"/../../scripts/common.sh)"

CONFIGDIR="${BASEDIR}/configuration-shelley"

. "${CONFIGDIR}"/parameters

cd ${BASEDIR}

tmp="`mktemp`.d"
args=(--genesis-dir     "${tmp}"
      --testnet-magic    ${MAGIC}
      --supply           ${SUPPLY}
      --gen-genesis-keys $NNODES
      --gen-utxo-keys    $NNODES
)

run 'cardano-cli' shelley genesis create "${args[@]}"


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

## Genesis spec tweaks
GENESIS_SPEC_UPDATES=(
    -e 's/"activeSlotsCoeff": .*,/"activeSlotsCoeff": 0.1,/'
    -e 's/"decentralisationParam": .*,/"decentralisationParam": 0.7,/'
    -e 's/"epochLength": .*,/"epochLength": 60,/'
    # -e 's/"minFeeA": .*,/"minFeeA": 44,/'
    # -e 's/"minFeeB": .*,/"minFeeB": 155000,/'
    -e 's/"nOpt": .*,/"nOpt": 3,/'
    -e 's/"securityParam": .*/"securityParam": 5/'
    -e 's/"slotLength": .*,/"slotLength": 1,/'
)
$SED -i "${tmp}"/genesis.spec.json "${GENESIS_SPEC_UPDATES[@]}"

run='run_noxc'

UPDATE_GENESIS=(
  $run 'cardano-cli' shelley 'genesis' 'create'
    --genesis-dir     "${tmp}"
    --testnet-magic    ${MAGIC}
    --supply           ${SUPPLY}
)
"${UPDATE_GENESIS[@]}"

## create KES, VRF, certs per node
for i in $(seq 1 $NNODES); do
    mkdir -p "${tmp}"/node$i/cold

    KEYGEN_KES=(
    $run 'cardano-cli' shelley node 'key-gen-KES'
      --verification-key-file              "${tmp}"/node$i/kes.vkey
      --signing-key-file                   "${tmp}"/node$i/kes.skey
    )
    "${KEYGEN_KES[@]}"

    KEYGEN_VRF=(
    $run 'cardano-cli' shelley node 'key-gen-VRF'
      --verification-key-file              "${tmp}"/node$i/vrf.vkey
      --signing-key-file                   "${tmp}"/node$i/vrf.skey
    )
    "${KEYGEN_VRF[@]}"

    # cold keys (do not copy to production system)
    # for Release-1.13
    KEYGEN=(
    $run 'cardano-cli' shelley node 'key-gen'
      --cold-verification-key-file         "${tmp}"/node$i/cold/operator.vkey
      --cold-signing-key-file              "${tmp}"/node$i/cold/operator.skey
      --operational-certificate-issue-counter-file
                                           "${tmp}"/node$i/cold/operator.counter
    )
    "${KEYGEN[@]}"

    # certificate (adapt kes-period for later certs)
    ISSUE_OPCERT=(
    $run 'cardano-cli' shelley node 'issue-op-cert'
      --hot-kes-verification-key-file      "${tmp}"/node$i/kes.vkey
      --cold-signing-key-file              "${tmp}"/delegate-keys/delegate$i.skey
      --operational-certificate-issue-counter
                                           "${tmp}"/delegate-keys/delegate$i.counter
      --out-file                           "${tmp}"/node$i/node.cert
      --kes-period                         0
    )
    "${ISSUE_OPCERT[@]}"
done

# === delegation ===

## prepare addresses
mkdir -p ${tmp}/addresses

USER_ADDRS=$(for N in $STAKEPOOLS; do echo -n "user${N} "; done)
POOL_ADDRS=$(for N in $STAKEPOOLS; do echo -n "pool-owner${N} "; done)

ADDRS="${USER_ADDRS} ${POOL_ADDRS}"
for ADDR in ${ADDRS}; do

  echo -n "$ADDR "
  ### Payment address keys
  $run 'cardano-cli' shelley address key-gen \
      --verification-key-file ${tmp}/addresses/${ADDR}.vkey \
      --signing-key-file      ${tmp}/addresses/${ADDR}.skey

  ### Stake address keys
  $run 'cardano-cli' shelley stake-address key-gen \
      --verification-key-file ${tmp}/addresses/${ADDR}-stake.vkey \
      --signing-key-file      ${tmp}/addresses/${ADDR}-stake.skey

  ### Payment addresses
  $run 'cardano-cli' shelley address build \
      --payment-verification-key-file ${tmp}/addresses/${ADDR}.vkey \
      --stake-verification-key-file ${tmp}/addresses/${ADDR}-stake.vkey \
      --testnet-magic ${MAGIC} \
      --out-file ${tmp}/addresses/${ADDR}.addr

  ### Stake addresses
  $run 'cardano-cli' shelley stake-address build \
      --stake-verification-key-file ${tmp}/addresses/${ADDR}-stake.vkey \
      --testnet-magic ${MAGIC} \
      --out-file ${tmp}/addresses/${ADDR}-stake.addr

  ### Stake addresses registration certs
  $run 'cardano-cli' shelley stake-address registration-certificate \
      --stake-verification-key-file ${tmp}/addresses/${ADDR}-stake.vkey \
      --out-file ${tmp}/addresses/${ADDR}-stake.reg.cert

done
echo

## create delegation certs

for N in ${STAKEPOOLS}; do
  echo -n "user ${N} -> pool ${N}  "
  ### Stake address delegation certs
  $run 'cardano-cli' shelley stake-address delegation-certificate \
      --stake-verification-key-file ${tmp}/addresses/user${N}-stake.vkey \
      --cold-verification-key-file  ${tmp}/node${N}/cold/operator.vkey \
      --out-file ${tmp}/addresses/user${N}-stake.deleg.cert

  ln -s ../addresses/pool-owner${N}-stake.vkey ${tmp}/node${N}/owner.vkey
  ln -s ../addresses/pool-owner${N}-stake.skey ${tmp}/node${N}/owner.skey

done
echo

## make stake pool registration cert

for NODE in ${STAKEPOOLS}; do
  echo -n "pool ${NODE}  "
  $run 'cardano-cli' shelley stake-pool registration-certificate \
    --testnet-magic ${MAGIC} \
    --pool-pledge 0 --pool-cost 0 --pool-margin 0 \
    --cold-verification-key-file             ${tmp}/node${NODE}/cold/operator.vkey \
    --vrf-verification-key-file              ${tmp}/node${NODE}/vrf.vkey \
    --reward-account-verification-key-file   ${tmp}/node${NODE}/owner.vkey \
    --pool-owner-stake-verification-key-file ${tmp}/node${NODE}/owner.vkey \
    --out-file                               ${tmp}/node${NODE}/registration.cert
done
echo

## prepare delegation transaction

STAKE=$((SUPPLY / NNODES))
for N in ${STAKEPOOLS}; do

    echo "move funds to user ${N}, delegate to pool ${N}"
    ### build tx
    $run 'cardano-cli' shelley transaction build-raw \
        --ttl 1000 \
        --fee 0 \
        --tx-in $(run 'cardano-cli' shelley genesis initial-txin \
                    --testnet-magic ${MAGIC} \
                    --verification-key-file ${tmp}/utxo-keys/utxo${N}.vkey) \
        --tx-out $(cat ${tmp}/addresses/user${N}.addr)+${STAKE} \
        --certificate-file ${tmp}/addresses/pool-owner${N}-stake.reg.cert \
        --certificate-file ${tmp}/node${N}/registration.cert \
        --certificate-file ${tmp}/addresses/user${N}-stake.reg.cert \
        --certificate-file ${tmp}/addresses/user${N}-stake.deleg.cert \
        --out-file ${tmp}/node${N}/tx-delegate${N}.txbody

    ### sign tx
    $run 'cardano-cli' shelley transaction sign \
        --signing-key-file ${tmp}/utxo-keys/utxo${N}.skey \
        --signing-key-file ${tmp}/addresses/user${N}-stake.skey \
        --signing-key-file ${tmp}/node${N}/owner.skey \
        --signing-key-file ${tmp}/node${N}/cold/operator.skey \
        --testnet-magic ${MAGIC} \
        --tx-body-file  ${tmp}/node${N}/tx-delegate${N}.txbody \
        --out-file      ${tmp}/node${N}/tx-delegate${N}.tx

done

# GENHASH=$(run 'cardano-cli' \
#               print-genesis-hash \
#               --genesis-json "${tmp}/genesis.json" |
#                   tail -1)
TARGETDIR="${CONFIGDIR}/genesis"

rm -rf "${TARGETDIR}"
mv "${tmp}" "${TARGETDIR}"

echo
