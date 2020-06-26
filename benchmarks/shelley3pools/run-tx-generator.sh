#!/usr/bin/env bash

BASEDIR=$(realpath $(dirname "$0"))
. ${BASEDIR}/../../scripts/common.sh

CONFIGDIR=${BASEDIR}/configuration

. "${CONFIGDIR}"/parameters

# arguments
TARGETNODES=`for N in $targetnodes; do echo -n "--target-node (\"127.0.0.1\",$((3000 + $N))) "; done`

localsock=$BASEDIR/logs/sockets/1

args_common=(
  --config                 ${CONFIGDIR}/configuration-generator.yaml
  --delegation-certificate ${CONFIGDIR}/genesis/delegation-cert.000.json
  --genesis-file           ${CONFIGDIR}/genesis/genesis.json
  --socket-path            $localsock
  --num-of-txs             $numtx
  --add-tx-size            $addsizetx
  --inputs-per-tx          $inputstx
  --outputs-per-tx         $outputstx
  --tx-fee                 $txfee
  --tps                    $tps
  --init-cooldown          $init_cooldown
  ${TARGETNODES}
  )
args_byron=(
  --signing-key            ${CONFIGDIR}/genesis/delegate-keys.000.key
  --sig-key                ${CONFIGDIR}/genesis/delegate-keys.000.key
  --sig-key                ${CONFIGDIR}/genesis/delegate-keys.001.key
  --sig-key                ${CONFIGDIR}/genesis/delegate-keys.002.key
  --sig-key                ${CONFIGDIR}/genesis/delegate-keys.003.key
  --sig-key                ${CONFIGDIR}/genesis/delegate-keys.004.key
  --sig-key                ${CONFIGDIR}/genesis/delegate-keys.005.key
)
args_shelley=(
  --signing-key            ${CONFIGDIR}/genesis/utxo-keys/utxo3.skey
  --sig-key                ${CONFIGDIR}/genesis/utxo-keys/utxo3.skey
  --sig-key                ${CONFIGDIR}/genesis/utxo-keys/utxo1.skey
  --sig-key                ${CONFIGDIR}/genesis/utxo-keys/utxo2.skey
)

echo "starting submission to:  $TARGETNODES"

case $era in
byron )
        run 'cardano-tx-generator' "${args_common[@]}" "${args_byron[@]}";;
shelley )
        set +e
        run 'cardano-tx-generator' "${args_common[@]}" "${args_shelley[@]}"
        echo run 'cardano-tx-generator' "${args_common[@]}" "${args_shelley[@]}";;
*) echo "ERROR:  unknown era '$era'" >&2;;
esac

../../scripts/analyse.sh
