#!/usr/bin/env bash
# shellcheck disable=SC1090

BASEDIR="$(realpath "$(dirname "$0")")"
. "$(realpath "${BASEDIR}"/../../scripts/common.sh)"

. "${BASEDIR}"/configuration/parameters

CONFIGDIR=${BASEDIR}/configuration
CONFIGFILE=${CONFIGDIR}/log-config-generator.yaml

GENESISJSON="${CONFIGDIR}/genesis/genesis.json"

# arguments
TARGETNODES=`for N in $targetnodes; do echo -n "--target-node (\"127.0.0.1\",$((3000+$N))) "; done`

echo "$TARGETNODES"

run 'cardano-tx-generator' \
  --config ${CONFIGFILE} \
  --signing-key ${CONFIGDIR}/genesis/delegate-keys.000.key \
  --delegation-certificate ${CONFIGDIR}/genesis/delegation-cert.000.json \
  --genesis-file ${GENESISJSON} \
  --socket-path /tmp/cluster3nodes-socket/0 \
  --num-of-txs $numtx \
  --add-tx-size $addsizetx \
  --inputs-per-tx $inputstx \
  --outputs-per-tx $outputstx \
  --tx-fee $txfee \
  --tps $tps \
  --sig-key ${CONFIGDIR}/genesis/delegate-keys.000.key \
  --sig-key ${CONFIGDIR}/genesis/delegate-keys.001.key \
  --sig-key ${CONFIGDIR}/genesis/delegate-keys.002.key \
  --sig-key ${CONFIGDIR}/genesis/delegate-keys.003.key \
  --sig-key ${CONFIGDIR}/genesis/delegate-keys.004.key \
  --sig-key ${CONFIGDIR}/genesis/delegate-keys.005.key \
  --init-cooldown 20 \
  ${TARGETNODES}

../../scripts/analyse.sh
