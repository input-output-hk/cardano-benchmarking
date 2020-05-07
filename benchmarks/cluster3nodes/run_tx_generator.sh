#!/usr/bin/env bash

BASEDIR=$(realpath $(dirname $0))

. ${BASEDIR}/configuration/parameters

CONFIGDIR=${BASEDIR}/configuration
CONFIGFILE=${CONFIGDIR}/log-config-generator.yaml

GENESISHASH=`cat ${CONFIGDIR}/latest-genesis/GENHASH`
GENESISJSON="${CONFIGDIR}/latest-genesis/genesis.json"

sed -i 's/^GenesisHash: .*$/GenesisHash: '${GENESISHASH}'/' ${CONFIGFILE}

# arguments
TARGETNODES=`for N in $targetnodes; do echo -n "--target-node (\"127.0.0.1\",$((3000+$N))) "; done`

echo "$TARGETNODES"

#RUNNER=${RUNNER:-cabal v2-run -v0}
#GENERATOR="${RUNNER} cardano-cli --"
GENERATOR="${BASEDIR}/../../bin/cardano-tx-generator"

${GENERATOR} \
  --config ${CONFIGFILE} \
  --signing-key ${CONFIGDIR}/latest-genesis/delegate-keys.000.key \
  --delegation-certificate ${CONFIGDIR}/latest-genesis/delegation-cert.000.json \
  --genesis-file ${GENESISJSON} \
  --socket-path /tmp/cluster3nodes-socket/0 \
  --num-of-txs $numtx \
  --add-tx-size $addsizetx \
  --inputs-per-tx $inputstx \
  --outputs-per-tx $outputstx \
  --tx-fee $txfee \
  --tps $tps \
  --sig-key ${CONFIGDIR}/latest-genesis/delegate-keys.000.key \
  --sig-key ${CONFIGDIR}/latest-genesis/delegate-keys.001.key \
  --sig-key ${CONFIGDIR}/latest-genesis/delegate-keys.002.key \
  ${TARGETNODES}
