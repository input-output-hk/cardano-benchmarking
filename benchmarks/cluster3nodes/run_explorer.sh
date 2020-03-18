#!/bin/sh

BASEDIR=$(realpath $(dirname $0))

CONFIGFILE=${BASEDIR}/configuration/log-config-explorer.yaml

. ${BASEDIR}/configuration/psql-settings.sh

GENESISHASH=`cat ${BASEDIR}/configuration/latest-genesis/GENHASH`
GENESISJSON="${BASEDIR}/configuration/latest-genesis/genesis.json"

sed -i 's/^GenesisHash: .*$/GenesisHash: '${GENESISHASH}'/' ${CONFIGFILE}

#RUNNER=${RUNNER:-cabal v2-run -v0}
#EXPLORER="${RUNNER} cardano-explorer-node --"
EXPLORER="${BASEDIR}/../../bin/cardano-db-sync"

exec ${EXPLORER} \
  --config ${CONFIGFILE} \
  --genesis-file ${GENESISJSON} \
  --socket-path /tmp/cluster3nodes-socket/0 \
  --schema-dir ${BASEDIR}/../../ext/cardano-db-sync.git/schema \