#!/usr/bin/env bash
# shellcheck disable=SC1090

set -x

BASEDIR="$(realpath "$(dirname "$0")")"
. "$(realpath "${BASEDIR}"/../../scripts/common.sh)"

CONFIGFILE=${BASEDIR}/configuration/log-config-explorer.yaml

. ${BASEDIR}/configuration/psql-settings.sh

GENESISJSON=${BASEDIR}/configuration/genesis/genesis.json
GENESISHASH=`cat ${BASEDIR}/configuration/genesis/GENHASH`
sed -i 's/^GenesisHash: .*$/GenesisHash: '${GENESISHASH}'/' ${CONFIGFILE}

run 'cardano-db-sync' \
  --config ${CONFIGFILE} \
  --genesis-file ${GENESISJSON} \
  --socket-path /tmp/cluster3nodes-socket/0 \
  --schema-dir ${BASEDIR}/../../ext/cardano-db-sync.git/schema \
