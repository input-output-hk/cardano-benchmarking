#!/usr/bin/env bash
# shellcheck disable=SC1090

set -x

BASEDIR="$(realpath "$(dirname "$0")")"
. "$(realpath "${BASEDIR}"/../../scripts/common.sh)"

CONFIGDIR=${BASEDIR}/configuration-$era
CONFIGFILE=${CONFIGDIR}/configuration-explorer.yaml

. ${CONFIGDIR}/psql-settings.sh

GENESISJSON=${CONFIGDIR}/genesis/genesis.json
GENESISHASH=`cat ${CONFIGDIR}/genesis/GENHASH`
sed -i 's/^GenesisHash: .*$/GenesisHash: '${GENESISHASH}'/' ${CONFIGFILE}

run 'cardano-db-sync' \
  --config ${CONFIGFILE} \
  --genesis-file ${GENESISJSON} \
  --socket-path ${BASEDIR}/sockets/0 \
  --schema-dir ${BASEDIR}/../../ext/cardano-db-sync.git/schema \
