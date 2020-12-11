#!/usr/bin/env bash
# shellcheck disable=SC1090

BASEDIR="$(realpath "$(dirname "$0")")"
. "$(realpath "${BASEDIR}"/../../scripts/common.sh)"

CONFIGDIR=${BASEDIR}/configuration-$era
GENESISDIR=${CONFIGDIR}/genesis

. ${CONFIGDIR}/parameters

# === submit delegation transactions ===

for N in ${STAKEPOOLS}; do
  if [ -e ${GENESISDIR}/node${N}/tx-delegate${N}.tx ]; then
    export CARDANO_NODE_SOCKET_PATH=sockets/${N}
    run 'cardano-cli' transaction submit \
                --tx-file ${GENESISDIR}/node${N}/tx-delegate${N}.tx \
                --testnet-magic ${MAGIC}
  else
    echo "no delegation transaction ${GENESISDIR}/node${N}/tx-delegate${N}.tx found!"
  fi
done
