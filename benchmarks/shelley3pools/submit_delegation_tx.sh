#!/usr/bin/env bash

BASEDIR=$(realpath $(dirname "$0"))
. ${BASEDIR}/../../scripts/common.sh
. ${BASEDIR}/configuration/parameters

CLICMD=${CLICMD:-"run cardano-cli"}

# === submit delegation transactions ===

for N in ${STAKEPOOLS}; do
  if [ -e ${GENESISDIR_shelley}/node${N}/tx-delegate${N}.tx ]; then
    CARDANO_NODE_SOCKET_PATH=logs/sockets/${N} \
    ${CLICMD} shelley transaction submit \
                --tx-file ${GENESISDIR_shelley}/node${N}/tx-delegate${N}.tx \
                --testnet-magic ${MAGIC}
  else
    echo "no delegation transaction ${GENESISDIR_shelley}/node${N}/tx-delegate${N}.tx found!"
  fi
done
