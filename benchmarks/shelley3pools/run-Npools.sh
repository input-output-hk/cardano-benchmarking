#!/usr/bin/env bash

if [ -z "${WORKINGDIRECTORY}" ]; then
  echo "missing \$WORKINGDIRECTORY."
  exit 1
fi

BASEDIR=$(realpath $(dirname "$0"))
. ${BASEDIR}/../../scripts/common.sh
. ${BASEDIR}/configuration/parameters

NODECMD=${NODECMD:-"run cardano-node"}

# the host address and interface the node listens:
HOSTADDR=127.0.0.1

if [ -z "$PORTBASE" ]; then
  echo "missing \$PORTBASE in configuration/parameters"; exit 1
fi

GENESISDIR=configuration/genesis

# redirect stderr if LiveView active
REDIRSTDERR="2>/dev/null"
REDIRSTDERR=

TMUX_ENV_PASSTHROUGH=(
         "export SCRIPTS_LIB_SH_MODE=${SCRIPTS_LIB_SH_MODE};"
         "export __COMMON_SRCROOT=${__COMMON_SRCROOT};"
         "export DEFAULT_DEBUG=${DEFAULT_DEBUG};"
         "export DEFAULT_VERBOSE=${DEFAULT_VERBOSE};"
         "export DEFAULT_TRACE=${DEFAULT_TRACE}"
         "$(nix_cache_passthrough)"
)
tmux split-window -v
tmux split-window -h
tmux select-pane -t 0
tmux split-window -h

for N in $(seq 1 "${NNODES}")
do tmux select-pane -t $((N - 1))
   tmux send-keys \
     "${TMUX_ENV_PASSTHROUGH[*]}

      cd '${BASEDIR}';
      . ${__COMMON_SRCROOT}/scripts/lib.sh;
      . ${__COMMON_SRCROOT}/scripts/lib-cli.sh;
      . ${__COMMON_SRCROOT}/scripts/lib-node.sh;

      grep -q -e '^ViewMode: *LiveView' ${WORKINGDIRECTORY}/configuration-node-${N}.yaml &&
        REDIRSTDERR='2>/dev/null'

      ${NODECMD} run \
            --topology ${WORKINGDIRECTORY}/topology-node-${N}.json \
            --database-path db/${N} \
            --socket-path logs/sockets/${N} \
            --host-addr ${HOSTADDR} --port $((PORTBASE + N - 1)) \
            --config ${WORKINGDIRECTORY}/configuration-node-${N}.yaml \
            --shelley-kes-key ${GENESISDIR}/node${N}/kes.skey \
            --shelley-vrf-key ${GENESISDIR}/node${N}/vrf.skey \
            --shelley-operational-certificate ${GENESISDIR}/node${N}/node.cert \
            " ${REDIRSTDERR} \
     C-m
done
tmux select-pane -t 0
