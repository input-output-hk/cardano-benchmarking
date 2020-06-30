#!/usr/bin/env bash

# preparation
BASEDIR=$(realpath $(dirname "$0"))
. "${BASEDIR}"/../../scripts/common.sh

##prebuild 'cardano-tx-generator' || exit 1
#prebuild 'cardano-rt-view' || exit 1
prebuild 'cardano-node' || exit 1
prebuild 'cardano-cli' || exit 1

export CLICMD="run cardano-cli"

TMUX_ENV_PASSTHROUGH=(
         "export era=shelley; export DBDIR=${DBDIR}; export SOCKETDIR=${SOCKETDIR};"
         "export SCRIPTS_LIB_SH_MODE=${SCRIPTS_LIB_SH_MODE};"
         "export __COMMON_SRCROOT=${__COMMON_SRCROOT};"
         "export DEFAULT_DEBUG=${DEFAULT_DEBUG};"
         "export DEFAULT_VERBOSE=${DEFAULT_VERBOSE};"
         "export DEFAULT_TRACE=${DEFAULT_TRACE};"
         "$(nix_cache_passthrough)"
)


# 0 cleanup
rm -rf ./db/* ./logs/*
mkdir -p logs/sockets

# 1 prepare genesis
./prepare_genesis-generic.sh

# 2 run rt-view
tmux select-window -t :0
tmux new-window -n RTview \
             "${TMUX_ENV_PASSTHROUGH[*]} ./run-rt-view.sh; $SHELL"
sleep 1

# 3 run pools
tmux select-window -t :0
tmux new-window -n Nodes \
             "${TMUX_ENV_PASSTHROUGH[*]} ./run-Npools.sh; $SHELL"
sleep 2

# 4 run tx-gen
tmux select-window -t :0
tmux new-window -n TxGen \
             "sleep 7; ${TMUX_ENV_PASSTHROUGH[*]} ./run-tx-generator.sh; $SHELL"
sleep 30

# 5 send delegation transactions
tmux select-window -t :0
tmux new-window -n Delegation \
             "sleep 5; ${TMUX_ENV_PASSTHROUGH[*]} ./submit_delegation_tx.sh; $SHELL"

tmux select-window -t Nodes
sleep 1
$SHELL
