#!/usr/bin/env bash

# preparation
BASEDIR=$(realpath $(dirname "$0"))
. "${BASEDIR}"/../../scripts/common.sh

prebuild 'cardano-tx-generator' || exit 1
prebuild 'cardano-rt-view-service' || exit 1
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

set -e

# 0 cleanup
rm -rf ./db/* ./logs/*
mkdir -p db logs/sockets

# 1 prepare genesis
./prepare_genesis_staked.sh

# 2 run rt-view
tmux select-window -t :0
tmux new-window -n RTview \
             "${TMUX_ENV_PASSTHROUGH[*]} ./run-rt-view.sh; $SHELL"
sleep 1

# 3 run pools
tmux select-window -t :0
tmux new-window -n Nodes \
             "${TMUX_ENV_PASSTHROUGH[*]} ./run-3pools.sh; $SHELL"
sleep 2

tmux select-window -t :0
echo -n "Waiting for node socket to appear ($BASEDIR/logs/sockets/1): "
while test ! -e $BASEDIR/logs/sockets/1
do echo -n "."; sleep 1; done; echo

# 4 run tx-gen
tmux select-window -t :0
tmux new-window -n TxGen \
             "${TMUX_ENV_PASSTHROUGH[*]} ./run-tx-generator.sh; $SHELL"
sleep 1

tmux select-window -t Nodes
sleep 1
$SHELL
