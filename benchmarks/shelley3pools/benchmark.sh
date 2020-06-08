#!/usr/bin/env bash

# preparation
BASEDIR=$(realpath $(dirname "$0"))
. "${BASEDIR}"/../../scripts/common.sh

##prebuild 'cardano-tx-generator' || exit 1
prebuild 'cardano-rt-view' || exit 1
prebuild 'cardano-node' || exit 1
prebuild 'cardano-cli' || exit 1

# 0 cleanup
rm -rf ./db/* ./logs/*
mkdir -p logs/sockets

# 1 prepare genesis
./prepare_genesis.sh

# 2 run rt-view
tmux select-window -t :0
tmux new-window -n RTview \
             "./run-rt-view.sh; $SHELL"
sleep 1

# 3 run pools
tmux select-window -t :0
tmux new-window -n Nodes \
             "./run-3pools.sh; $SHELL"
sleep 2

# 4 run tx-gen
tmux select-window -t :0
tmux new-window -n TxGen \
             "sleep 5; ./run-tx-generator.sh; $SHELL"
sleep 1

tmux select-window -t Nodes
sleep 1
$SHELL
