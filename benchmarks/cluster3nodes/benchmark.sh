#!/usr/bin/env bash
# shellcheck disable=SC1090

BASEDIR="$(realpath "$(dirname "$0")")"
. "$(realpath "${BASEDIR}"/../../scripts/common.sh)"

set -e

export IGNOREEOF=2

### parameters

# process
create_new_genesis=1
clean_explorer_db=0
run_rt_view_service=1
run_cluster_nodes=1
run_second_cluster=1
run_explorer=0
run_tx_generator=1


### >>>>>> do not change anything below this point

## Oh the absolute, sheer horrors of 'tmux'..
## NOTE:  this inherits the eval caches from 'start.sh'
TMUX_ENV_PASSTHROUGH=(
         "export SCRIPTS_LIB_SH_MODE=${SCRIPTS_LIB_SH_MODE};"
         "export __COMMON_SRCROOT=${__COMMON_SRCROOT};"
         "export DEFAULT_DEBUG=${DEFAULT_DEBUG};"
         "export DEFAULT_VERBOSE=${DEFAULT_VERBOSE};"
         "export DEFAULT_TRACE=${DEFAULT_TRACE};"
         "$(nix_cache_passthrough)"
)
## ^^ Keep in sync with run-3node-cluster.sh

# clean
rm -rf ./db/ ./db-*/ ./logs/ ./socket/
mkdir -p logs socket

# 1) generate new genesis
if [ $create_new_genesis -eq 1 ]; then
  ./genesis.sh
fi
set -x

# 2) prepare SQL database
# (assuming db user has been defined in the database system)

if [ $clean_explorer_db -eq 1 ]; then
  ./clean-explorer-db.sh ${BASEDIR}
fi

node_mode=local

# 3) run rt-view service. If it enabled, it should be launched BEFORE
# the cluster, to avoid TraceForwarderConnectionError.
if [ $run_rt_view_service -eq 1 ]; then
  node_mode=rt-view
  tmux select-window -t :0
  tmux new-window -n RTView \
               "${TMUX_ENV_PASSTHROUGH[*]} ./run_rt_view_service.sh; $SHELL"
  sleep 2
fi


# 4a) run cluster in tmux
if [ $run_cluster_nodes -eq 1 ]; then
  tmux select-window -t :0
  tmux new-window -n Nodes1 \
               "${TMUX_ENV_PASSTHROUGH[*]} ./run-3node-cluster.sh ${node_mode}; $SHELL"
  sleep 2
fi


# 4b) run cluster in tmux
if [ $run_second_cluster -eq 1 ]; then
  tmux select-window -t :0
  tmux new-window -n Nodes2 \
               "${TMUX_ENV_PASSTHROUGH[*]} ./run-second-3node-cluster.sh ${node_mode}; $SHELL"
  sleep 2
fi


# 5) run transaction generator
if [ $run_tx_generator -eq 1 ]; then
  tmux select-window -t :0
  tmux new-window -n TxGen \
               "${TMUX_ENV_PASSTHROUGH[*]} sleep 5; ./run_tx_generator.sh; $SHELL"
  sleep 6
fi


# 6) run explorer
if [ $run_explorer -eq 1 ]; then
  tmux select-window -t :0
  tmux new-window -n Explorer \
               "${TMUX_ENV_PASSTHROUGH[*]} sleep 1; ./run_explorer.sh; $SHELL"
  sleep 2
fi


tmux select-window -t Nodes
sleep 1
$SHELL
