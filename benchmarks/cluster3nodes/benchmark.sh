#!/usr/bin/env bash
# shellcheck disable=SC1090

BASEDIR="$(realpath "$(dirname "$0")")"
. "$(realpath "${BASEDIR}"/../../scripts/common.sh)"

export IGNOREEOF=2

### parameters

# process
create_new_genesis=1
clean_explorer_db=0
run_rt_view_service=1
run_cluster_nodes=1
run_explorer=0
run_tx_generator=1


### >>>>>> do not change anything below this point

## Oh the absolute, sheer horrors of 'tmux'..
prebuild 'cardano-tx-generator'
prebuild 'cardano-rt-view-service'
prebuild 'cardano-node'
prebuild 'cardano-db-sync'
prebuild 'cardano-cli'
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
for x in db db-* logs socket
do test -d ./"$x" && rm -rf ./"$x"
done


# mk dirs
mkdir -p db logs


# 1) generate new genesis
if [ $create_new_genesis -eq 1 ]; then
  ./genesis.sh
  read -p "Continue? (y|n)" answ
  case $answ in
    [Nn]) exit 1;;
    * ) echo continuing;;
  esac
fi
set -x

# 2) prepare SQL database
# (assuming db user has been defined in the database system)

if [ $clean_explorer_db -eq 1 ]; then
  ./clean-explorer-db.sh ${BASEDIR}
fi


# 3) run rt-view service. If it enabled, it should be launched BEFORE
# the cluster, to avoid TraceForwarderConnectionError.
if [ $run_rt_view_service -eq 1 ]; then
  tmux select-window -t :0
  tmux new-window -n RTView \
               "${TMUX_ENV_PASSTHROUGH[*]} ./run_rt_view_service.sh; $SHELL"
  sleep 3
fi


# 4) run cluster in tmux
if [ $run_cluster_nodes -eq 1 ]; then
  tmux select-window -t :0
  # If rt-view service is enabled, run cluster's nodes
  # using another configuration files to forward metrics.
  if [ $run_rt_view_service -eq 1 ]; then
    tmux new-window -n Nodes \
               "${TMUX_ENV_PASSTHROUGH[*]} sleep 3; ./run-3node-cluster.sh rt-view; $SHELL"
  else
    tmux new-window -n Nodes \
               "${TMUX_ENV_PASSTHROUGH[*]} ./run-3node-cluster.sh local; $SHELL"
  fi
  sleep 1
fi


# 5) run transaction generator
if [ $run_tx_generator -eq 1 ]; then
  tmux select-window -t :0
  tmux new-window -n TxGen \
               "${TMUX_ENV_PASSTHROUGH[*]} sleep 5; ./run_tx_generator.sh; $SHELL"
  sleep 1
fi


# 6) run explorer
if [ $run_explorer -eq 1 ]; then
  tmux select-window -t :0
  tmux new-window -n Explorer \
               "${TMUX_ENV_PASSTHROUGH[*]} sleep 5; ./run_explorer.sh; $SHELL"
  sleep 1
fi


tmux select-window -t Nodes
sleep 1
$SHELL
