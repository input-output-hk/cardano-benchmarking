#!/usr/bin/env bash
# shellcheck disable=SC1090

BASEDIR="$(realpath "$(dirname "$0")")"
. "$(realpath "${BASEDIR}"/../../scripts/common.sh)"

### parameters

create_new_genesis=1
clean_explorer_db=0
run_rt_view_service=1
run_cluster_nodes=1
run_second_cluster=1
run_explorer=0
run_tx_generator=1
do_delegate=0

while test $# -ge 1
do case "$1" in
           --no-genesis )        create_new_genesis=0;;
           --explorer )          clean_explorer_db=1; run_explorer=1;;
           --rtview )            run_rt_view_service=1;;
           --no-rtview )         run_rt_view_service=0;;
           --second-cluster )    run_second_cluster=1;;
           --no-second-cluster ) run_second_cluster=0;;
           --no-generator )      run_tx_generator=0;;
           --delegate )          do_delegate=1;;
           --no-delegate )       do_delegate=0;;
           * ) break;; esac; shift; done

case "$era" in
        shelley )
                ## For now, those are not supported.
                run_second_cluster=0;;
esac

### >>>>>> do not change anything below this point

## Oh the absolute, sheer horrors of 'tmux'..
## NOTE:  this inherits the eval caches from 'start.sh'
TMUX_ENV_PASSTHROUGH=(
         "export era=${era}; export DBDIR=${DBDIR}; export SOCKETDIR=${SOCKETDIR};"
         "export SCRIPTS_LIB_SH_MODE=${SCRIPTS_LIB_SH_MODE};"
         "export __COMMON_SRCROOT=${__COMMON_SRCROOT};"
         "export DEFAULT_DEBUG=${DEFAULT_DEBUG};"
         "export DEFAULT_VERBOSE=${DEFAULT_VERBOSE};"
         "export DEFAULT_TRACE=${DEFAULT_TRACE};"
         "export debug=${debug} verbose=${verbose} trace=${trace};"
         "export profile=${profile} xc=${xc};"
         "$(nix_cache_passthrough)"
)
## ^^ Keep in sync with run-3node-cluster.sh

# clean
rm -rf ./db/ ./sockets/ ./logs/
mkdir -p logs sockets logs/sockets

# Generate new genesis
if [ $create_new_genesis -eq 1 ]; then
  if ! ./genesis-$era.sh
  then echo -e "--( FATAL:  genesis generation failed\nPress Enter to exit."
       read
       exit 1; fi; fi

# Prepare SQL database
# (assuming db user has been defined in the database system)

if [ $clean_explorer_db -eq 1 ]; then
  ./clean-explorer-db.sh ${BASEDIR}
fi

node_mode=local

# Run rt-view service. If it enabled, it should be launched BEFORE
# the cluster, to avoid TraceForwarderConnectionError.
if [ $run_rt_view_service -eq 1 ]; then
  node_mode=rt-view
  tmux select-window -t :0
  tmux new-window -n RTView \
               "${TMUX_ENV_PASSTHROUGH[*]} ./run_rt_view_service.sh; $SHELL"
  sleep 2
fi


# Run cluster in tmux
if [ $run_cluster_nodes -eq 1 ]; then
  tmux select-window -t :0
  tmux new-window -n Nodes1 \
               "${TMUX_ENV_PASSTHROUGH[*]} ./run-3nodes.sh ${node_mode}; $SHELL"
fi


# Run cluster in tmux
if [ $run_second_cluster -eq 1 ]; then
  sleep 1
  tmux select-window -t :0
  tmux new-window -n Nodes2 \
               "${TMUX_ENV_PASSTHROUGH[*]} export CLUSTER_INDEX_START=4; ./run-3nodes.sh ${node_mode}; $SHELL"
fi

sleep 1
tmux select-window -t :0
echo -n "Waiting for node socket to appear ($BASEDIR/sockets/1): "
while test ! -e $BASEDIR/sockets/1
do echo -n "."; sleep 1; done; echo

# Delegate
if [ $era = 'shelley' -a $do_delegate -eq 1 ]; then
  tmux select-window -t :0
  tmux new-window -n Delegation \
               "${TMUX_ENV_PASSTHROUGH[*]} ./submit_delegation_tx.sh; $SHELL"
  sleep 5
fi

# Run transaction generator
if [ $run_tx_generator -eq 1 ]; then
  tmux select-window -t :0
  tmux new-window -n TxGen \
               "${TMUX_ENV_PASSTHROUGH[*]} ./run_tx_generator.sh 2>&1 | tee $BASEDIR/logs/generator.stderr; $SHELL"
fi

# Run explorer
if [ $run_explorer -eq 1 ]; then
  tmux select-window -t :0
  tmux new-window -n Explorer \
               "sleep 1; ${TMUX_ENV_PASSTHROUGH[*]} ./run_explorer.sh; $SHELL"
  sleep 2
fi

if [ $run_tx_generator -eq 1 ]; then
  tmux select-window -t TxGen
  sleep 1
fi

$SHELL
