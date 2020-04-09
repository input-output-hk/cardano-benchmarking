#!/bin/sh

### parameters

# process
create_new_genesis=1
clean_explorer_db=1
run_rt_view_service=1
run_cluster_nodes=1
run_explorer=1
run_tx_generator=1


### >>>>>> do not change anything below this point

# check for explorer binary
if [ ! -e ../../bin/cardano-db-sync ]; then
  echo "cardano-db-sync missing. disabling explorer functionality."
  clean_explorer_db=0
  run_explorer=0
fi

BASEDIR=$(realpath $(dirname $0))

# clean
if [ -d db ]; then rm -rf db; fi
if [ -d logs ]; then rm -rf logs; fi
if [ -d socket ]; then rm -rf socket; fi
if [ -d db-0 ]; then rm -rf db-0; fi
if [ -d db-1 ]; then rm -rf db-1; fi
if [ -d db-2 ]; then rm -rf db-2; fi

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


# 2) prepare SQL database
# (assuming db user has been defined in the database system)

if [ $clean_explorer_db -eq 1 ]; then
  { . configuration/psql-settings.sh
    psql -d postgres -c "DROP DATABASE ${PGDATABASE};" || echo "DB missing"
    psql -d postgres -c "CREATE DATABASE ${PGDATABASE} OWNER=${PGUSER};" || echo "ignored"
    read -p "Continue? (y|n)" answ
    case $answ in
      [Nn]) exit 1;;
      * ) echo continuing;;
    esac
  }
fi


# 3) run rt-view service. If it enabled, it should be launched BEFORE
# the cluster, to avoid TraceForwarderConnectionError.
if [ $run_rt_view_service -eq 1 ]; then
  tmux select-window -t :0
  tmux new-window -n RTView "sleep 5; ./run_rt_view_service.sh; $SHELL"
  sleep 1
  tmux set-window-option remain-on-exit on
fi


# 4) run cluster in tmux
if [ $run_cluster_nodes -eq 1 ]; then
  tmux select-window -t :0
  # If rt-view service is enabled, run cluster's nodes
  # using another configuration files to forward metrics.
  if [ $run_rt_view_service -eq 1 ]; then
    tmux new-window -n Nodes "./run-3node-cluster-rt-view.sh; $SHELL"
  else
    tmux new-window -n Nodes "./run-3node-cluster.sh; $SHELL"  
  fi
  sleep 1
  tmux set-window-option remain-on-exit on
fi


# 5) run transaction generator
if [ $run_tx_generator -eq 1 ]; then
  tmux select-window -t :0
  tmux new-window -n TxGen "sleep 10; ./run_tx_generator.sh; $SHELL" 
  sleep 1
  tmux set-window-option remain-on-exit on
fi


# 6) run explorer
if [ $run_explorer -eq 1 ]; then
  tmux select-window -t :0
  tmux new-window -n Explorer "sleep 5; ./run_explorer.sh; $SHELL"
  sleep 1
  tmux set-window-option remain-on-exit on
fi


tmux select-window -n Nodes
sleep 1
tmux set-window-option remain-on-exit on
$SHELL
