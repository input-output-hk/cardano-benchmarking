#!/usr/bin/env bash

if [ $# -lt 1 ]; then
  echo "call: $0 <rt-view|local>"
  exit 1
fi

SUBCONFIG=
if [ $1 == "rt-view" ]; then
  SUBCONFIG="-rt-view"
fi

# add to your ~/.tmux.conf:
# set-window-option -g mouse on
# set -g default-terminal "tmux-256color"

# start a tmux session:
# tmux new-session -s 'Demo' -t demo
if [ -z "${TMUX}" ]; then
  echo "can only be run under 'tmux' control."
  exit 1
fi

BASEPATH=$(realpath $(dirname $0))

#RUNNER=${RUNNER:-cabal v2-exec -v0}
#CMD="${RUNNER} cardano-node --"
CMD="${BASEPATH}/../../bin/cardano-node "

# VERBOSITY="--tracing-verbosity-minimal"
# VERBOSITY="--tracing-verbosity-normal"
VERBOSITY="--tracing-verbosity-maximal"

genesis_root=${BASEPATH}/configuration/latest-genesis
genesis_file=${genesis_root}/genesis.json


### prep cli arguments

function nodecfg () {
        sed -i 's|^GenesisFile: .*$|GenesisFile: '${genesis_file}'|' configuration/log-config${SUBCONFIG}-${1}.yaml
        printf -- "--config configuration/log-config${SUBCONFIG}-${1}.yaml "
}
function dlgkey () {
        printf -- "--signing-key ${genesis_root}/delegate-keys.%03d.key " "$1"
}
function dlgcert () {
        printf -- "--delegation-certificate ${genesis_root}/delegation-cert.%03d.json " "$1"
}
function commonargs() {
        printf -- "--topology ${BASEPATH}/configuration/simple-topology-real-pbft-node-$1.json "
        printf -- "--database-path ./db-$1/ "
        printf -- "--socket-path /tmp/cluster3nodes-socket/$1 "
}

function nodeargs () {
        commonargs $1
        dlgkey $1
        dlgcert $1
        printf -- "--port $((3000 + $1)) "
        nodecfg $1
}

# create tmux panes
tmux split-window -v
tmux split-window -h
tmux select-pane -t 0

# start nodes
tmux select-pane -t 0
tmux send-keys "cd '${BASEPATH}'; ${CMD} run $(nodeargs 0) " C-m
tmux select-pane -t 1
tmux send-keys "cd '${BASEPATH}'; ${CMD} run $(nodeargs 1) " C-m
tmux select-pane -t 2
tmux send-keys "cd '${BASEPATH}'; ${CMD} run $(nodeargs 2) " C-m

