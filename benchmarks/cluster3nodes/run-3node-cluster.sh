#!/usr/bin/env bash
# shellcheck disable=SC1090

BASEDIR="$(realpath "$(dirname "$0")")"

. "${BASEDIR}"/lib.sh

if [ $# -lt 1 ]; then
  echo "call: $0 <rt-view|local>"
  exit 1
fi

SED=sed
ARCH=$(uname)
if [ $ARCH == "Darwin" ]; then
  # check for GNU sed
  T=$(which gsed && echo 1 || echo 0)
  if [ "${T}" == "0" ]; then
    echo "On Darwin we need GNU's version of sed"
    echo "can be installed via 'brew install gnu-sed'"
    exit 1
  else
    SED=gsed
  fi
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
test -n "${TMUX}" || fail "can only be run under 'tmux' control."

#RUNNER=${RUNNER:-cabal v2-exec -v0}
#CMD="${RUNNER} cardano-node --"
#CMD="$(nix_binary_for 'cardano-node' 'cardano-node' 'cardano-node') "
#CMD="${BASEDIR}/../../bin/cardano-node "
CMD="stack exec cardano-node --"

genesis_root=${BASEDIR}/configuration/latest-genesis
genesis_file=${genesis_root}/genesis.json

SOCKETDIR="/tmp/cluster3nodes-socket/"
if [ ! -d $SOCKETDIR ]; then
  mkdir $SOCKETDIR
fi

### prep cli arguments

function nodecfg () {
        $SED -i 's|^GenesisFile: .*$|GenesisFile: '${genesis_file}'|' ${BASEDIR}/configuration/log-config${SUBCONFIG}-${1}.yaml
        printf -- "--config ${BASEDIR}/configuration/log-config${SUBCONFIG}-${1}.yaml "
}
function dlgkey () {
        printf -- "--signing-key ${genesis_root}/delegate-keys.%03d.key " "$1"
}
function dlgcert () {
        printf -- "--delegation-certificate ${genesis_root}/delegation-cert.%03d.json " "$1"
}
function commonargs() {
        printf -- "--topology ${BASEDIR}/configuration/simple-topology-real-pbft-node-$1.json "
        printf -- "--database-path ./db-$1/ "
        printf -- "--socket-path ${SOCKETDIR}/$1 "
}

function nodeargs () {
        commonargs $1
        dlgkey $1
        dlgcert $1
        printf -- "--port $((3000 + $1)) "
        printf -- "+RTS -T -RTS "
        nodecfg $1
}

# create tmux panes
tmux split-window -v
tmux split-window -h
tmux select-pane -t 0

# start nodes
tmux select-pane -t 0
tmux send-keys "cd '${BASEDIR}'; ${CMD} run $(nodeargs 0) " C-m
tmux select-pane -t 1
tmux send-keys "cd '${BASEDIR}'; ${CMD} run $(nodeargs 1) " C-m
tmux select-pane -t 2
tmux send-keys "cd '${BASEDIR}'; ${CMD} run $(nodeargs 2) " C-m

