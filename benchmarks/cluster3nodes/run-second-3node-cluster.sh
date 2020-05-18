#!/usr/bin/env bash
# shellcheck disable=SC1090

set -x
BASEDIR="$(realpath "$(dirname "$0")")"
. "$(realpath "${BASEDIR}"/../../scripts/common.sh)"

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

# start a tmux session:
# tmux new-session -s 'Demo' -t demo
test -n "${TMUX}" || fail "can only be run under 'tmux' control."

genesis_root=${BASEDIR}/configuration/genesis

SOCKETDIR="/tmp/cluster3nodes-socket/"
if [ ! -d $SOCKETDIR ]; then
  mkdir $SOCKETDIR
fi

### prep cli arguments

function nodecfg () {
        printf -- "--config configuration/log-config${SUBCONFIG}-${1}.yaml "
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

## Keep in sync with benchmark.sh
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

for i in 3 4 5
do tmux select-pane -t $((i-3))
   tmux send-keys \
     "${TMUX_ENV_PASSTHROUGH[*]}

      cd '${BASEDIR}';
      . ${__COMMON_SRCROOT}/scripts/lib.sh;
      . ${__COMMON_SRCROOT}/scripts/lib-cli.sh;
      . ${__COMMON_SRCROOT}/scripts/lib-node.sh;

      run cardano-node run $(nodeargs $i)" \
     C-m
done
tmux select-pane -t 0
