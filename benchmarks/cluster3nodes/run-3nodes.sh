#!/usr/bin/env bash
# shellcheck disable=SC1090

set -x
BASEDIR="$(realpath "$(dirname "$0")")"

. "$(realpath "${BASEDIR}"/../../scripts/common.sh)"

CONFIGDIR=${BASEDIR}/configuration-${era}

if [ $# -lt 1 ]; then
  echo "call: $0 <rt-view|local>"
  exit 1
fi

SUBCONFIG=
if [ $1 == "rt-view" ]; then
  SUBCONFIG="-rt-view"
  # REDIRSTDERR="2>/dev/null"
fi

# start a tmux session:
# tmux new-session -s 'Demo' -t demo
test -n "${TMUX}" || fail "can only be run under 'tmux' control."

export GENESISDIR=${CONFIGDIR}/genesis
export DBDIR=${BASEDIR}/db
export SOCKETDIR=${BASEDIR}/sockets

# the nodes will listen on ports starting with:
PORTBASE=3000
# the host address and interface the node listens:
HOSTADDR=127.0.0.1

### prep cli arguments

function nodeargs_common () {
        mkdir -p "${DBDIR}/$1" "${SOCKETDIR}"
        printf -- "--topology ${CONFIGDIR}/topology-node-$1.json "
        printf -- "--database-path ${DBDIR}/$1 "
        printf -- "--socket-path ${SOCKETDIR}/$1 "
        printf -- "--config ${CONFIGDIR}/configuration-node-$1${SUBCONFIG}.yaml "
        printf -- "--host-addr ${HOSTADDR} "
        printf -- "--port $((PORTBASE + $1)) "
}

function nodeargs_byron () {
        nodeargs_common "$1"
        printf -- "--signing-key            ${GENESISDIR}/delegate-keys.%03d.key " "$1"
        printf -- "--delegation-certificate ${GENESISDIR}/delegation-cert.%03d.json " "$1"
}

function nodeargs_shelley () {
        nodeargs_common "$1"
        printf -- "--shelley-kes-key ${GENESISDIR}/node$1/kes.skey "
        printf -- "--shelley-vrf-key ${GENESISDIR}/node$1/vrf.skey "
        printf -- "--shelley-operational-certificate ${GENESISDIR}/node$1/node.cert "
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

for i in $(seq ${CLUSTER_INDEX_START:-0} $((${CLUSTER_INDEX_START:-0} + 2)))
do tmux select-pane -t $((i - ${CLUSTER_INDEX_START:-0}))
   tmux send-keys \
     "${TMUX_ENV_PASSTHROUGH[*]}

      cd '${BASEDIR}';
      . ${__COMMON_SRCROOT}/scripts/lib.sh;
      . ${__COMMON_SRCROOT}/scripts/lib-cli.sh;
      . ${__COMMON_SRCROOT}/scripts/lib-node.sh;

      set -x
      run 'cardano-node' run $(nodeargs_$era $i)" ${REDIRSTDERR} \
     C-m
done
tmux select-pane -t 0
