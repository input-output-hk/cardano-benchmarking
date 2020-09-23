#!/usr/bin/env bash

BASEDIR=$(realpath $(dirname "$0"))
. ${BASEDIR}/../../scripts/common.sh
. ${BASEDIR}/configuration/parameters

#NODECMD="stack --nix exec cardano-node --"
NODECMD=${NODECMD:-"run cardano-node"}

# the host address and interface the node listens:
HOSTADDR=127.0.0.1

# the nodes will listen on ports starting with:
PORTBASE=3000

# redirect stderr if LiveView active
REDIRSTDERR="2>/dev/null"
REDIRSTDERR=

TMUX_ENV_PASSTHROUGH=(
         "export SCRIPTS_LIB_SH_MODE=${SCRIPTS_LIB_SH_MODE};"
         "export __COMMON_SRCROOT=${__COMMON_SRCROOT};"
         "export DEFAULT_DEBUG=${DEFAULT_DEBUG};"
         "export DEFAULT_VERBOSE=${DEFAULT_VERBOSE};"
         "export DEFAULT_TRACE=${DEFAULT_TRACE};"
         "export allow_path_exes=${allow_path_exes};"
         "$(nix_cache_passthrough)"
)

tmux split-window -v
tmux split-window -h

mkdir -p logs
for N in 1 2 3
do N1=$((N - 1))
   N13=$(printf "%03d" $N1)
   tmux select-pane -t $N1

   common_args=(
           --topology               configuration/topology-node-"$N".json
           --database-path          db/"$N"
           --socket-path            logs/sockets/"$N"
           --host-addr              "$HOSTADDR" --port $((PORTBASE + N - 1))
           --config                 configuration/configuration-node-"$N".yaml
           --signing-key            "$GENESISDIR_byron"/delegate-keys."$N13".key
           --delegation-certificate "$GENESISDIR_byron"/delegation-cert."$N13".json
   )
   if test "$N" -le "$num_pools"
   then # Pools nodes come first
           ix=$N
           mode_args=(
            --shelley-kes-key                 "$GENESISDIR_shelley"/pools/kes"$ix".skey
            --shelley-vrf-key                 "$GENESISDIR_shelley"/pools/vrf"$ix".skey
            --shelley-operational-certificate "$GENESISDIR_shelley"/pools/opcert"$ix".cert
            )
   else # BFT node is last
           ix=$((N - num_pools))
           mode_args=(
            --shelley-kes-key                 "$GENESISDIR_shelley"/delegate-keys/delegate"$ix".kes.skey
            --shelley-vrf-key                 "$GENESISDIR_shelley"/delegate-keys/delegate"$ix".vrf.skey
            --shelley-operational-certificate "$GENESISDIR_shelley"/delegate-keys/opcert"$ix".cert
            )
   fi

   tmux send-keys \
     "${TMUX_ENV_PASSTHROUGH[*]}

      cd '${BASEDIR}';
      . ${__COMMON_SRCROOT}/scripts/lib.sh;
      . ${__COMMON_SRCROOT}/scripts/lib-cli.sh;
      . ${__COMMON_SRCROOT}/scripts/lib-nix.sh;
      . ${__COMMON_SRCROOT}/scripts/lib-node.sh;

      ${NODECMD} run ${common_args[*]} ${mode_args[*]} " $REDIRSTDERR \
     C-m
done
tmux select-pane -t 0
