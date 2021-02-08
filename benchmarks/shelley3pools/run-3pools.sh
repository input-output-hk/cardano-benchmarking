#!/usr/bin/env bash

BASEDIR=$(realpath $(dirname "$0"))
. ${BASEDIR}/../../scripts/common.sh
. ${BASEDIR}/configuration/parameters

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

mkdir -p logs/analysis
ln -s 'logs-node-1' 'logs/analysis/logs-explorer'
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
   if test "$N" -le "$num_non_bulk_pools"
   then # Non-bulk pools come first
           ix=$N
           mode_args=(
            --shelley-kes-key                 "$GENESISDIR_shelley"/pools/kes"$ix".skey
            --shelley-vrf-key                 "$GENESISDIR_shelley"/pools/vrf"$ix".skey
            --shelley-operational-certificate "$GENESISDIR_shelley"/pools/opcert"$ix".cert
           )
   elif test "$N" -le "$num_pools"
   then # Bulk pools then
           ix=$((N - num_non_bulk_pools))
           mode_args=(
            --bulk-credentials-file           "$GENESISDIR_shelley"/pools/bulk"$ix".creds
            +RTS -N4 -RTS
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

      if test -n \"${DEFAULT_VERBOSE}\"
      then verbose=t
      fi
      if test -n \"${DEFAULT_DEBUG}\"
      then verbose=t; debug=t
      fi
      if test -n \"${DEFAULT_TRACE}\"
      then verbose=t; debug=t; trace=t; set -x
      fi

      cd '${BASEDIR}';
      . ${__COMMON_SRCROOT}/scripts/lib.sh;
      . ${__COMMON_SRCROOT}/scripts/lib-cli.sh;
      . ${__COMMON_SRCROOT}/scripts/lib-nix.sh;
      . ${__COMMON_SRCROOT}/scripts/lib-node.sh;

      echo -e '\n\n\nFollow the link for RTView:  http://localhost:12799/\n             ...for EKG:     http://localhost:$((12787+N))/\n\n';

      ${NODECMD} run ${common_args[*]} ${mode_args[*]} " $REDIRSTDERR \
     C-m
done
tmux select-pane -t 0
