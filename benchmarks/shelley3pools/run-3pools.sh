#!/bin/sh

NODECMD="stack --nix exec cardano-node --"

# the host address and interface the node listens:
HOSTADDR=127.0.0.1

# the nodes will listen on ports starting with:
PORTBASE=3000

GENESISDIR=configuration/genesis

# redirect stderr if LiveView active
REDIRSTDERR="2>/dev/null"
REDIRSTDERR=

tmux split-window -v
tmux split-window -h

for N in 1 2 3
do tmux select-pane -t $((N - 1))
   tmux send-keys \
     "${NODECMD} run \
            --topology configuration/topology-node-${N}.json \
            --database-path db/${N} \
            --socket-path logs/sockets/${N} \
            --host-addr ${HOSTADDR} --port $((PORTBASE + N - 1)) \
            --config configuration/configuration-node-${N}.yaml \
            --shelley-kes-key ${GENESISDIR}/node${N}/kes.skey \
            --shelley-vrf-key ${GENESISDIR}/node${N}/vrf.skey \
            --shelley-operational-certificate ${GENESISDIR}/node${N}/node.cert \
            " ${REDIRSTDERR} \
     C-m
done
tmux select-pane -t 0
