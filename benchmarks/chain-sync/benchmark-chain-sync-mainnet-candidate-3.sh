#!/usr/bin/env bash

# >> cpu time limit in seconds
TIME_LIMIT=$((6*60))

BASEPATH=$(realpath $(dirname $0))

DATADIR=state-node-mainnet-candidate3
if [ -d $DATADIR ]; then
  rm -rf $DATADIR
fi
mkdir -p $DATADIR
cd $DATADIR

# remove old log files
rm node-0*

#set -euo pipefail

date --iso-8601=seconds > STARTTIME

if [[ $1 == 'stack' ]]; then
  NODE="stack --nix exec cardano-node -- "
  shift
elif [[ $1 == 'cabal' ]]; then
  NODE="cabal v2-run exe:cardano-node -- "
  shift
else
  # Default to local node
  NODE="${BASEPATH}/../../bin/cardano-node "
fi

#timeout ${TIME_LIMIT} ${NODE} \
${NODE} \
  run \
  --config ${BASEPATH}/configuration/mainnet_candidate_3-config.json \
  --database-path ./db \
  --topology ${BASEPATH}/configuration/mainnet_candidate_3-topology.json \
  --socket-path /tmp/socket-mc-chain-sync \
  --host-addr 127.0.0.1 \
  --port 7774 \
   \
  $@

# "+RTS -T -I0 -N2 -A16m -RTS"

cd ..
${BASEPATH}/analyse-logs.sh mainnet-candidate3

