#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yj jq curl

MAINNET="mainnet"
TESTNET="testnet"

if [ $# -lt 1 ]; then
  echo "call: $0 $MAINNET|$TESTNET"
  exit 1
fi

set -euo pipefail

BASEDIR="$(realpath $(dirname $0))"

CLUSTER="$1"

[[ $CLUSTER == $MAINNET ]] \
  && EXPLORER_URL="https://cardanoexplorer.com" \
  || EXPLORER_URL="https://explorer.cardano-testnet.iohkdev.io"

LOG_CONFIG="$(yj < $BASEDIR/configuration/log-config-ci.yaml)"

CUSTOM_CONFIG="{nodeConfig = builtins.fromJSON ''$LOG_CONFIG'';}"

nix build --out-link launch_node -f $BASEDIR/../.. cardanoNode.scripts.$CLUSTER.node --arg customConfig "$CUSTOM_CONFIG"

rm -rf "./state-node-$CLUSTER"
mkdir "./state-node-$CLUSTER"
cd "./state-node-$CLUSTER"

echo
echo "configuration"
echo "============="
echo "${LOG_CONFIG}"
echo
echo "topology"
echo "========"
TOPOLOGY=`cat ../launch_node | sed -ne 's/.* --topology \([^ ]\+\) .*/\1/p;' | tail -1`
cat "${TOPOLOGY}"
echo
echo

echo "Using explorer at $EXPLORER_URL to retrieve last block height"
LAST_BLOCK_HEIGHT=$(curl --silent $EXPLORER_URL/api/blocks/pages | jq -r '.[][1][0].cbeBlkHeight')
echo "Last block height as reported by cardano-explorer is $LAST_BLOCK_HEIGHT"

../launch_node &
NODE_PID=$!

SYNCED_BLOCKS=0

while [ $SYNCED_BLOCKS -lt $LAST_BLOCK_HEIGHT ]
do
  REMAINING_BLOCKS=$(($LAST_BLOCK_HEIGHT - $SYNCED_BLOCKS))
  echo "$SYNCED_BLOCKS blocks synced, $REMAINING_BLOCKS to go."
  sleep 5
  SYNCED_BLOCKS=$(curl --silent 127.0.0.1:13789/metrics | grep cardano_node_ChainDB_metrics_blockNum_int  | cut -d ' ' -f 2)
done

kill $NODE_PID
sleep 5

cd ..
$BASEDIR/analyse-logs.sh $CLUSTER | tee benchmark-results.log
