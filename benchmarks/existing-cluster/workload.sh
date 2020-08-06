#!/usr/bin/env bash
# shellcheck disable=SC1090

BASEDIR=$(realpath "$(dirname "$0")")
. "$BASEDIR"/../../scripts/common.sh

prebuild 'cardano-tx-generator' || exit 1

usage() {
        cat <<EOF
Usage: $(basename "$0") COMMAND ARGS..

Commands:

  sync-chain           Sync a local proxy node DB against the cluster specified
                         in the 'configuration' subdirectory of
                         $BASEDIR

  workload-from-coin SIGKEYFILE TXID TXIX ADDR COIN
                       Begin a transaction generation workload, by using
                         the pair of a synced local proxy node and txgen,
                         targeting the latter at the node specified by
                         $BASEDIR/topology.json
                       Initial funds are obtained from the specified coin

  workload-from-utxo SIGKEYFILE UTXO-FILE
                       Begin a transaction generation workload, by using
                         the pair of a synced local proxy node and txgen,
                         targeting the latter at the node specified by
                         $BASEDIR/topology.json
                       Initial funds are obtained from the specified UTxO
                         JSON file of the following (informal) schema:
                         [[{ "txid": TXID, "txix": TXIX },
                           { "addr": ADDR, "coin": LOVELACE }]
                         , ...]
                         Which is, essentially, a list of TxIn/TxOut pairs.

EOF
} >&2

txs=4000
tps=20
targets=2
fee=400000

main() {
        cmd=${1:-hel}; shift
        case "$cmd" in
                sync-chain | sync ) op_sync_chain;;
                workload-from-* )   op_workload "$cmd" "$@";;
                * ) usage; exit 1;;
        esac
}

op_sync_chain() {
        run cardano-node "${args_proxy[@]}"
        run cardano-tx-generator "${args_txgen[@]}"
}

node_pid=
shutdown_node() {
        test -n "$node_pid" && {
                echo "--( shutting down node PID $node_pid.."
                kill "$node_pid"
        }
        rm -f "$BASEDIR"/socket
}
trap shutdown_node EXIT

op_workload() {
        mode=$1; shift
        skey=${1:?$(usage)}; shift
        case "$mode" in
                workload-from-coin )
                        txid=${1:?$(usage)}
                        txix=${2:?$(usage)}
                        addr=${3:?$(usage)}
                        coin=${4:?$(usage)}
                        args_funds=(
                                --utxo-funds-key "$skey"
                                --tx-out         "$addr+$coin"
                                --tx-in          "$txid"'#'"$txix"
                        );;
                workload-from-utxo )
                        utxo_file=${1:?$(usage)}
                        args_funds=(
                                --split-utxo-funds-key "$skey"
                                --split-utxo           "$utxo_file"
                        );; esac
        local nodeaddr nodeport n
        nodeport=$(jq '.Producers[0].port' \
                      $BASEDIR/configuration/topology.json -r)
        args_txgen=()
        for n in $(seq 1 $targets)
        do nodeaddr=$(jq '.Producers[0].addr' \
                        $BASEDIR/configuration/topology.json -r)
           if test -n "$(echo $nodeaddr | grep 'amazon\|aws\|com\|net\|org')"
           then nodeip=$(echo $nodeaddr | \
                                 xargs host | head -n1 | sed 's/.*has address //')
           else nodeip=$nodeaddr; fi
           echo "--( target node $n:  $nodeaddr:$nodeip:$nodeport" >&2
           args_txgen+=(
             --target-node    '("'$nodeip'",'$nodeport')'
           )
        done

        args_txgen+=(
        --config         "$BASEDIR"/configuration/config-txgen.json
        --socket-path    "$BASEDIR"/socket
        --num-of-txs     $txs
        --add-tx-size    0
        --inputs-per-tx  1
        --outputs-per-tx 1
        --tx-fee         $fee
        --tps            $tps
        --init-cooldown  60

        --shelley        ## Cardano mode in Shelley era
        --addr-mainnet
        )
        args_txgen+=("${args_funds[@]}")

        rm -rf logs

        echo "--( parameters:  txs=$txs, tps=$tps, targets=$targets, fee=$fee"
        echo "--( starting node, logs in ./logs"
        mkdir -p logs
        run cardano-node         "${args_proxy[@]}" >logs/node.log 2>&1 &
        node_pid=$!
        echo -n "--( waiting for node (pid $node_pid) socket ($BASEDIR/socket) to appear:  "
        while test ! -e "$BASEDIR/socket"; do echo -n .; sleep 1; done; echo "ok, got it."
        run cardano-tx-generator "${args_txgen[@]}" 2>&1 | tee logs/generator.log
}

args_proxy=(
        'run'
        --config         "$BASEDIR"/configuration/config-proxy.json
        --database-path  "$BASEDIR"/db
        --socket-path    "$BASEDIR"/socket
        --topology       "$BASEDIR"/configuration/topology.json
)

main "$@"
