#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2016

BASEDIR=$(realpath "$(dirname "$0")")
. "$BASEDIR"/../../scripts/common.sh

addr=$1

args=(
        shelley query utxo
        --testnet-magic 42
        --address "$addr"
)

{
        echo -n '['
        CARDANO_NODE_SOCKET_PATH=socket \
            cardano-cli "${args[@]}" |
            grep -F '"' |
            sed 's_ [ ]*_, _g;
                 s_^_, [ "id": _; s_$_ ]_;
                 s_"id": \(".*"\), \([0-9][0-9]*\), \([0-9][0-9]*\)_{ "txid": \1, "txix": \2 }, { "addr": "'"$addr"'", "coin": \3 }_'
        echo ']'
} | sed 's_\[,_\[_'
