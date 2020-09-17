#!/usr/bin/env bash
# shellcheck disable=SC1090

set -e

basedir=$(realpath "$(dirname "$0")")
. "$basedir"/../../scripts/common.sh
. "$basedir"/configuration/parameters

cd "$basedir"

gendir=$GENESISDIR_shelley
cli=${CLICMD:-'run cardano-cli'}

coin=4000000000000000

byron_delegate_key=$GENESISDIR_byron/delegate-keys.000.key
byron_poor_key=$GENESISDIR_byron/poor-keys.000.key
utxo_ext_vkey=$GENESISDIR_shelley/utxo-keys/utxo1.ext.vkey
utxo_skey=$GENESISDIR_shelley/utxo-keys/utxo1.skey
utxo_vkey=$GENESISDIR_shelley/utxo-keys/utxo1.vkey

rm -f $utxo_ext_vkey $utxo_vkey $utxo_skey

tmpfiles=()
atexit() {
        rm -f ${tmpfiles[*]}
}
trap atexit EXIT
new_temp_file() {
        local f
        f=$(mktemp -t "$1-XXXXXXXX" )
        tmpfiles+=($f)
        echo "$f"
}

get_funds_via_byron_payment_key_byron() {
        local src_skey=$1 target_skey=$2 target_vkey=$3 coin=$4
        local src_key_addr target_key_addr
        local byron_inter_skey txfile
        byron_inter_skey=$(new_temp_file "byron-inter")
        txfile=$(new_temp_file "genesis-expenditure")

        rm -f $byron_inter_skey
        ## create intermediate key
        $cli keygen \
             --byron-formats \
             --no-password \
             --secret $byron_inter_skey
        target_key_addr=$($cli signing-key-address \
                               --byron-formats \
                               --testnet-magic $MAGIC \
                               --secret $byron_inter_skey | head -n1)
        ## move funds from genesis to intermediate
        src_key_addr=$($cli signing-key-address \
                            --byron-formats \
                            --testnet-magic $MAGIC \
                            --secret $src_skey | head -n1)
        $cli issue-genesis-utxo-expenditure \
             --byron-formats \
             --genesis-json $GENESISDIR_byron/genesis.json \
             --testnet-magic $MAGIC \
             --wallet-key $src_skey \
             --txout "(\"$target_key_addr\",$coin)" \
             --rich-addr-from $src_key_addr \
             --tx $txfile
        CARDANO_NODE_SOCKET_PATH=logs/sockets/1 \
        $cli submit-tx \
             --testnet-magic $MAGIC \
             --tx $txfile
        ## convert intermediate key to Shelley format
        $cli shelley key convert-byron-key \
             --byron-payment-key-type \
             --byron-signing-key-file $byron_inter_skey \
             --out-file $target_skey
        $cli shelley key verification-key \
             --signing-key-file $target_skey \
             --verification-key-file $target_vkey
        sed -i 's/PaymentVerificationKeyByron_ed25519_bip32/GenesisUTxOVerificationKey_ed25519/' $target_vkey
}

wait_seconds() {
        n=$1 expl="$2"
        echo -n "--( waiting $expl:  $n"
        while printf "\b\b\b%3d" $n
              test $n -gt 0
        do n=$((n-1)); sleep 1s; done
        echo
} >&2

query_addr_txin() {
        local addr=$1 coin=$2; shift 2
        echo "$(CARDANO_NODE_SOCKET_PATH=logs/sockets/1 \
                cardano-cli shelley query utxo \
                --address "$addr" \
                --testnet-magic "$MAGIC" "$@" | \
                grep "$coin" | \
                cut -d' ' -f1)#0"
}

get_byron_key_addr() {
        $cli signing-key-address \
             --byron-formats \
             --testnet-magic "$MAGIC" \
             --secret "$1" | head -n1
}

get_shelley_key_addr() {
        $cli shelley address build \
            --testnet-magic "$MAGIC" \
            --payment-verification-key-file "$1"
}

move_genesis_byron() {
        local key=$1 srcaddr=$2 toaddr=$3 coin=$4

        local txin
        txin="$(query_addr_txin "$srcaddr" "$coin" --cardano-mode)"
        if test -z "$txin"
        then echo "ERROR: couldn't determine initial TxIn for addr $srcaddr">&2; exit 1; fi
        echo "-- intermediate TxIn:        $txin" >&2

        local tx
        tx=$(new_temp_file "move.tx")
        $cli issue-utxo-expenditure \
             --byron-formats \
             --testnet-magic "$MAGIC" \
             --wallet-key "$key" \
             --txin "(\"$txin\",0)" \
             --txout "$toaddr:$coin" \
             --tx "$tx"

        $cli submit-tx \
             --testnet-magic "$MAGIC" \
             --tx "$tx"
}

move_utxo_shelley() {
        local key=$1 srcaddr=$2 toaddr=$3 coin=$4

        local txin
        txin="$(query_addr_txin "$srcaddr" "$coin" --cardano-mode)"
        if test -z "$txin"
        then echo "ERROR: couldn't determine initial TxIn for addr $srcaddr">&2; exit 1; fi
        echo "-- shelley TxIn:              $txin" >&2

        local txbody
        txbody=$(new_temp_file "move.txbody")
        $cli shelley transaction build-raw \
             --tx-in             "$txin" \
             --tx-out            "$toaddr+$coin" \
             --ttl               10000000 \
             --fee               0 \
             --out-file          "$txbody"
        local tx
        tx=$(new_temp_file "move.tx")
        $cli shelley transaction sign \
             --tx-body-file      "$txbody" \
             --signing-key-file  "$key" \
             --testnet-magic     "$MAGIC" \
             --out-file          "$tx"
        CARDANO_NODE_SOCKET_PATH=logs/sockets/1 \
        $cli shelley transaction submit \
             --tx-file           "$tx" \
             --testnet-magic     "$MAGIC"
}

get_funds_directly_delegate() {
        local src_skey=$1 target_skey=$2 target_vkey=$3 coin=$4
        local utxo_ext_vkey
        utxo_ext_vkey=$(new_temp_file "utxo.ext.vkey")

        $cli shelley key convert-byron-key \
             --byron-genesis-delegate-key-type \
             --byron-signing-key-file $src_skey \
             --out-file $target_skey
        $cli shelley key verification-key \
             --signing-key-file $target_skey \
             --verification-key-file $utxo_ext_vkey
        $cli shelley key non-extended-key \
             --extended-verification-key-file $utxo_ext_vkey \
             --verification-key-file $target_vkey
        sed -i 's/GenesisDelegateVerificationKey_ed25519/GenesisUTxOVerificationKey_ed25519/' $target_vkey
        rm -f $utxo_ext_vkey
}

get_funds_directly_poor() {
        local src_skey=$1 target_skey=$2 target_vkey=$3 coin=$4
        local utxo_ext_vkey
        utxo_ext_vkey=$(new_temp_file "utxo.ext.vkey")

        $cli shelley key convert-byron-key \
             --byron-payment-key-type \
             --byron-signing-key-file $src_skey \
             --out-file $target_skey
        $cli shelley key verification-key \
             --signing-key-file $target_skey \
             --verification-key-file $target_vkey
}

get_funds_poor_via_shelley() {
        local src_skey=$1 target_skey=$2 target_vkey=$3 coin=$4
        local inter_skey inter_vkey
        inter_skey=$(new_temp_file "inter.skey")
        inter_vkey=$(new_temp_file "inter.vkey")

        $cli shelley key convert-byron-key \
             --byron-payment-key-type \
             --byron-signing-key-file $src_skey \
             --out-file $inter_skey
        $cli shelley key verification-key \
             --signing-key-file $inter_skey \
             --verification-key-file $inter_vkey

        local src_addr
        src_addr=$(get_byron_key_addr "$src_skey")

        $cli shelley address key-gen \
            --verification-key-file $target_vkey \
            --signing-key-file      $target_skey

        local to_addr
        to_addr=$(get_shelley_key_addr "$target_vkey")
        # to_addr_byron=$(cardano-cli shelley address info \
        #                              --address "$to_addr" |
        #                 jq .base16 --raw-output)

        move_utxo_shelley "$inter_skey" "$src_addr" "$to_addr" "$coin"
}

wait_seconds 105 "until the first epoch passes"

# src_key=$byron_delegate_key
src_key=$byron_poor_key
echo "-- Byron funds source key:    $src_key" >&2

src_addr=$(get_byron_key_addr "$src_key")

if test -z "$src_addr"
then echo "ERROR: couldn't determine address for $src_key">&2; exit 1; fi
echo "-- Byron source key address:  $src_addr" >&2

# get_funds_directly_poor "$src_key" "$utxo_skey" "$utxo_vkey" "$coin"
# get_funds_directly_delegate "$src_key" "$utxo_skey" "$utxo_vkey" "$coin"
# get_funds_via_byron_payment_key_byron "$src_key" "$utxo_skey" "$utxo_vkey" "$coin"
get_funds_poor_via_shelley "$src_key" "$utxo_skey" "$utxo_vkey" "$coin"
echo "-- derived Shelley keys:      $utxo_skey $utxo_vkey" >&2

addr=$(get_shelley_key_addr $utxo_vkey)
if test -z "$addr"
then echo "ERROR: couldn't determine address for $utxo_vkey">&2; exit 1; fi
echo "-- derived Shelley key addr:  $addr" >&2

txout="$addr+$coin"

wait_seconds 10 "until the funds UTxO settles"

txin="$(query_addr_txin "$addr" "$coin")"
if test -z "$txin"
then echo "ERROR: couldn't determine initial TxIn for $utxo_vkey">&2; exit 1; fi
echo "-- funds TxIn:                $txin" >&2


jq '{ txout: "\($txout)"
    , txin:  $txin
    }
'  --arg txin  "$txin" \
   --arg txout "$txout" \
   <<<0

{
        echo -e "\nUTxO of src_addr ($src_addr):"
        CARDANO_NODE_SOCKET_PATH=logs/sockets/1 \
        $cli \
            shelley query utxo \
            --testnet-magic $MAGIC \
            --address "$src_addr"

        echo -e "\nUTxO of addr ($addr):"
        CARDANO_NODE_SOCKET_PATH=logs/sockets/1 \
        $cli \
            shelley query utxo \
            --testnet-magic $MAGIC \
            --address "$addr"
} >&2
