#!/usr/bin/env bash

BASEDIR=$(realpath $(dirname "$0"))
. ${BASEDIR}/../../scripts/common.sh

CONFIGDIR=${BASEDIR}/configuration

. "${CONFIGDIR}"/parameters

# arguments
TARGETNODES=`for N in $targetnodes; do echo -n "--target-node (\"127.0.0.1\",$((3000 + $N))) "; done`

localsock=$BASEDIR/logs/sockets/1


#export era="allegra"
export era="mary"
echo "--( args: $*"
echo "--( era:  $era"

args=(
  --config                 ${CONFIGDIR}/configuration-generator.yaml
  --socket-path            $localsock
  --num-of-txs             $numtx
  --add-tx-size            $addsizetx
  --inputs-per-tx          $inputstx
  --outputs-per-tx         $outputstx
  --tx-fee                 $txfee
  --tps                    $tps
  --init-cooldown          $init_cooldown
  ${TARGETNODES}
  )

echo "starting submission to:  $TARGETNODES"
jq '{ meta:
      { profile_content:
        { generator:
          { era: "era"
          , add_tx_size: 100
          , inputs_per_tx: 2
          , outputs_per_tx: 2
          , tps: 100
          , tx_count: 100
          }
        , genesis:
          { active_slots_coeff   : 1
          , delegators           : 1
          , dense_pool_density   : 1
          , epoch_length         : 1
          , parameter_k          : 1
          , max_block_size       : 1
          , max_tx_size          : 1
          , n_pools              : 1
          , slot_duration        : 1
          , utxo                 : 1
          }
        }
      , tag:     "shelley3pools"
      , profile: "shelley3pools"
      , genesis_cache_id: ""
      , timestamp: 1000000000
      }
    }' -n > logs/meta.json
ln -sf ../configuration/genesis-shelley/genesis.json logs/genesis.json

case $era in
shelley )
        args+=(
          --genesis-funds-key ${GENESISDIR_shelley}/utxo-keys/utxo1.skey
          --shelley
        )
        run 'cardano-tx-generator' "${args[@]}";;
mary )
        args+=(
          --genesis-funds-key ${GENESISDIR_shelley}/utxo-keys/utxo1.skey
          --mary
        )
        run 'cardano-tx-generator' "${args[@]}";;
allegra )
        args+=(
          --genesis-funds-key ${GENESISDIR_shelley}/utxo-keys/utxo1.skey
          --allegra
        )
        run 'cardano-tx-generator' "${args[@]}";;

*) echo "ERROR:  unknown era '$era'" >&2;;
esac

wait_seconds() {
        n=$1 expl="$2"
        echo -n "--( waiting $expl:  $n"
        while printf "\b\b\b%3d" $n
              test $n -gt 0
        do n=$((n-1)); sleep 1s; done
        echo
} >&2

wait_seconds 30 'for the mempool transactions to settle in blocks'
# ../../scripts/analyse.sh

./kill-session.sh
