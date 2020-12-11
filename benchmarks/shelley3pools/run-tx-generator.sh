#!/usr/bin/env bash

BASEDIR=$(realpath $(dirname "$0"))
. ${BASEDIR}/../../scripts/common.sh

CONFIGDIR=${BASEDIR}/configuration

. "${CONFIGDIR}"/parameters

# arguments
TARGETNODES=`for N in $targetnodes; do echo -n "--target-node (\"127.0.0.1\",$((3000 + $N))) "; done`

localsock=$BASEDIR/logs/sockets/1

era=${1:-shelley}
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

case $era in
byron )
        args+=(
          --genesis-funds-key ${GENESISDIR_byron}/poor-keys.000.skey
        )
        run 'cardano-tx-generator' "${args[@]}";;
shelley )
        args+=(
          --genesis-funds-key ${GENESISDIR_shelley}/utxo-keys/utxo1.skey
        )
        run 'cardano-tx-generator' "${args[@]}";;
cardano-shelley )
        txio="$($BASEDIR/prepare_genesis_expenditure.sh)"
        if test -z "$txio"
        then echo "ERROR: couldn't obtain funds for generator">&2; exit 1; fi
        args+=(
          --shelley
          --utxo-funds-key    ${GENESISDIR_shelley}/utxo-keys/utxo1.skey
          --tx-out            "$(jq .txout <<<$txio --raw-output)"
          --tx-in             "$(jq .txin  <<<$txio --raw-output)"
          # --fail-on-submission-errors
        )
        set +e
        echo run 'cardano-tx-generator' "${args[@]}"
        run 'cardano-tx-generator' "${args[@]}" |
                grep 'launching Tx\|MsgAcceptVersion\|SubServFed\|Error\|Summary' |
                tee "$BASEDIR"/logs/generator-summary.log;;
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
