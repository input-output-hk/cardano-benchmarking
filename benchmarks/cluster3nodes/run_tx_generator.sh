#!/usr/bin/env bash
# shellcheck disable=SC1090

BASEDIR="$(realpath "$(dirname "$0")")"
. "$(realpath "${BASEDIR}"/../../scripts/common.sh)"

CONFIGDIR=${BASEDIR}/configuration-$era

. "${CONFIGDIR}"/parameters

# arguments
TARGETNODES=`for N in $targetnodes; do echo -n "--target-node (\"127.0.0.1\",$((3000+$N))) "; done`

localsock=$BASEDIR/sockets/1

args=(
  --config                 ${CONFIGDIR}/configuration-generator.yaml
  --delegation-certificate ${CONFIGDIR}/genesis/delegation-cert.000.json
  --genesis-file           ${CONFIGDIR}/genesis/genesis.json
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
        args=("${args[@]}"
              --signing-key ${CONFIGDIR}/genesis/delegate-keys.000.key
              --sig-key     ${CONFIGDIR}/genesis/delegate-keys.000.key
              --sig-key     ${CONFIGDIR}/genesis/delegate-keys.001.key
              --sig-key     ${CONFIGDIR}/genesis/delegate-keys.002.key
              --sig-key     ${CONFIGDIR}/genesis/delegate-keys.003.key
              --sig-key     ${CONFIGDIR}/genesis/delegate-keys.004.key
              --sig-key     ${CONFIGDIR}/genesis/delegate-keys.005.key
             )
        run 'cardano-tx-generator' "${args[@]}";;
shelley )
        bft_ids=$(echo $STAKEPOOLS |
                  sed 's_ _\n_' |
                  jq '. as $pools
                     | [range(1; $nnodes + 1)] as $nodes
                     | ($nodes - $pools)
                     ' --argjson nnodes $NNODES --slurp)
        bft_first_id=$(jq <<<$bft_ids --raw-output '.[0]')
        bft_rest_ids=$(jq <<<$bft_ids --raw-output '.[1:] | join(" ")')

        args=("${args[@]}"
              --signing-key ${CONFIGDIR}/genesis/utxo-keys/utxo${bft_first_id}.skey
              --sig-key     ${CONFIGDIR}/genesis/utxo-keys/utxo${bft_first_id}.skey
             )
        for id in $bft_rest_ids
        do args+=(--sig-key ${CONFIGDIR}/genesis/utxo-keys/utxo$id.skey); done

        set +e
        run      'cardano-tx-generator' "${args[@]}"
        echo run 'cardano-tx-generator' "${args[@]}";;
*) echo "ERROR:  unknown era '$era'" >&2;;
esac

../../scripts/analyse.sh
