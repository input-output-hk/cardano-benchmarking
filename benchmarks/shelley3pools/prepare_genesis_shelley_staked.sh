#!/usr/bin/env bash
# shellcheck disable=SC1090

set -e

basedir=$(realpath "$(dirname "$0")")
. "$basedir"/../../scripts/common.sh
. "$basedir"/configuration/parameters

cd "$basedir"

gendir=$GENESISDIR_shelley
cli=${CLICMD:-'run cardano-cli'}

###
### Overview
###
### This script generates a genesis with a set of pre-made stake pools.
###
### Artifacts produced:
###
###  For all nodes:
###  - $NNODES KES & VRF operational keypairs
###      - $gendir/node${N}/kes.vkey
###        - used as runtime parameter for each node,
###          and to issue the operational certificate
###      - $gendir/node${N}/vrf.vkey
###        - used as runtime parameter for each node
###  - $NNODES node operational certificates:
###      - $gendir/node${N}/node.cert
###        - used as runtime parameter for each node
###
###  For the BFT node:
###  - 1 genesis stake keypair:
###      - $gendir/genesis-keys/genesis1.{skey,vkey}
###        - not actually used afterwards (in this setup)
###  - 1 delegate keypair:
###      - $gendir/delegate-keys/delegate1.{skey,vkey,counter}
###        - used to issue the node operational certificate
###
###  For pool nodes:
###
###  - $NPOOLS sets of following:
###      - cold operator keypair
###        - node${N}/cold/operator.{skey,vkey,counter}
###          - used to identify the pool (as HASH(vkey))
###            in the genesis staking relationship,
###            and as runtime parameter for each pool
###      - genesis UTxO keypair for stake funds:
###        - $gendir/addresses/pool-owner$N.{skey,vkey}
###          - controls genesis UTxO funds and stake
###          - derives the pool-assigned genesis UTxO address
###      - stake keypair:
###        - $gendir/addresses/pool-owner$N-stake.{skey,vkey}
###          - used to derive the pool-assigned genesis UTxO address,
###            and to express the genesis staking relationship
###      - genesis UTxO payment address:
###        - $gendir/addresses/$addr.addr
###          - refers to the the genesis payment and stake keys,
###            and defines the genesis UTxO
###
###  For generator:
###
###  - 1 genesis UTxO keypair holding funds not delegated to pools:
###      - $gendir/utxo-keys/utxo1.{skey,vkey}
###
###  And, finally, $gendir/genesis.json, which defines (among other things):
###   - the genesis delegation (for BFT nodes)
###   - the genesis stake pools
###   - the genesis stake keys
###   - the genesis stake pool delegation
###

rm -rf   "$gendir"
mkdir -p "$gendir"

params=(--genesis-dir      "$gendir"
        --testnet-magic    "$MAGIC"
        --supply           "$TOTAL_SUPPLY"
        --gen-genesis-keys 1
        --gen-utxo-keys    1
       )
$cli shelley genesis create "${params[@]}"

## set parameters in template
params=(--argjson slotLength            "$GEN_SLOTLENGTH"
        --argjson activeSlotsCoeff      "$GEN_ACTIVESLOTSCOEFF"
        --argjson securityParam         "$GEN_SECURITYPARAM"
        --argjson epochLength           "$GEN_EPOCHLENGTH"
        --argjson decentralisationParam "$GEN_DECENTRALISATIONPARAM"
       )
jq '. +
   { slotLength:            $slotLength
   , activeSlotsCoeff:      $activeSlotsCoeff
   , securityParam:         $securityParam
   , epochLength:           $epochLength
   , protocolParams:
     (.protocolParams +
      { decentralisationParam: $decentralisationParam
      })
   }
   ' "${params[@]}" \
 < "$gendir"/genesis.spec.json > "$gendir"/genesis.spec.json.
mv "$gendir"/genesis.spec.json.  "$gendir"/genesis.spec.json

params=(--genesis-dir      "$gendir"
        --testnet-magic    "$MAGIC"
        --start-time       "$(date --iso-8601=s --date=@$start_time --utc | cut -c-19)Z"
        --supply           "$((TOTAL_SUPPLY - POOL_SUPPLY))"
        --supply-delegated "$POOL_SUPPLY"
        --gen-pools        $((num_bulk_pool_pools + num_non_bulk_pools))
        --gen-stake-delegs $((num_bulk_pool_pools * num_delegators_per_pool + num_non_bulk_pools))
        --bulk-pool-cred-files 1
        --bulk-pools-per-file  $num_bulk_pool_pools
        --num-stuffed-utxo $num_stuffed_utxo
       )

## extend template with final parameters
$cli shelley genesis create-staked "${params[@]}"

"$basedir"/hash_genesis.sh
