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
        --supply           "$TOTAL_SUPPLY"
        --start-time       "$(date --iso-8601=s --date=@$start_time --utc | cut -c-19)Z"
       )
## update genesis from template
$cli shelley genesis create "${params[@]}"

## create KES, VRF, certs per node
for N in $(seq 1 $NNODES); do Pools[$N]=1; done
for N in $STAKEPOOLS;      do Pools[$N]=2; done
for N in $(seq 1 $NNODES); do
    mkdir -p "$gendir"/node${N}/cold

    $cli shelley node key-gen-KES \
      --verification-key-file "$gendir"/node${N}/kes.vkey \
      --signing-key-file "$gendir"/node${N}/kes.skey

    #### cold keys (do not copy to production system)
    if [ ${Pools[$N]} -eq 2 ]; then   ## Stakepool node
        $cli shelley node key-gen \
        --cold-verification-key-file "$gendir"/node${N}/cold/operator.vkey \
        --cold-signing-key-file "$gendir"/node${N}/cold/operator.skey \
        --operational-certificate-issue-counter-file "$gendir"/node${N}/cold/operator.counter
        $cli shelley node key-gen-VRF \
        --verification-key-file "$gendir"/node${N}/vrf.vkey \
        --signing-key-file "$gendir"/node${N}/vrf.skey
    else ## BFT node
        SRCN=1
        ln -s ../../delegate-keys/delegate${SRCN}.skey    "$gendir"/node${N}/cold/operator.skey
        ln -s ../../delegate-keys/delegate${SRCN}.vkey    "$gendir"/node${N}/cold/operator.vkey
        ln -s ../../delegate-keys/delegate${SRCN}.counter "$gendir"/node${N}/cold/operator.counter
        ln -s ../delegate-keys/delegate${SRCN}.vrf.skey   "$gendir"/node${N}/vrf.skey
        ln -s ../delegate-keys/delegate${SRCN}.vrf.vkey   "$gendir"/node${N}/vrf.vkey
    fi

    # certificate (adapt kes-period for later certs)
    $cli shelley node issue-op-cert \
      --hot-kes-verification-key-file         "$gendir"/node${N}/kes.vkey \
      --cold-signing-key-file                 "$gendir"/node${N}/cold/operator.skey \
      --operational-certificate-issue-counter "$gendir"/node${N}/cold/operator.counter \
      --kes-period 0 \
      --out-file "$gendir"/node${N}/node.cert
done

# === delegation ===

## prepare addresses & set up genesis staking
mkdir -p "$gendir"/addresses

pools_json='{}'
stake_json='{}'
initial_funds_json='{}'
for N in $STAKEPOOLS
do
   ### Payment address keys
   $cli shelley address key-gen \
        --verification-key-file         "$gendir"/addresses/pool-owner$N.vkey \
        --signing-key-file              "$gendir"/addresses/pool-owner$N.skey

   ### Stake address keys
   $cli shelley stake-address key-gen \
        --verification-key-file         "$gendir"/addresses/pool-owner$N-stake.vkey \
        --signing-key-file              "$gendir"/addresses/pool-owner$N-stake.skey

   ### Payment addresses
   $cli shelley address build \
        --payment-verification-key-file "$gendir"/addresses/pool-owner$N.vkey \
        --stake-verification-key-file   "$gendir"/addresses/pool-owner$N-stake.vkey \
        --testnet-magic ${MAGIC} \
        --out-file "$gendir"/addresses/pool-owner$N.addr

    pool_id=$($cli shelley stake-pool id \
               --verification-key-file   "$gendir"/node${N}/cold/operator.vkey  --output-format hex)
    pool_vrf=$($cli shelley node key-hash-VRF \
               --verification-key-file  "$gendir"/node${N}/vrf.vkey)
    deleg_staking=$($cli shelley stake-address key-hash \
               --stake-verification-key-file "$gendir"/addresses/pool-owner${N}-stake.vkey)
    initial_addr=$($cli shelley address info --address $(cat "$gendir"/addresses/pool-owner$N.addr) |
                   jq '.base16' --raw-output)
    params=(
    --arg      poolId          "$pool_id"
    --arg      vrf             "$pool_vrf"
    --arg      delegStaking    "$deleg_staking"
    --arg      initialAddr     "$initial_addr"
    --argjson  initialPoolCoin $((POOL_SUPPLY / $(echo $STAKEPOOLS | wc -w)))
    )
    pools_json=$(jq '
      . +
      { "\($poolId)":
        { publicKey:     $poolId
        , vrf:           $vrf
        , rewardAccount:
          { network:     "Testnet"
          , credential:
            { "key hash": $delegStaking
            }
          }
        , owners:        []
        , relays:        []
        , pledge:        0
        , cost:          0
        , margin:        0
        , metadata: null
        }
      }
      ' <<<$pools_json "${params[@]}" )
    stake_json=$(jq '
      . +
      { "\($delegStaking)": $poolId
      }
      ' <<<$stake_json "${params[@]}" )
    stake_json=$(jq '
      . +
      { "\($delegStaking)": $poolId
      }
      ' <<<$stake_json "${params[@]}" )
    initial_funds_json=$(jq '
      . +
      { "\($initialAddr)": $initialPoolCoin
      }
      ' <<<$initial_funds_json "${params[@]}" )
done

sed -i 's_Genesis UTxO verification key_PaymentVerificationKeyShelley_' \
    "$gendir"/utxo-keys/utxo1.vkey
sed -i 's_Genesis UTxO signing key_PaymentSigningKeyShelley_' \
    "$gendir"/utxo-keys/utxo1.skey
initial_addr_non_pool_bech32=$($cli shelley address build \
                               --payment-verification-key-file "$gendir"/utxo-keys/utxo1.vkey \
                               --testnet-magic ${MAGIC})
initial_addr_non_pool_base16=$($cli shelley address info --address "$initial_addr_non_pool_bech32" |
                               jq '.base16' --raw-output)

params=(--argjson pools                   "$pools_json"
        --argjson stake                   "$stake_json"
        --argjson initialFundsOfPools     "$initial_funds_json"
        --arg     initialFundsNonPoolAddr "$initial_addr_non_pool_base16"
        --argjson initialFundsNonPoolCoin $((TOTAL_SUPPLY - POOL_SUPPLY))
       )
jq '. +
   { staking:
     { pools: $pools
     , stake: $stake
     }
   , initialFunds:
     ({ "\($initialFundsNonPoolAddr)":
           $initialFundsNonPoolCoin
      } + $initialFundsOfPools)
   }
   ' "${params[@]}" \
 < "$gendir"/genesis.json        > "$gendir"/genesis.json.
mv "$gendir"/genesis.json.         "$gendir"/genesis.json

$cli shelley genesis hash --genesis "$gendir"/genesis.json |
        tr -d '"' > "$gendir"/GENHASH
