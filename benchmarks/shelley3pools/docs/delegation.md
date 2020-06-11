
# delegation of funds to a pool and getting rewards

(from https://github.com/input-output-hk/cardano-node/blob/master/scripts/shelley-from-scratch/mkfiles.sh)


Make some payment and stake addresses
  user1..n:       will own all the funds in the system, we'll set this up from
                  initial utxo the
  pool-owner1..n: will be the owner of the pools and we'll use their reward
                  account for pool rewards

## settings
CLICMD="stack --nix exec cardano-cli --"

GENESISDIR=configuration/genesis
MAGIC=42

USER_ADDRS="user1 user2"
POOL_ADDRS="pool-owner1 pool-owner2"

POOL_NODES="1 2"

### user N delegates to pool N
USER_POOL_N="1 2"


### stake per address
STAKE=333333334


## prepare addresses

mkdir -p ${GENESISDIR}/addresses

ADDRS="${USER_ADDRS} ${POOL_ADDRS}"
for ADDR in ${ADDRS}; do

  echo -n "$ADDR "

  ### Payment address keys
  ${CLICMD} shelley address key-gen \
      --verification-key-file ${GENESISDIR}/addresses/${ADDR}.vkey \
      --signing-key-file      ${GENESISDIR}/addresses/${ADDR}.skey

  ### Stake address keys
  ${CLICMD} shelley stake-address key-gen \
      --verification-key-file ${GENESISDIR}/addresses/${ADDR}-stake.vkey \
      --signing-key-file      ${GENESISDIR}/addresses/${ADDR}-stake.skey

  ### Payment addresses
  ${CLICMD} shelley address build \
      --payment-verification-key-file ${GENESISDIR}/addresses/${ADDR}.vkey \
      --stake-verification-key-file ${GENESISDIR}/addresses/${ADDR}-stake.vkey \
      --testnet-magic ${MAGIC} \
      --out-file ${GENESISDIR}/addresses/${ADDR}.addr

  ### Stake addresses
  ${CLICMD} shelley stake-address build \
      --stake-verification-key-file ${GENESISDIR}/addresses/${ADDR}-stake.vkey \
      --testnet-magic ${MAGIC} \
      --out-file ${GENESISDIR}/addresses/${ADDR}-stake.addr

  ### Stake addresses registration certs
  ${CLICMD} shelley stake-address registration-certificate \
      --stake-verification-key-file ${GENESISDIR}/addresses/${ADDR}-stake.vkey \
      --out-file ${GENESISDIR}/addresses/${ADDR}-stake.reg.cert

done
echo

## create delegation certs

for N in ${USER_POOL_N}; do

  echo -n "user ${N} -> pool ${N}  "
  ### Stake address delegation certs
  ${CLICMD} shelley stake-address delegation-certificate \
      --stake-verification-key-file ${GENESISDIR}/addresses/user${N}-stake.vkey \
      --cold-verification-key-file  ${GENESISDIR}/node${N}/cold/operator.vkey \
      --out-file ${GENESISDIR}/addresses/user${N}-stake.deleg.cert

  ln -s ../addresses/pool-owner${N}-stake.vkey ${GENESISDIR}/node${N}/owner.vkey
  ln -s ../addresses/pool-owner${N}-stake.skey ${GENESISDIR}/node${N}/owner.skey

done
echo


## make stake pool registration cert

for NODE in ${POOL_NODES}; do
  echo -n "pool ${NODE}  "
  ${CLICMD} shelley stake-pool registration-certificate \
    --testnet-magic ${MAGIC} \
    --pool-pledge 0 --pool-cost 0 --pool-margin 0 \
    --cold-verification-key-file             ${GENESISDIR}/node${NODE}/cold/operator.vkey \
    --vrf-verification-key-file              ${GENESISDIR}/node${NODE}/vrf.vkey \
    --reward-account-verification-key-file   ${GENESISDIR}/node${NODE}/owner.vkey \
    --pool-owner-stake-verification-key-file ${GENESISDIR}/node${NODE}/owner.vkey \
    --out-file                               ${GENESISDIR}/node${NODE}/registration.cert
done
echo

### Now we'll construct one whopper of a transaction that does everything
### just to show off that we can, and to make the script shorter

### We'll transfer all the funds to the user1, which delegates to pool1
### We'll register certs to:
###  1. register the pool-owner1 stake address
###  2. register the stake pool 1
###  3. register the user1 stake address
###  4. delegate from the user1 stake address to the stake pool

for N in ${USER_POOL_N}; do

    echo "move funds to user ${N}, delegate to pool ${N}"
    ${CLICMD} shelley transaction build-raw \
        --ttl 1000 \
        --fee 0 \
        --tx-in $(${CLICMD} shelley genesis initial-txin \
                    --testnet-magic ${MAGIC} \
                    --verification-key-file ${GENESISDIR}/utxo-keys/utxo${N}.vkey) \
        --tx-out $(cat ${GENESISDIR}/addresses/user${N}.addr)+${STAKE} \
        --certificate-file ${GENESISDIR}/addresses/pool-owner${N}-stake.reg.cert \
        --certificate-file ${GENESISDIR}/node${N}/registration.cert \
        --certificate-file ${GENESISDIR}/addresses/user${N}-stake.reg.cert \
        --certificate-file ${GENESISDIR}/addresses/user${N}-stake.deleg.cert \
        --out-file ${GENESISDIR}/node${N}/tx-delegate${N}.txbody

### So we'll need to sign this with a bunch of keys:
### 1. the initial utxo spending key, for the funds
### 2. the user1 stake address key, due to the delegatation cert
### 3. the pool1 owner key, due to the pool registration cert
### 3. the pool1 operator key, due to the pool registration cert

    ${CLICMD} shelley transaction sign \
        --signing-key-file ${GENESISDIR}/utxo-keys/utxo${N}.skey \
        --signing-key-file ${GENESISDIR}/addresses/user${N}-stake.skey \
        --signing-key-file ${GENESISDIR}/node${N}/owner.skey \
        --signing-key-file ${GENESISDIR}/node${N}/cold/operator.skey \
        --testnet-magic ${MAGIC} \
        --tx-body-file  ${GENESISDIR}/node${N}/tx-delegate${N}.txbody \
        --out-file      ${GENESISDIR}/node${N}/tx-delegate${N}.tx

done
echo

## submit the delegation transactions

for N in ${USER_POOL_N}; do
    CARDANO_NODE_SOCKET_PATH=logs/sockets/${N} \
    ${CLICMD} shelley transaction submit \
                --tx-file ${GENESISDIR}/node${N}/tx-delegate${N}.tx \
                --testnet-magic ${MAGIC}"
done
