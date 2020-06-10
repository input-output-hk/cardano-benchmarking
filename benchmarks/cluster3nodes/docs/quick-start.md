# Quick start

this summarizes somes of the [Pioneer's testnet docs](https://github.com/input-output-hk/cardano-tutorials/tree/master/pioneers-testnet)

CLICMD="stack --nix exec cardano-cli --"
NODECMD="stack --nix exec cardano-node --"


## start a passive node which follow's the node

${NODECMD} run --topology ff-topology.json --database-path db --socket-path logs/socket --host-addr 127.0.0.1 --port 3001 --config ff-config.json


## create payment key pair

${CLICMD} shelley address key-gen \
     --verification-key-file payment.vkey \
     --signing-key-file payment.skey


## create stake key pair

${CLICMD} shelley stake-address key-gen \
 --verification-key-file stake.vkey \ 
 --signing-key-file stake.skey


## generate payment address

${CLICMD} shelley address build \
     --payment-verification-key-file payment.vkey > addr.payment


## query an address

export CARDANO_NODE_SOCKET_PATH=`pwd`/logs/socket

${CLICMD} shelley query utxo \
     --address $(cat addr.payment) \
     --testnet-magic 42


## generate stake address

${CLICMD} shelley stake-address build \
     --staking-verification-key-file stake.vkey > addr.stake



## create registration certificate

${CLICMD} shelley stake-address registration-certificate \
     --staking-verification-key-file stake.vkey \
     --out-file stake.cert


## prepare transaction: calculate minimum fees

${CLICMD} shelley transaction calculate-min-fee \
     --tx-in-count 1 \
     --tx-out-count 1 \
     --ttl 200000 \
     --testnet-magic 42 \
     --signing-key-file payment.skey \
     --signing-key-file stake.skey \
     --certificate stake.cert \
     --protocol-params-file protocol.json

