CLICMD="cabal --enable-nix exec cardano-cli --"


# the index of the genesis UTxO to transfer
NUMUTXO=1

# path to the node's socket for submitting the transaction
CARDANO_NODE_SOCKET_PATH=logs/sockets/1

# identifier for this testnet genesis
MAGIC=42

# address of genesis funds

TXIN=$($CLICMD shelley genesis initial-txin \
    --testnet-magic ${MAGIC} \
    --verification-key-file $GENESISDIR/utxo-keys/utxo${NUMUTXO}.vkey)

===>>> $TXIN


# create new address that receives these funds

$CLICMD shelley address key-gen \
    --verification-key-file $GENESISDIR/addr1.vkey \
    --signing-key-file $GENESISDIR/addr1.skey


TXOUT=$($CLICMD shelley address build \
    --testnet-magic ${MAGIC} \
    --payment-verification-key-file  $GENESISDIR/addr1.vkey)

===>>> $TXOUT


# prepare raw transaction 

AMOUNT=3906250   # all available in this genesis UTxO

$CLICMD shelley transaction build-raw \
    --tx-in  ${TXIN} \
    --tx-out ${TXOUT}+${AMOUNT} \
    --ttl 3600 \
    --fee 0 \
    --tx-body-file $GENESISDIR/tx0.txbody


# sign transaction

$CLICMD shelley transaction sign \
  --tx-body-file $GENESISDIR/tx0.txbody \
  --signing-key-file $GENESISDIR/utxo-keys/utxo${NUMUTXO}.skey \
  --testnet-magic ${MAGIC} \
  --tx-file $GENESISDIR/tx0.tx


# submit transaction

CARDANO_NODE_SOCKET_PATH=logs/sockets/1 \
    $CLICMD shelley transaction submit \
        --tx-file $GENESISDIR/tx0.tx \
        --testnet-magic ${MAGIC}


# check UTxO

CARDANO_NODE_SOCKET_PATH=logs/sockets/1 \
    $CLICMD shelley query utxo \
      --testnet-magic ${MAGIC} \
      --address <addr from initialFunds in genesis>

CARDANO_NODE_SOCKET_PATH=logs/sockets/1 \
    $CLICMD shelley query utxo \
      --testnet-magic ${MAGIC} \
      --address ${TXOUT}

