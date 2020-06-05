CLICMD="stack --nix exec cardano-cli --"


# address of genesis funds

$CLICMD shelley genesis initial-txin \
    --verification-key-file $GENESISDIR/utxo-keys/utxo.0.vkey

===>>> $TXIN


# create new address that receives these funds

$CLICMD shelley address key-gen \
    --verification-key-file $GENESISDIR/addr1.vkey \
    --signing-key-file $GENESISDIR/addr1.skey


$CLICMD shelley address build \
    --payment-verification-key-file  $GENESISDIR/addr1.vkey

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
  --signing-key-file $GENESISDIR/utxo-keys/utxo.0.skey \
  --testnet-magic 4242 \
  --tx-file $GENESISDIR/tx0.tx


# submit transaction

CARDANO_NODE_SOCKET_PATH=logs/sockets/0 \
    $CLICMD shelley transaction submit \
        --tx-file $GENESISDIR/tx0.tx \
	--testnet-magic 4242


# check UTxO

CARDANO_NODE_SOCKET_PATH=logs/sockets/0 \
    $CLICMD shelley query utxo \
      --testnet-magic 4242 \
      --address <addr from initialFunds in genesis>

CARDANO_NODE_SOCKET_PATH=logs/sockets/0 \
    $CLICMD shelley query utxo \
      --testnet-magic 4242 \
      --address ${TXOUT}

