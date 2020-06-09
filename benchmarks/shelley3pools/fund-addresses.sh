TXIN=$1
SIGNING_KEY=$2
NUM_OF_ADDRESSES=$3
AMOUNT=$4
WORKDIR=$5
MAGIC=42
TTL=5000
FEE=100000

rm -fr $WORKDIR/txs
mkdir $WORKDIR/txs

rm $WORKDIR/protocol.json
cardano-cli shelley query protocol-parameters \
    --testnet-magic $MAGIC \
    --out-file $WORKDIR/protocol.json

$CLICMD shelley address key-gen \
    --verification-key-file $WORKDIR/fund.vkey \
    --signing-key-file $WORKDIR/fund.skey

# $CLICMD shelley address build \
#     --testnet-magic $MAGIC \
#     --payment-verification-key-file  $WORKDIR/fund.vkey > $WORKDIR/fund.addr
$CLICMD shelley address build \
    --verification-key-file  $WORKDIR/fund.vkey > $WORKDIR/fund.addr

$CLICMD shelley transaction build-raw \
    --tx-in 6abc9cded89953747e8f22609917c3170008dbbca1b97cdf4c5c05bb454c4fd1#0 \
    --tx-out $(cat $WORKDIR/fund.addr)+333333333 \
    --ttl $TTL \
    --fee 0 \
    --tx-body-file $WORKDIR/genesis_tx.txbody

$CLICMD shelley transaction sign \
  --tx-body-file $WORKDIR/genesis_tx.txbody \
  --signing-key-file $WORKDIR/configuration/genesis/utxo-keys/utxo1.skey \
  --testnet-magic $MAGIC \
  --tx-file $WORKDIR/genesis_tx.tx

$CLICMD shelley transaction submit \
    --tx-file $WORKDIR/genesis_tx.tx \
    --testnet-magic 42

for i in $(seq 1 $NUM_OF_ADDRESSES)
do
    # Get TX-Specific Values
    # TTL=$(cardano-cli shelley query tip --testnet-magic $MAGIC) + 250

    # FEE=$(cardano-cli shelley transaction calculate-min-fee \
    #         --tx-in-count 1 \
    #         --tx-out-count 2 \
    #         --ttl $TTL \
    #         --testnet-magic $MAGIC \
    #         --signing-key-file $SIGNING_KEY \
    #         --protocol-params-file $WORKDIR/protocol.json)
    

    # Build Transaction
    $CLICMD shelley transaction build-raw \
        --tx-in  ${TXIN} \
        --tx-out ${TXOUT}+${AMOUNT} \
        --tx-out $(cat payment.addr)+499398236348
        --ttl $TTL \
        --fee 0 \
        --tx-body-file $WORKDIR/txs/tx_$i.txbody

    # Sign Transaction
    $CLICMD shelley transaction sign \
    --tx-body-file $WORKDIR/txs/tx_$i.txbody \
    --signing-key-file $WORKDIR/utxo-keys/utxo${NUMUTXO}.skey \
    --testnet-magic ${MAGIC} \
    --tx-file $WORKDIR/txs/tx_$i.tx

    # Submit Transaction
    $CLICMD shelley transaction submit \
        --tx-file $WORKDIR/txs/tx_$i.tx \
        --testnet-magic $MAGIC
done
