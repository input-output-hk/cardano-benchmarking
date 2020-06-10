# Initial set-up

rm -fr tmp
mkdir tmp
mkdir tmp/txs

# Fund address from Genesis
## Get the initial UTxO TxIn

cardano-cli shelley genesis initial-txin \
    --verification-key-file configuration/genesis/utxo-keys/utxo1.vkey > tmp/genesis_utxo

## Set-up the Payer
### Create the keys and addresses for Payer
cardano-cli shelley address key-gen \
    --verification-key-file tmp/payer.vkey \
    --signing-key-file tmp/payer.skey

cardano-cli shelley address build \
    --payment-verification-key-file tmp/payer.vkey > tmp/payer.addr

### Build, Sign, Submit a Genesis UTxO to the Payer
cardano-cli shelley transaction build-raw \
    --tx-in  `cat tmp/genesis_utxo`#0 \
    --tx-out `cat tmp/payer.addr`+333333333 \
    --ttl 10000 \
    --fee 0 \
    --tx-body-file tmp/txs/genesis_to_funding.txbody

cardano-cli shelley transaction sign \
    --tx-body-file tmp/txs/genesis_to_funding.txbody \
    --signing-key-file configuration/genesis/utxo-keys/utxo1.skey \
    --testnet-magic 42 \
    --tx-file tmp/txs/genesis_to_funding.tx

cardano-cli shelley transaction submit \
    --tx-file tmp/txs/genesis_to_funding.tx \
    --testnet-magic 42


# Create n target addresses
./create-addresses "100" "/tmp"

for i in $(seq 1 100)
do
    previous_tx_id = `cat tmp/txs/tx_$(expr $i - 1).id`

    # Build n transactions
    cardano-cli shelley transaction build-raw \
        --tx-in previous_tx_id#0 \
        --tx-out `cat tmp/addresses/address_$1`+3000000 \
        --tx-out `cat tmp/payer.addr`+499398236348 \
        --ttl 10000 \
        --fee 300000 \
        --out-file tmp/txs/tx_$i.raw
    
    # Get the TxID of the above tx
    cardano-cli shelley transaction txid --tx-body-file tmp/txs/tx_$i.raw > tmp/txs/tx_$i.id:

    # Sign n transactions
    cardano-cli shelley transaction sign \
        --tx-body-file tmp/txs/tx_$i.raw \
        --signing-key-file tmp/payer.skey \
        --testnet-magic 42 \
        --out-file tmp/txs/tx_$i.signed
    
    # Submit n transactions
    cardano-cli shelley transaction submit \
        --tx-file tmp/txs/tx_$i.signed \
        --testnet-magic 42
done

