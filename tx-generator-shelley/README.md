# Shelley Transaction Generator

## CLI arguments:
```
cardano-tx-generator-shelley - a transaction generator Shelley.

Usage: tx-generator-shelley (--mainnet | --testnet-magic NATURAL)
                            --config FILEPATH --socket-path FILEPATH
                            --target-node (HOST,PORT) --num-of-txs INT
                            --tx-fee INT --tps FLOAT [--init-cooldown INT] 
                            [--add-tx-size INT] [--submit-to-api URL]
                            --fund-value LOVELACE --signing-key-file FILE
                            --tx-in TX-IN --fund-addr FILE

Available options:
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Specify a testnet magic id.
  --config FILEPATH        a cardano-node config file (for logging config)
  --socket-path FILEPATH   Path to a cardano-node socket
  --target-node (HOST,PORT)
                           host and port of the node transactions will be sent
                           to.
  --num-of-txs INT         Number of transactions generator will create.
  --tx-fee INT             Fee per transaction, in Lovelaces.
  --tps FLOAT              TPS (transaction per second) rate.
  --init-cooldown INT      Delay between init and main submission phases.
  --add-tx-size INT        Additional size of transaction, in bytes.
  --submit-to-api URL      Explorer's API endpoint to submit transaction.
  --fund-value LOVELACE    Lovelace value of the initial fund
  --signing-key-file FILE  Input filepath of the signing key
  --tx-in TX-IN            The input transaction as TxId#TxIx where TxId is the
                           transaction hash and TxIx is the index.
  --fund-addr FILE         address used for transactions
  -h,--help                Show this help text
```

