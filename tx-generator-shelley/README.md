# Shelley Transaction Generator

## CLI arguments:
```
cardano-tx-generator-shelley - a transaction generator Shelley.

Usage: tx-generator-shelley (--mainnet | --testnet-magic NATURAL)
                            --config FILEPATH --socket-path FILEPATH
                            --target-node (HOST,PORT) --num-of-txs INT
                            --tx-fee INT --tps FLOAT [--init-cooldown INT] 
                            [--add-tx-size INT] [--submit-to-api URL]
                            --fund-value LOVELACE --fund-skey FILE
                            --fund-utxo ARG --fund-addr ARG

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
  --fund-skey FILE         signingkey for spending the initial fund
  --fund-utxo ARG          utxo of the initial fund
  --fund-addr ARG          address uses for transactions
  -h,--help                Show this help text
```

### TODO

* `--add-tx-size INT` is not implemented.
* `--num-of-txs INT` is per target node (not the total number).

## Example

```
tx-generator-shelley\
  --testnet-magic 42\
  --config /work/shelley3pools/node-config.json\
  --socket-path /work/shelley3pools/logs/sockets/1\
  --num-of-txs 6000\
  --tx-fee 10000\
  --tps 0.7\
  --target-node '("127.0.0.1",3000)'\
  --target-node '("127.0.0.1",3001)'\
  --target-node '("127.0.0.1",3002)'\
  --fund-utxo '355fae08a93b5920eca47ffb60ba13401d3d579cccfed1b69b766941452eb715#0'\
  --fund-value 33333333334\
  --fund-skey /work/shelley3pools/tmp-exgenesis/payer.skey\
  --fund-addr 60734487ec861e69fa5509866a10d9eec7fba99051c366ce5e490cc2d298ad7579
```
