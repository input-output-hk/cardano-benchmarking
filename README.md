[![Build status](https://badge.buildkite.com/19c55c10f4ea983dd84ec31abbd39c85f2396cb65364dd180b.svg)](https://buildkite.com/input-output-hk/cardano-benchmarking?branch=master)

# cardano-benchmarking

This repository contains important tool for `cardano-node` benchmarking - [cardano-tx-generator](https://github.com/input-output-hk/cardano-benchmarking/tree/master/cardano-tx-generator). This is a generator of transactions, both for Byron and Shelley. For more details please [see the project](https://github.com/input-output-hk/cardano-benchmarking/tree/master/cardano-tx-generator).

## Preparation

After first checkout of the repository, update the submodules:

```
$ git submodule update --init
```

## Building on Linux / macOS

Please see our [wiki page](https://github.com/input-output-hk/cardano-benchmarking/wiki/BuildingOnUnix).

## Building on Windows

Install _MinGW_ (64 bit) ![MinGW](https://raw.githubusercontent.com/wiki/input-output-hk/cardano-benchmarking/mingw64.png) environment and run `stack` in it to compile _cardano-benchmarking_ and all its dependencies.
(see our [wiki page](https://github.com/input-output-hk/cardano-benchmarking/wiki/BuildingOnWindows))
