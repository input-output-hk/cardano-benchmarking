
the 'analyse.sh' script is to be run in the extracted directory from a benchmark run.

it will:

1.) extract transaction ids with timestamp from the generator log
2.) extract timestamps and tx ids from blocks received on cardano-node on the explorer node

this gives us the "time-to-block" distribution over all transactions.


other scripts are included to extract specific information from log files like CPU usage, block forge times, etc.

