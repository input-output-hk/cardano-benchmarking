set -x

NUM_OF_GENERATORS=$1
SOCKET_ID=1

for i in $(seq 1 ${NUM_OF_GENERATORS})
do
    let "utxo_id = ${i} + 2"
    ./fund-addresses-generic.sh ${i} ${utxo_id} ${SOCKET_ID} &
done
wait