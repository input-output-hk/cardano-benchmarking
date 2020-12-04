NUM_OF_ADDRESSES=$1
WORKDIR=$2

rm -fr $WORKDIR/addresses
mkdir $WORKDIR/addresses
for i in $(seq 1 $NUM_OF_ADDRESSES)
do
    cardano-cli address key-gen \
        --verification-key-file $WORKDIR/addresses/payment_$i.vkey \
        --signing-key-file $WORKDIR/addresses/payment_$i.skey

    cardano-cli address build \
        --verification-key-file $WORKDIR/addresses/payment_$i.vkey > $WORKDIR/addresses/address_$i
done

