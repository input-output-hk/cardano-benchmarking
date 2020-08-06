#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2016

BASEDIR=$(realpath "$(dirname "$0")")
. "$BASEDIR"/../../scripts/common.sh

key=$1
faucet=${2:-faucet.mainnet-candidate-4.dev.cardano.org}
api_key=${3:-xPzFbsQRWIbB8O6BWkSaG04Ow9M5qWGf}

cli_args=(
        shelley address build
        --payment-verification-key-file "$key"
        --mainnet
)
addr=$(cardano-cli "${cli_args[@]}")
test -n "$addr" ||
        fail "couldn't get address for key file:  $key"

curl -XPOST "https://$faucet/send-money/$addr?apiKey=$api_key"
