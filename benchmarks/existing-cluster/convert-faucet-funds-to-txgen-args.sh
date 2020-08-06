#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2016

BASEDIR=$(realpath "$(dirname "$0")")
. "$BASEDIR"/../../scripts/common.sh

key=$1
faucet_funds=$2

cli_args=(
        shelley address build
        --payment-verification-key-file "$key"
        --mainnet
)
addr=$(cardano-cli "${cli_args[@]}")
test -n "$addr" ||
        fail "couldn't get address for key file:  $key"

curl -XPOST "https://$faucet/send-money/$addr?apiKey=$api_key"
