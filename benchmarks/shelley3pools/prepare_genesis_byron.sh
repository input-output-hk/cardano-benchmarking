#!/usr/bin/env bash
# shellcheck disable=SC1090

set -e

basedir=$(realpath "$(dirname "$0")")
. "$basedir"/../../scripts/common.sh
. "$basedir"/configuration/parameters

cd "$basedir"

gendir=$GENESISDIR_byron
cli=${CLICMD:-'run cardano-cli'}

umask 077
protocol_params="$basedir/configuration/byron-protocol-params.json"

protocol_magic=$MAGIC
total_balance=8000000000000002

parameter_k=10
n_poors=1
n_delegates=3
delegate_share=0.5
avvm_entries=0
avvm_entry_balance=10000000000000
not_so_secret=2718281828

tmpdir="`mktemp`.d"
args=(
      --genesis-output-dir           "${tmpdir}"
      --start-time                   "${start_time}"
      --protocol-parameters-file     "${protocol_params}"
      --k                            ${parameter_k}
      --protocol-magic               ${protocol_magic}
      --n-poor-addresses             ${n_poors}
      --n-delegate-addresses         ${n_delegates}
      --total-balance                ${total_balance}
      --delegate-share               ${delegate_share}
      --avvm-entry-count             ${avvm_entries}
      --avvm-entry-balance           ${avvm_entry_balance}
      --real-pbft
      --secret-seed                  ${not_so_secret}
)

$cli byron genesis genesis "${args[@]}" "$@"

rm -rf "$gendir"
mkdir -p "$gendir"
cp -ia ${tmpdir}/genesis.json           $gendir/
cp -ia ${tmpdir}/delegate-keys.*.key    $gendir/
cp -ia ${tmpdir}/delegation-cert.*.json $gendir/
cp -ia ${tmpdir}/genesis-keys.*.key     $gendir/
cp -ia ${tmpdir}/poor-keys.*.key        $gendir/

$cli shelley key convert-byron-key \
     --byron-payment-key-type \
     --byron-signing-key-file           $gendir/poor-keys.000.key \
     --out-file                         $gendir/poor-keys.000.skey

$cli print-genesis-hash --genesis-json "$gendir"/genesis.json |
        tail -1                       > $gendir/GENHASH
