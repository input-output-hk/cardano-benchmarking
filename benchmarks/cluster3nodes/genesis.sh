#!/usr/bin/env bash
# shellcheck disable=SC1090

set -e

BASEDIR="$(realpath "$(dirname "$0")")"
. "$(realpath "${BASEDIR}"/../../scripts/common.sh)"

CONFIGDIR="${BASEDIR}/configuration"

umask 077
start_time=$(date '+%s')
protocol_params="${CONFIGDIR}/protocol-params.json"

parameter_k=2160
protocol_magic=459045235
n_poors=128
n_delegates=3
total_balance=8000000000000000
delegate_share=0.9
avvm_entries=128
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

run 'cardano-cli' genesis "${args[@]}" "$@"

# move new genesis to configuration
GENHASH=$(run 'cardano-cli' \
              print-genesis-hash \
              --genesis-json "${tmpdir}/genesis.json" |
                  tail -1)
TARGETDIR="${CONFIGDIR}/${GENHASH:0:5}"
set -x
mkdir -p "${TARGETDIR}"
cp -ia ${tmpdir}/genesis.json ${TARGETDIR}/
cp -ia ${tmpdir}/delegate-keys.*.key ${TARGETDIR}/
cp -ia ${tmpdir}/delegation-cert.*.json ${TARGETDIR}/
echo $GENHASH > ${TARGETDIR}/GENHASH

if [ -d ${CONFIGDIR}/genesis ]; then
  mv ${CONFIGDIR}/genesis "${CONFIGDIR}/$(cut -c-5 ${CONFIGDIR}/genesis/GENHASH)"
fi
mv ${CONFIGDIR}/${GENHASH:0:5} ${CONFIGDIR}/genesis

echo "genesis created with hash = $(cat "${CONFIGDIR}/genesis/GENHASH")"
echo "  in directory ${CONFIGDIR}/genesis"
