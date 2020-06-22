#!/usr/bin/env bash

BASEDIR=$(realpath $(dirname "$0"))
. ${BASEDIR}/../../scripts/common.sh

set -e

prebuild 'bm-timeline' || exit 1

TSTAMP=$(TZ=UTC date --iso-8601=seconds)

LOGPATH=${LOGPATH:-logs}
NNODES=${NNODES:- 3}
OUTDIR=${OUTDIR:-"timeline-${TSTAMP}"}
if [ -d "${OUTDIR}" ]; then
  echo "already exists output directory: ${OUTDIR}"
else
  mkdir -v ${OUTDIR}
fi

for N in $(seq 0 $((NNODES - 1))); do
  echo "analysing logs of node ${N}"
  ../../scripts/nodeisleader.sh ${LOGPATH}/node$((N+1))-*.json | sed -e 's/^\(.*\)$/'${N}',\1/' - > ${OUTDIR}/leader-${N}.csv
  ../../scripts/addedtocurrentchain.sh ${LOGPATH}/node$((N+1))-*.json | sed -e 's/^\(.*\)$/'${N}',\1/' - > ${OUTDIR}/addtochain-${N}.csv
  ../../scripts/adoptedblock.sh ${LOGPATH}/node$((N+1))-*.json | sed -e 's/^\(.*\)$/'${N}',\1/' - > ${OUTDIR}/adopted-${N}.csv
  # collect all adopted events
  cat ${OUTDIR}/adopted-${N}.csv >> ${OUTDIR}/adopted.csv
  # detect forks
  ../../scripts/tryswitchtoafork.sh ${N} ${LOGPATH}/node$((N+1))-*.json >> ${OUTDIR}/tryswitchtoafork.csv
  ../../scripts/switchedtoafork.sh ${N} ${LOGPATH}/node$((N+1))-*.json >> ${OUTDIR}/switchedtoafork.csv
  # transactions adopted in blocks
  ../../scripts/extract_adopted_tx_ids.sh ${N} ${LOGPATH}/node$((N+1))-*.json >> ${OUTDIR}/txadopted.csv
  # transactions added to mempool
  ../../scripts/mempooladdedtx.sh ${N} ${LOGPATH}/node$((N+1))-*.json >> ${OUTDIR}/txmempool.csv
  # transactions rejected
  ../../scripts/mempoolrejectedtx.sh ${N} ${LOGPATH}/node$((N+1))-*.json >> ${OUTDIR}/txrejected.csv
  # memory usage, CPU usage
  ../../scripts/grep-cpu.sh ${LOGPATH}/node$((N+1))-*.json > ${OUTDIR}/cpu-${N}.csv
  ../../scripts/grep-mem.sh ${LOGPATH}/node$((N+1))-*.json > ${OUTDIR}/mem-${N}.csv
  # parameters in genesis.json
  GENESIS=$(find ${LOGPATH}/.. -name "genesis.json" | head -1)
  if [ -e $GENESIS ]; then
    jq '[ "activeSlotsCoeff", .activeSlotsCoeff, "slotLength", .slotLength, "securityParam", .securityParam, "epochLength", .epochLength, "decentralisationParam", .protocolParams.decentralisationParam ] | @csv' ${GENESIS} > ${OUTDIR}/genesis_parameters.csv
  fi
done

#stack --nix run reconstruct-timeline -- ${NNODES} ${OUTDIR} | tee -a ${OUTDIR}/timeline.txt
run reconstruct-timeline ${NNODES} ${OUTDIR} | tee -a ${OUTDIR}/timeline.txt
cp timeline.csv ${OUTDIR}/

