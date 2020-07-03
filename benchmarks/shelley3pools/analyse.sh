#!/usr/bin/env bash

# usage:
# indicate path to log files in environment variable: LOGPATH (default: logs)
# set number of nodes in benchmarking run in env. var.: NNODES (default: 3)
# indicate output directory for analysed data in env. var.: OUTDIR (default: timeline-${TSTAMP})
#
# then, call this script


BASEDIR=$(realpath $(dirname "$0"))
. ${BASEDIR}/../../scripts/common.sh

set -e

prebuild 'bmtimeline' || exit 1
prebuild 'bmtime2block' || exit 1
prebuild 'bmresources' || exit 1

TSTAMP=$(TZ=UTC date --iso-8601=seconds)

LOGPATH=${LOGPATH:-logs}
NNODES=${NNODES:- 3}
OUTDIR=${OUTDIR:-"timeline-${TSTAMP}"}

if [ -d "${OUTDIR}" ]; then
  echo "already exists output directory: ${OUTDIR}"
else
  mkdir -v ${OUTDIR}
fi

# stem of logfile names:
LOGFILESTEM="node"
for N in $(seq 0 $((NNODES - 1))); do
  echo "analysing logs of node ${N}"
  ../../scripts/nodeisleader.sh ${LOGPATH}/${LOGFILESTEM}$((N+1))-*.json | sed -e 's/^\(.*\)$/'${N}',\1/' - > ${OUTDIR}/leader-${N}.csv
  ../../scripts/addedtocurrentchain.sh ${LOGPATH}/${LOGFILESTEM}$((N+1))-*.json | sed -e 's/^\(.*\)$/'${N}',\1/' - > ${OUTDIR}/addtochain-${N}.csv
  ../../scripts/adoptedblock.sh ${LOGPATH}/${LOGFILESTEM}$((N+1))-*.json | sed -e 's/^\(.*\)$/'${N}',\1/' - > ${OUTDIR}/adopted-${N}.csv
  # collect all adopted events
  cat ${OUTDIR}/adopted-${N}.csv >> ${OUTDIR}/adopted.csv
  # detect forks
  ../../scripts/tryswitchtoafork.sh ${N} ${LOGPATH}/${LOGFILESTEM}$((N+1))-*.json >> ${OUTDIR}/tryswitchtoafork.csv
  ../../scripts/switchedtoafork.sh ${N} ${LOGPATH}/${LOGFILESTEM}$((N+1))-*.json >> ${OUTDIR}/switchedtoafork.csv
  # transactions adopted in blocks
  ../../scripts/extract_adopted_tx_ids.sh ${N} ${LOGPATH}/${LOGFILESTEM}$((N+1))-*.json >> ${OUTDIR}/txadopted.csv
  # transactions added to mempool
  ../../scripts/mempooladdedtx.sh ${N} ${LOGPATH}/${LOGFILESTEM}$((N+1))-*.json >> ${OUTDIR}/txmempool.csv
  # transactions rejected
  ../../scripts/mempoolrejectedtx.sh ${N} ${LOGPATH}/${LOGFILESTEM}$((N+1))-*.json >> ${OUTDIR}/txrejected.csv
  # memory usage, CPU usage
  ../../scripts/grep-cpu.sh ${LOGPATH}/${LOGFILESTEM}$((N+1))-*.json > ${OUTDIR}/cpu-${N}.csv
  ../../scripts/grep-mem.sh ${LOGPATH}/${LOGFILESTEM}$((N+1))-*.json > ${OUTDIR}/mem-${N}.csv
  # extract mempool size in number of txs and bytes
  ../../scripts/outcomeforge.sh ${N} ${LOGPATH}/${LOGFILESTEM}$((N+1))-*.json >> ${OUTDIR}/outcome.csv
  # check for leadership, outputs size of UTxO set
  ../../scripts/leadershipcheck.sh ${N} ${LOGPATH}/${LOGFILESTEM}$((N+1))-*.json >> ${OUTDIR}/utxosize.csv
  # parameters in genesis.json
  GENESIS=$(find ${LOGPATH}/.. -name "genesis.json" | head -1)
  if [ -e $GENESIS ]; then
    jq '[ "activeSlotsCoeff", .activeSlotsCoeff, "slotLength", .slotLength, "securityParam", .securityParam, "epochLength", .epochLength, "decentralisationParam", .protocolParams.decentralisationParam ] | @csv' --raw-output ${GENESIS} > ${OUTDIR}/genesis_parameters.csv
  fi
done

run bmtimeline 'stub' ${NNODES} ${OUTDIR} | tee -a ${OUTDIR}/timeline.txt
cp timeline.csv ${OUTDIR}/

run bmtime2block ${OUTDIR}
cp time2block.csv ${OUTDIR}/

run bmresources ${NNODES} ${OUTDIR}
cp resources.csv ${OUTDIR}/

if test -n "$(command -v libreoffice)" &&
   test -n "$(command -v ssconvert)"
then ./report.sh "${OUTDIR}"
fi
