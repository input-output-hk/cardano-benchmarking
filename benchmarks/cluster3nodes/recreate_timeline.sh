#!/bin/sh

BASEPATH=$(realpath $(dirname $0))
SCRIPTDIR=${BASEPATH}/../../scripts

ISLEADER=${SCRIPTDIR}/nodeisleader.sh
if [ ! -x ${ISLEADER} ]; then
  echo "missing script: ${ISLEADER}"
  exit 1
fi

ADDEDTOCURRCHAIN=${SCRIPTDIR}/addedtocurrentchain.sh
if [ ! -x ${ADDEDTOCURRCHAIN} ]; then
  echo "missing script: ${ADDEDTOCURRCHAIN}"
  exit 1
fi

ADOPTEDBLOCK=${SCRIPTDIR}/adoptedblock.sh
if [ ! -x ${ADOPTEDBLOCK} ]; then
  echo "missing script: ${ADOPTEDBLOCK}"
  exit 1
fi

INPUTDIR=logs
OUTPUDIR=timeline
if [ ! -d ${OUTPUDIR} ]; then
  mkdir -v ${OUTPUDIR}
fi

NODES="0 1 2"
for NODE in ${NODES}; do
  echo "processing logs of node ${NODE}"
  ${ISLEADER} ${INPUTDIR}/node-${NODE}-*.json | sed -e 's/^\(.*\)$/'${NODE}',\1/' > ${OUTPUDIR}/leader-${NODE}.csv
  ${ADOPTEDBLOCK} ${INPUTDIR}/node-${NODE}-*.json | sed -e 's/^\(.*\)$/'${NODE}',\1/' > ${OUTPUDIR}/adopted-${NODE}.csv
  ${ADDEDTOCURRCHAIN} ${INPUTDIR}/node-${NODE}-*.json | sed -e 's/^\(.*\)$/'${NODE}',\1/' > ${OUTPUDIR}/addtochain-${NODE}.csv
done

