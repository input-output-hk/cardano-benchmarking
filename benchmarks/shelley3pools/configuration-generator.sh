#!/usr/bin/env bash

BASEDIR=$(realpath $(dirname "$0"))

if [ -z "${WORKINGDIRECTORY}" ]; then
  echo "missing \$WORKINGDIRECTORY."
  exit 1
fi

. ./configuration/parameters

if [ -z "$NNODES" ]; then
  echo "missing \$NNODES in configuration/parameters"; exit 1
fi

for i in $(seq 1 ${NNODES}); do
    cp ./configuration/configuration-template.yaml ${WORKINGDIRECTORY}/configuration-node-${i}.yaml
    sed -i "s/<CHANGE_ME_TO_NODE_ID>/${i}/g" ${WORKINGDIRECTORY}/configuration-node-${i}.yaml
    sed -i "s,^GenesisFile:.*$,GenesisFile: ${BASEDIR}/configuration/genesis/genesis.json," ${WORKINGDIRECTORY}/configuration-node-${i}.yaml
    GenesisFile: 
done

cp ./configuration/configuration-rt-view.yaml ${WORKINGDIRECTORY}/

for i in $(seq 1 ${NNODES}); do
  echo "  - nodeName: \"pool${i}\"" >> ${WORKINGDIRECTORY}/configuration-rt-view.yaml
  echo "    remoteAddr:" >> ${WORKINGDIRECTORY}/configuration-rt-view.yaml
  echo "      tag: RemotePipe" >> ${WORKINGDIRECTORY}/configuration-rt-view.yaml
  echo "      contents: \"logs/sockets/pool${i}\"" >> ${WORKINGDIRECTORY}/configuration-rt-view.yaml
done
