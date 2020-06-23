#!/usr/bin/env bash

. ./configuration/parameters

NNODES=${NNODES:-3}

for i in $(seq 1 ${NNODES}) 
do
    cp ./configuration/configuration-template.yaml ./configuration/configuration-node-${i}.yaml
    sed -i "s/<CHANGE_ME_TO_NODE_ID>/${i}/g" ./configuration/configuration-node-${i}.yaml
done
