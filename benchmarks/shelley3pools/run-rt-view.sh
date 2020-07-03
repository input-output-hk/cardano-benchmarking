#!/usr/bin/env bash

if [ -z "${WORKINGDIRECTORY}" ]; then
  echo "missing \$WORKINGDIRECTORY."
  exit 1
fi

BASEDIR=$(realpath $(dirname "$0"))
. ${BASEDIR}/../../scripts/common.sh
. ${BASEDIR}/configuration/parameters

RTVIEWCMD=${RTVIEWCMD:-"run cardano-rt-view-service"}
#"stack exec cardano-rt-view-service -- "

RTviewPort=12799
RTviewConfig=${WORKINGDIRECTORY}/configuration-rt-view.yaml

Sockets=logs/sockets
if [ ! -d $Sockets ]; then
  mkdir -p $Sockets
else
  rm $Sockets/*
fi

$RTVIEWCMD --config ${RTviewConfig} --static static --port ${RTviewPort}

