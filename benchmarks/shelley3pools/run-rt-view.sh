#!/bin/sh

RTVIEWCMD="stack exec cardano-rt-view-service -- "

RTviewPort=12799
RTviewConfig=configuration/configuration-rt-view.yaml

Sockets=logs/sockets
if [ ! -d $Sockets ]; then
  mkdir -p $Sockets
else
  rm $Sockets/*
fi

$RTVIEWCMD --config ${RTviewConfig} --static static --port ${RTviewPort}

