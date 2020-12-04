#!/usr/bin/env bash

BASEDIR=$(realpath $(dirname "$0"))
. ${BASEDIR}/../../scripts/common.sh
. ${BASEDIR}/configuration/parameters

RTVIEW_EXE=cardano-rt-view
RTVIEW_ACHIVE=$RTVIEW_EXE.tar.gz
RTVIEW_DIR=/tmp/rt-view

RTVIEW_WEB_PORT=12799
RTVIEW_CONFIG=$BASEDIR/configuration/configuration-rt-view.yaml

PIPES_DIR=logs/sockets
if [ ! -d $PIPES_DIR ]; then
  mkdir -p $PIPES_DIR
else
  rm $PIPES_DIR/*
fi

# $RTVIEWCMD --config ${RTviewConfig} --static static --port ${RTviewPort}

rm -rf $RTVIEW_DIR
mkdir $RTVIEW_DIR
cd $RTVIEW_DIR

wget -O $RTVIEW_ACHIVE https://hydra.iohk.io/build/4447146/download/1/cardano-rt-view-0.2.0-linux-x86_64.tar.gz
tar -xf $RTVIEW_ACHIVE
cd $BASEDIR
/$RTVIEW_DIR/$RTVIEW_EXE --config $RTVIEW_CONFIG --static /$RTVIEW_DIR/static --port $RTVIEW_WEB_PORT
