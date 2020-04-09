#!/bin/sh

BASEDIR=$(realpath $(dirname $0))

CONFIGDIR=${BASEDIR}/configuration
CONFIGFILE=${CONFIGDIR}/log-config-rt-view-service.yaml
STATICDIR=${BASEDIR}/../../cardano-rt-view/static/

#RUNNER=${RUNNER:-cabal v2-run -v0}
#GENERATOR="${RUNNER} cardano-cli --"
RTVIEWSERVICE="${BASEDIR}/../../bin/cardano-rt-view-service"

exec ${RTVIEWSERVICE} \
  --config ${CONFIGFILE} \
  --static ${STATICDIR} \
  --port 8024
