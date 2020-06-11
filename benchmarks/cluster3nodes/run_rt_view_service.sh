#!/usr/bin/env bash
# shellcheck disable=SC1090

BASEDIR="$(realpath "$(dirname "$0")")"
. "$(realpath "${BASEDIR}"/../../scripts/common.sh)"

CONFIGDIR=${BASEDIR}/configuration-$era
CONFIGFILE=${CONFIGDIR}/configuration-rt-view.yaml
STATICDIR=${BASEDIR}/../../cardano-rt-view/static/

RTviewPort=12799

run 'cardano-rt-view-service' \
  --config ${CONFIGFILE} \
  --static ${STATICDIR} \
  --port   ${RTviewPort}
