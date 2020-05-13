#!/usr/bin/env bash
# shellcheck disable=SC1090

BASEDIR="$(realpath "$(dirname "$0")")"
. "$(realpath "${BASEDIR}"/../../scripts/common.sh)"

CONFIGDIR=${BASEDIR}/configuration
CONFIGFILE=${CONFIGDIR}/log-config-rt-view-service.yaml
STATICDIR=${BASEDIR}/../../cardano-rt-view/static/

run 'cardano-rt-view-service' \
  --config ${CONFIGFILE} \
  --static ${STATICDIR} \
  --port 8024
