#!/usr/bin/env bash

BASEDIR="$(realpath "$(dirname "$0")")"

era='byron'

CONFIGDIR=${BASEDIR}/configuration-$era

. "${CONFIGDIR}"/psql-settings.sh

components=($(ls "${BASEDIR}"/../../scripts/*.sql))

psql -q ${components[*]/#/--file }

