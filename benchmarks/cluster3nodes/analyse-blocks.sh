#!/usr/bin/env bash

BASEDIR="$(realpath "$(dirname "$0")")"

. "${BASEDIR}"/configuration/psql-settings.sh

components=($(ls "${BASEDIR}"/../../scripts/*.sql))

psql -q ${components[*]/#/--file }

