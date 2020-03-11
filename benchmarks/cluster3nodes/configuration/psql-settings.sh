#!/bin/sh

if [ -z "${BASEDIR}" -o ! -d "${BASEDIR}" ]; then
  echo "missing \$BASEDIR"
  exit 1
fi

export PGDATABASE=cl3demo
export PGUSER=cl3demo
export PGHOST=localhost
export PGPORT=5432
export PGPASSFILE=${BASEDIR}/configuration/pgpass

