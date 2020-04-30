#!/usr/bin/env bash

. configuration/psql-settings.sh "$1"

DROPS=(
  "drop domain lovelace cascade;"
  "drop domain hash28type cascade;"
  "drop domain hash32type cascade;"
  "drop domain outsum cascade;"
  "drop domain txindex cascade;"
  "drop domain uinteger cascade;"

  "drop table tx cascade;"
  "drop table tx_in cascade;"
  "drop table tx_out cascade;"
  "drop table block cascade;"
  "drop table slot_leader cascade;"
  "drop table schema_version cascade;"
  "drop table meta cascade;"
  "drop table epoch cascade;"
)

psql --command "${DROPS[*]}" "" || echo "DB missing"
