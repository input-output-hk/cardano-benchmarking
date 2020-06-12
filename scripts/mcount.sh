#!/usr/bin/env bash
 
SED=sed
ARCH=$(uname)
if [ $ARCH == "Darwin" ]; then
  # check for GNU sed
  T=$(which gsed && echo 1 || echo 0)
  if [ "${T}" == "0" ]; then
    echo "On Darwin we need GNU's version of sed"
    echo "can be installed via 'brew install gnu-sed'"
    exit 1
  else
    SED=gsed
  fi
fi

$SED -ne 's/.*"kind":\(".*"\),"namespace":\(".*"\).*/\2 \1/p' - |
  while true; do

    read nmsp kind

    if [ -z "$kind" ]; then break; fi

    echo -n "$nmsp $kind "

    if [ "${kind}" = '"no .data.kind"' ]; then
      grep "\"ns\":.*${nmsp}" $* | wc -l
    else
      grep "\"ns\":.*${nmsp}.*\"kind\":${kind}" $* | wc -l
    fi
  done

