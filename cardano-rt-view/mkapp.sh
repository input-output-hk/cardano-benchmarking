#!/usr/bin/env bash

set -e

BASEPATH=$(realpath $(dirname $0))

PROGNAME=cardano-rt-view-service
EXE=${BASEPATH}/../bin/${PROGNAME}

if [[ $(uname -a) == *"NixOS"* ]]; then
  # We're on NixOS, so appimage-run program need to run AppImage,
  # please make sure you installed it (nix-env -iA nixos.appimage-run).
  APPIMGRUN="appimage-run"
else
  APPIMGRUN=
fi

# if FUSE is not an option, we can still extract the AppImage and run it directly
# appimagetool-x86_64.AppImage --appimage-extract && mv squashfs-root 
if [ -d appimagetool-x86_64.squashfs ]; then
  RUNAPPIMG="./appimagetool-x86_64.squashfs/AppRun"
else
  RUNAPPIMG="${APPIMGRUN} ${HOME}/Downloads/appimagetool-x86_64.AppImage"
fi

TIMESTAMP=$(TZ=UTC date --iso-8601=seconds | sed -e 's/:/_/g;s/+.*$//')
OUTDIR="appdir-${TIMESTAMP}"
mkdir ${OUTDIR}

cd ${OUTDIR}

# copy libraries to local lib folder
if [ -d lib ]; then
  rm -ivr lib
fi
mkdir lib

LIBS=$(ldd ${EXE}  | sed -ne 's/.* => \([^ ]\+\) .*/\1/p')

for L in $LIBS; do
  cp -iv ${L} ./lib/
done

# hack hack
export LD_LIBRARY_PATH="$(pwd)/lib":$LD_LIBRARY_PATH

# copy binary and change library search path
cp -iv ${EXE} .

patchelf --remove-rpath ${PROGNAME}
patchelf --set-rpath lib/ ${PROGNAME}

# copy resources
cp ../resources/cardano-rt-view.desktop .
cp ../resources/cardano-rt-view.png .
cp ../resources/rt-view.yaml0 .

cp -rv ../static .

cp ../resources/AppRun .
chmod 700 AppRun

# create appimage
cd ..

ARCH=x86_64 ${RUNAPPIMG}  -v  `pwd`/${OUTDIR}/ /tmp/Cardano_RTview-x86_64.AppImage
