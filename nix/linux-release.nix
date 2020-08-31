############################################################################
# Linux release cardano-rt-view-service-*.AppImage
#
# This bundles up the Linux build and its dependencies (into AppImage),
# and sets up the Hydra build artifact.
#
############################################################################

{ pkgs
, project
, exes
}:

let
  lib = pkgs.lib;
  name = "cardano-rt-view-service-${project.version}-linux";

in pkgs.runCommand name {
    buildInputs = with pkgs.buildPackages; [
      haskellBuildUtils.package
    ];
  } ''
  RUNAPPIMG="appimage-run ${HOME}/Downloads/appimagetool-x86_64.AppImage"

  TIMESTAMP=$(TZ=UTC date --iso-8601=seconds | sed -e 's/:/_/g;s/+.*$//')
  OUTDIR="appdir-${TIMESTAMP}"

  mkdir ${OUTDIR}
  cd ${OUTDIR}

  cp -iv /nix/store/somePath/bin/cardano-rt-view-service .

  cp ../resources/cardano-rt-view.desktop .
  cp ../resources/cardano-rt-view.png .
  cp ../resources/rt-view.yaml0 .

  cp -rv ../static .

  cp ../resources/AppRun .
  chmod 700 AppRun

  cd ..

  ARCH=x86_64 ${RUNAPPIMG} -v `pwd`/${OUTDIR}/ /tmp/Cardano_RTview-x86_64.AppImage
''
