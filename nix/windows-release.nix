############################################################################
# Windows release cardano-rt-view-service-*.zip
#
# This bundles up the windows executable with its dependencies
# and static directory.
#
############################################################################

{ pkgs
, releaseVersion
, exes
, staticDir
}:

let
  lib = pkgs.lib;
  name = "cardano-rt-view-service-${releaseVersion}-win64";
  rtViewServiceExe = lib.head (lib.filter (exe: lib.hasInfix "cardano-rt-view-service" exe.name) exes);

in pkgs.runCommand name {
    buildInputs = with pkgs.buildPackages; [
      zip
      haskellBuildUtils.package
    ];
  } ''
  mkdir -p $out release
  cd release

  cp -n --remove-destination -v ${rtViewServiceExe}/bin/* ./

  mkdir ./static
  cp -R ${staticDir}/* ./static/

  chmod -R +w .

  zip -r $out/${name}.zip .
''
