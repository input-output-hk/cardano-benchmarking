############################################################################
# Darwin release cardano-rt-view-service-*.zip
#
# This bundles up the macOS executable with its dependencies
# and static directory. Result is *.zip file.
#
############################################################################

{ pkgs
, releaseVersion
, exes
, staticDir
}:

let
  lib = pkgs.lib;
  name = "cardano-rt-view-service-${releaseVersion}-darwin";
  rtViewServiceExe = lib.head (lib.filter (exe: lib.hasInfix "cardano-rt-view-service" exe.name) exes);

in pkgs.runCommand name {
    buildInputs = with pkgs.buildPackages; [
      haskellBuildUtils.package
      zip
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
