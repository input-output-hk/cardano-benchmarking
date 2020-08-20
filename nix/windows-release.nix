############################################################################
# Windows release cardano-rt-view-service-*.zip
#
# This bundles up the windows build and its dependencies,
# and sets up the Hydra build artifact.
#
############################################################################

{ pkgs
, project
, exes
}:

let
  lib = pkgs.lib;
  name = "cardano-rt-view-service-${project.version}-win64";

in pkgs.runCommand name {
    buildInputs = with pkgs.buildPackages; [
      zip
      haskellBuildUtils.package
    ];
  } ''
  mkdir -p $out release
  cd release

  cp -n --remove-destination -v ${pkgs.lib.concatMapStringsSep " " (exe: "${exe}/bin/*") exes} ./
  chmod -R +w .

  zip -r $out/${name}.zip .

  dist_file=$(ls $out)
  mkdir -p $out/nix-support
  echo "file binary-dist $out/$dist_file" > $out/nix-support/hydra-build-products
''
