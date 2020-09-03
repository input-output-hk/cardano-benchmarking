############################################################################
# Linux release cardano-rt-view-service-*.AppImage
#
# This bundles up the linux executable with its dependencies
# and static directory. Result is *.AppImage file.
#
############################################################################

{ pkgs
, releaseVersion
, exes
, staticDir
, resourcesDir
}:

let
  lib = pkgs.lib;
  name = "cardano-rt-view-service-${releaseVersion}-linux-x86_64";
  rtViewServiceExe = lib.head (lib.filter (exe: lib.hasInfix "cardano-rt-view-service" exe.name) exes);
  appimagetool = pkgs.fetchurl {
    url = https://github.com/AppImage/AppImageKit/releases/download/12/appimagetool-x86_64.AppImage;
    sha256 = "04ws94q71bwskmhizhwmaf41ma4wabvfgjgkagr8wf3vakgv866r";
  };

in pkgs.runCommand name {
    buildInputs = with pkgs.buildPackages; [
      appimage-run
      file
      haskellBuildUtils.package
    ];
  } ''
  mkdir -p $out release
  cd release

  cp -n --remove-destination -v ${rtViewServiceExe}/bin/* ./

  mkdir ./static
  cp -R ${staticDir}/* ./static/

  cp ${resourcesDir}/* ./
  chmod 700 ./AppRun

  cd ..

  # Copy appimagetool to /tmp, to make it executable before calling.
  cp ${appimagetool} /tmp/appimagetool-x86_64.AppImage
  chmod +x /tmp/appimagetool-x86_64.AppImage

  ARCH=x86_64 appimage-run /tmp/appimagetool-x86_64.AppImage -v `pwd`/release/ $out/${name}.AppImage
''
