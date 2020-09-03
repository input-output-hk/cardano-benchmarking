############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

# The project sources
{ cardano-benchmarking ? { outPath = ./.; rev = "abcdef"; }

# Function arguments to pass to the project
, projectArgs ? {
    config = { allowUnfree = false; inHydra = true; };
    gitrev = cardano-benchmarking.rev;
  }

# The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]

# The systems used for cross-compiling
, supportedCrossSystems ? [ "x86_64-linux" ]

# A Hydra option
, scrubJobs ? true

# Dependencies overrides
, sourcesOverride ? {}

# Import pkgs, including IOHK common nix lib
, pkgs ? import ./nix { inherit sourcesOverride; }

# Release version of cardano-benchmarking, corresponds to GitHub tag, for example
# https://github.com/input-output-hk/cardano-benchmarking/releases/tag/1.15.0
, releaseVersion ? "1.0.0"

}:

with (import pkgs.iohkNix.release-lib) {
  inherit pkgs;
  inherit supportedSystems supportedCrossSystems scrubJobs projectArgs;
  packageSet = import cardano-benchmarking;
  gitrev = cardano-benchmarking.rev;
};

with pkgs.lib;

let

  testsSupportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
  # Recurse through an attrset, returning all test derivations in a list.
  collectTests' = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);
  # Adds the package name to the test derivations for windows-testing-bundle.nix
  # (passthru.identifier.name does not survive mapTestOn)
  collectTests = ds: concatLists (
    mapAttrsToList (packageName: package:
      map (drv: drv // { inherit packageName; }) (collectTests' package)
    ) ds);

  sources = import ./nix/sources.nix;

  inherit (systems.examples) mingwW64 musl64;

  # Recurse through an attrset, returning all derivations in a list matching test supported systems.
  collectJobs' = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);
  # Adds the package name to the derivations for windows-testing-bundle.nix
  # (passthru.identifier.name does not survive mapTestOn)
  collectJobs = ds: concatLists (
  mapAttrsToList (packageName: package:
    map (drv: drv // { inherit packageName; }) (collectJobs' package)
  ) ds);

  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
  nativePkgs = import nixpkgsSrc nixpkgsArgs;

  jobs = {
    native = mapTestOn (__trace (__toJSON (packagePlatforms project)) (packagePlatforms project));
    statically = nativePkgs.pkgsCross.musl64.pkgs.haskell-nix.cabalProject {
      src = ./.;
      compiler-nix-name = "ghc865";
      modules = [
        { packages.cardano-config.flags.systemd = false; }
      ];
    };
    "${mingwW64.config}" = mapTestOnCross mingwW64 (packagePlatformsCross (removeAttrs project [ "cardanoDbSyncHaskellPackages" "cardanoDbSync" ]));
    cardano-rt-view-service-win64-release = import ./nix/windows-release.nix {
      inherit pkgs releaseVersion;
      exes = collectJobs jobs.${mingwW64.config}.exes;
      staticDir = ./cardano-rt-view/static;
    };
    cardano-rt-view-service-linux-release = import ./nix/linux-release.nix {
      inherit pkgs releaseVersion;
      exes = collectJobs jobs.statically.cardano-rt-view.components.exes;
      staticDir = ./cardano-rt-view/static;
      resourcesDir = ./cardano-rt-view/resources;
    };
    cardano-rt-view-service-darwin-release = import ./nix/darwin-release.nix {
      inherit pkgs releaseVersion;
      exes = collectJobs jobs.native.cardano-rt-view-service.x86_64-darwin;
      staticDir = ./cardano-rt-view/static;
    };
  } // (mkRequiredJob (
      [
        jobs.native.cardano-tx-generator.x86_64-darwin
        jobs.native.cardano-tx-generator.x86_64-linux
        jobs.native.cardano-rt-view-service.x86_64-darwin
        jobs.native.cardano-rt-view-service.x86_64-linux
        jobs.cardano-rt-view-service-win64-release
        jobs.cardano-rt-view-service-linux-release
        jobs.cardano-rt-view-service-darwin-release
      ]));

in jobs
