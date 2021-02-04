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
    inherit sourcesOverride;
    config = { allowUnfree = false; inHydra = true; };
    gitrev = cardano-benchmarking.rev;
  }

# The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" ]

# The systems used for cross-compiling (default: linux)
, supportedCrossSystems ? [ (builtins.head supportedSystems) ]

# Build for linux
, linuxBuild ? builtins.elem "x86_64-linux" supportedSystems

# Build for macos
, macosBuild ? builtins.elem "x86_64-darwin" supportedSystems

# Cross compilation to Windows is currently only supported on linux.
, windowsBuild ? builtins.elem "x86_64-linux" supportedCrossSystems

# A Hydra option
, scrubJobs ? true

# Dependencies overrides
, sourcesOverride ? {}

# Import pkgs, including IOHK common nix lib
, pkgs ? import ./nix { inherit sourcesOverride; }

}:
with pkgs.lib;

let
  linuxRelease = (import pkgs.iohkNix.release-lib) {
    inherit pkgs;
    supportedSystems = [ "x86_64-linux" ];
    supportedCrossSystems = filter (s: s == "x86_64-linux" && windowsBuild) supportedCrossSystems;
    inherit scrubJobs projectArgs;
    packageSet = import cardano-benchmarking;
    gitrev = cardano-benchmarking.rev;
  };
  archs = filterAttrs (n: _: elem n supportedSystems) {
    x86_64-linux = linuxRelease.pkgsFor "x86_64-linux";
  };
  mkPins = inputs: pkgs.runCommand "ifd-pins" {} ''
    mkdir $out
    cd $out
    ${concatMapStringsSep "\n" (input: "ln -sv ${input.value} ${input.key}") (attrValues (mapAttrs (key: value: { inherit key value; }) inputs))}
  '';

  extraBuilds = {
  };

  # restrict supported systems to a subset where tests (if exist) are required to pass:
  testsSupportedSystems = intersectLists supportedSystems [ "x86_64-linux" ];
  # Recurse through an attrset, returning all derivations in a list matching test supported systems.
  collectJobs' = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);
  # Adds the package name to the derivations for windows-testing-bundle.nix
  # (passthru.identifier.name does not survive mapTestOn)
  collectJobs = ds: concatLists (
    mapAttrsToList (packageName: package:
      map (drv: drv // { inherit packageName; }) (collectJobs' package)
    ) ds);

  nonDefaultBuildSystems = tail supportedSystems;

  # Paths or prefixes of paths of derivations to build only on the default system (ie. linux on hydra):
  onlyBuildOnDefaultSystem = [
    ["checks"]
  ];
  # Paths or prefix of paths for which cross-builds (mingwW64, musl64) are disabled:
  noCrossBuild = [
    ["shell"] ["roots"]
  ] ++ onlyBuildOnDefaultSystem;
  noMusl64Build = [ ["checks"] ["tests"] ["benchmarks"] ["haskellPackages"] ["plan-nix"] ]
    ++ noCrossBuild;

  # Remove build jobs for which cross compiling does not make sense.
  filterProject = noBuildList: project: mapAttrsRecursiveCond (a: !(isDerivation a)) (path: value:
    if (isDerivation value && (any (p: take (length p) path == p) noBuildList)) then null
    else value
  ) project;

  inherit (pkgs.commonLib) sources nixpkgs;

  jobs = {
    ifd-pins = mkPins {
      inherit (sources) iohk-nix "haskell.nix";
      inherit nixpkgs;
      inherit (pkgs.haskell-nix) hackageSrc stackageSrc;
    };
  } // (optionalAttrs linuxBuild (with linuxRelease; {
    linux =
      let filteredBuilds = mapAttrsRecursiveCond (a: !(isList a)) (path: value:
        if (any (p: take (length p) path == p) onlyBuildOnDefaultSystem) then filter (s: !(elem s nonDefaultBuildSystems)) value else value)
        (packagePlatforms project);
      in {
        native = mapTestOn (__trace (__toJSON filteredBuilds) filteredBuilds);
      };
  })) // extraBuilds // (linuxRelease.mkRequiredJob (concatLists [
    # Linux builds:
    (optionals linuxBuild (concatLists [
      (collectJobs jobs.linux.native.checks)
      (collectJobs jobs.linux.native.benchmarks)
      (collectJobs jobs.linux.native.exes)
    ]))
  ]));

in jobs
