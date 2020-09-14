{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and iohk-nix:
# nix build -f default.nix cardano-node --arg sourcesOverride '{
#   iohk-nix = ../iohk-nix;
# }'
, sourcesOverride ? {}
# override scripts with custom configuration
, customConfig ? {}
# pinned version of nixpkgs augmented with overlays (iohk-nix and our packages).
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride customConfig; }
, gitrev ? pkgs.iohkNix.commitIdFromGitRepoOrZero ./.git
}:
with pkgs; with commonLib;
let

  haskellPackages = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (cardanoBenchmarkingHaskellPackages.projectPackages);

  self = {
    inherit haskellPackages cardanoNode pkgs;

    # Grab the executable component of our package.
    inherit (haskellPackages.cardano-tx-generator.components.exes)
      cardano-tx-generator;
    inherit (haskellPackages.cardano-rt-view.components.exes)
      cardano-rt-view-service;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    exes = collectComponents' "exes" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;
    };

    scripts = (callPackage (import ./nix/svclib.nix) {}).scripts;

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = false;
    };
  };
in self
