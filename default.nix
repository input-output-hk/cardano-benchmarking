{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# override scripts with custom configuration
, customConfig ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and iohk-nix:
# nix build -f default.nix cardano-tx-generator --arg sourcesOverride '{
#   iohk-nix = ../iohk-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (iohk-nix and our packages).
, pkgs ? import ./nix/default.nix { inherit system crossSystem config sourcesOverride gitrev; }
# Git sha1 hash, to be passed when not building from a git work tree.
, gitrev ? null
}:
with pkgs; with commonLib;
let

  haskellPackages = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages cardanoBenchmarkingHaskellPackages);

  scripts = callPackage ./nix/scripts.nix { inherit customConfig; };

  rewrite-static = _: p: p;

  packages = {
    inherit haskellPackages cardano-tx-generator locli;

    # so that eval time gc roots are cached (nix-tools stuff)
    inherit (cardanoBenchmarkingHaskellPackages) roots;

    inherit (haskellPackages.cardano-tx-generator.identifier) version;

    inherit (haskellPackages.cardano-tx-generator.project) plan-nix;

    exes = mapAttrsRecursiveCond (as: !(isDerivation as)) rewrite-static (collectComponents' "exes" haskellPackages);

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;

      # hlint = callPackage iohkNix.tests.hlint {
      #   src = ./. ;
      # };
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };
  };
in packages
