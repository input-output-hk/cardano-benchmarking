############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, stdenv
, haskell-nix
, buildPackages
, config ? {}
# GHC attribute name
, compiler ? config.haskellNix.compiler or "ghc865"
# Enable profiling
, profiling ? config.haskellNix.profiling or true
}:
let

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject {
    src = haskell-nix.haskellLib.cleanGit {
      name = "cardano-benchmarking";
      src = ../.;
    };
    ghc = buildPackages.haskell-nix.compiler.${compiler};
    modules = [
      {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.doExactConfig = true;

        # split data output for ekg to reduce closure size
        packages.ekg.components.library.enableSeparateDataOutput = true;
        # TODO: Enable -Werror:
        packages.cardano-tx-generator.configureFlags = [
          "--ghc-option=-Wall"
          #"--ghc-option=-Werror"
        ];
        packages.cardano-rt-view.configureFlags = [
          "--ghc-option=-Wall"
          #"--ghc-option=-Werror"
        ];
        enableLibraryProfiling = profiling;
      }
      (lib.optionalAttrs profiling {
        enableLibraryProfiling = true;
        packages.tx-generator.components.exes.tx-generator.enableExecutableProfiling = true;
        profilingDetail = "default";
      })
    ];
  };
in
  pkgSet
