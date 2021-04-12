# our packages overlay
pkgs: _: with pkgs;
  let
    compiler = config.haskellNix.compiler or "ghc8104";
  in {
  cardanoBenchmarkingHaskellPackages = import ./haskell.nix {
    inherit compiler
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
  };

  #Grab the executable component of our package.
  inherit (cardanoBenchmarkingHaskellPackages.cardano-tx-generator.components.exes) cardano-tx-generator;
  inherit (cardanoBenchmarkingHaskellPackages.locli.components.exes) locli;
}
