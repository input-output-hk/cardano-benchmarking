# our packages overlay
pkgs: _: with pkgs;
  let
    compiler = config.haskellNix.compiler or "ghc8102";
  in {
  cardanoBenchmarkingHaskellPackages = import ./haskell.nix {
    inherit compiler
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      cardanoNodeHaskellPackages
      cardanoNodeEventlogHaskellPackages
      ;
  };
}
