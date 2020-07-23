# our packages overlay
pkgs: _: with pkgs; {
  cardanoBenchmarkingHaskellPackages = import ./haskell.nix {
    inherit config
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
  };
}
