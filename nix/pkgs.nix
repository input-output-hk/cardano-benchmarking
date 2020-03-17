# our packages overlay
pkgs: _: with pkgs; {
  cardanoBenchmarkingHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
  };
}
