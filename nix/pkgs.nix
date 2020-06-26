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
  cardanoBenchmarkingProfiledHaskellPackages = import ./haskell.nix {
    inherit
      lib
      stdenv
      haskell-nix
      buildPackages
    ;
    config = config // { profiling = true; };
    profiling = true;
  };
}
