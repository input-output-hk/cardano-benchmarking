let
  # Fetch the latest haskell.nix and import its default.nix
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/a2bc3db.tar.gz) {};
  # haskell.nix provides access to the nixpkgs pins which are used by our CI, hence
  # you will be more likely to get cache hits when using these.
  # But you can also just use your own, e.g. '<nixpkgs>'
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;
  # haskell.nix provides some arguments to be passed to nixpkgs, including some patches
  # and also the haskell.nix functionality itself as an overlay.
  nixpkgsArgs = haskellNix.nixpkgsArgs;
in
{ nativePkgs ? import nixpkgsSrc nixpkgsArgs
, haskellCompiler ? "ghc865"
, cardano-benchmarking-src ? ./.
}:
nativePkgs.pkgsCross.musl64.pkgs.haskell-nix.cabalProject {
  src = cardano-benchmarking-src;
  modules = [
    { packages.cardano-config.flags.systemd = false; }
  ];
}
