# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
{ config ? {}
, sourcesOverride ? {}
, minimal ? false
, withHoogle ? (! minimal)
, withUpstreamDeps ? true
, pkgs ? import ./nix {
    inherit config sourcesOverride;
  }
}:
with pkgs;
let
  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = cardanoBenchmarkingHaskellPackages.shellFor {
    name = "cabal-dev-shell";

    packages = _:
      with haskellPackages;
      lib.attrValues cardanoBenchmarkingHaskellPackages.projectPackages
      ++ lib.optionals withUpstreamDeps
        (with cardanoNodeHaskellPackages; [
          cardano-api
          cardano-config
          cardano-cli
          cardano-node
          ouroboros-consensus
          ouroboros-consensus-byron
          ouroboros-consensus-cardano
          ouroboros-consensus-shelley
        ]);

    # These programs will be available inside the nix-shell.
    buildInputs = with haskellPackages; [
      cabal-install
      stack
      stylish-haskell
      nix
      niv
      pkgconfig
      tmux
      # postgresql
    ] ++ lib.optionals (! minimal) [
      # cardanoDbSync.cardano-db-sync
      cardano-cli
      cardano-node
      ghcid
      git
      hlint
      stylish-haskell
      sqlite-interactive
      weeder
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;

    inherit withHoogle;

    shellHook = ''
      echo "Modifying cabal.project to allow Cabal pick up Nix-provided dependencies:"

      ./scripts/cabal-inside-nix-shell.sh
    '';
  };

in

 shell
