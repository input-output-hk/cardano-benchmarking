# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
{ config ? {}
, customConfig ? {}
, sourcesOverride ? {}
, withHoogle ? true
, pkgs ? import ./nix {
    inherit config sourcesOverride;
}
# Benchmarking specifics:
, withConsensusDeps ? true
, withMonitoringDeps ? true
, withNodeDeps ? withConsensusDeps || withMonitoringDeps
}:
with pkgs;
let
  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  # NOTE: due to some cabal limitation,
  #  you have to remove all `source-repository-package` entries from cabal.project
  #  after entering nix-shell for cabal to use nix provided dependencies for them.
  shell = cardanoBenchmarkingHaskellPackages.shellFor {
    name = "cabal-dev-shell";

    # packages = ps: lib.attrValues (haskell-nix.haskellLib.selectProjectPackages ps);
    packages = ps:
      with haskellPackages;
      lib.attrValues (haskell-nix.haskellLib.selectProjectPackages ps)
      ++ lib.optionals withConsensusDeps
        (with cardanoNodeHaskellPackages; [
          ouroboros-consensus
          ouroboros-consensus-byron
          ouroboros-consensus-cardano
          ouroboros-consensus-shelley
        ])
      ++ lib.optionals withMonitoringDeps
        (with cardanoNodeHaskellPackages; [
          iohk-monitoring
          lobemo-backend-aggregation
          lobemo-backend-ekg
          lobemo-backend-monitoring
          lobemo-backend-trace-forwarder
        ])
      ++ lib.optionals withNodeDeps
        (with cardanoNodeHaskellPackages; [
          cardano-api
          cardano-config
          cardano-cli
          cardano-node
        ]);

    # These programs will be available inside the nix-shell.
    buildInputs = with haskellPackages; [
      cabal-install
      ghc-prof-flamegraph
      ghcid
      pkgs.git
      pkgs.gnuplot
      hlint
      niv
      nix
      pkgconfig
      profiterole
      profiteur
      sqlite-interactive
      stylish-haskell
      tmux
      weeder
      # from devops-shell:
      python3Packages.supervisor
      python3Packages.ipython
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;

    inherit withHoogle;
  };

in

 shell
